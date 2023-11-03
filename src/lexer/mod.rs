mod tokens;
use itertools::Itertools;
pub(crate) use tokens::{KeyWord, Literal, Token};

use std::{i64, iter::Peekable, str::Chars};
use thiserror::Error;

#[derive(Debug, Error)]
#[allow(dead_code)]
pub(crate) enum LexError {
    #[error("unexpected character {0}")]
    UnexpectedChar(char),
    // TODO: `expected` can be multiple characters
    #[error("expected: {expected}, found: {found}")]
    MissingExpectedChar { expected: char, found: char },
}

pub(crate) struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
        }
    }

    pub(crate) fn tokensize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.advance_token()?;
            if token == Token::Eof {
                break;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }

    pub(crate) fn tokenize_str(program: &str) -> Result<Vec<Token>, LexError> {
        let mut lexer = Lexer::new(program);
        lexer.tokensize()
    }

    #[inline]
    fn consume(&mut self) -> Option<char> {
        self.input.next()
    }

    #[inline]
    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    #[inline]
    fn take_while(&mut self, predicate: impl Fn(&char) -> bool) -> String {
        self.input.peeking_take_while(predicate).collect::<String>()
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        self.take_while(|&ch| ch.is_ascii_whitespace());
    }

    fn eat_number(&mut self) -> Token {
        // TODO: mare clean way to do this?
        let mut len = 0;
        let mut has_dot = false;
        let mut num_chars = [0 as char; 128];

        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                self.consume();
            } else if ch == '.' && !has_dot {
                self.consume();
                has_dot = true;
            } else if ch == '_' {
                // NOTE: _ b/w numbers is not in the grammar afaik but we'll add it
                continue;
            } else {
                break;
            }
            num_chars[len] = ch;
            len += 1;
        }

        let num_str = num_chars[..len].iter().collect::<String>();
        // SAFETY: it's safe to unwrap here because we've made sure that the array only has digits and at max of one `.`
        if has_dot {
            let num = num_str.parse::<f64>().unwrap();
            Token::Literal(tokens::Literal::Float(num))
        } else {
            let num = num_str.parse::<i64>().unwrap();
            Token::Literal(tokens::Literal::Int(num))
        }
    }

    fn eat_literal_str(&mut self) -> Result<String, LexError> {
        // NOTE: does not support `"` inside the string
        self.consume();
        let line = self.take_while(|&ch| ch != '"');
        match self.peek() {
            Some('"') => {
                self.consume();
                Ok(line)
            }
            ch => Err(LexError::MissingExpectedChar {
                expected: '"',
                found: ch.unwrap_or_default(),
            }),
        }
    }

    fn eat_literal_char(&mut self) -> Result<char, LexError> {
        self.consume();
        let ch = self
            .consume()
            .ok_or(LexError::UnexpectedChar(char::default()));
        match self.peek() {
            Some('\'') => {
                self.consume();
                ch
            }
            peek_ch => Err(LexError::MissingExpectedChar {
                expected: '\'',
                found: peek_ch.unwrap_or_default(),
            }),
        }
    }

    #[inline]
    fn eat_line(&mut self) -> String {
        self.take_while(|&ch| ch != '\n')
    }

    #[inline]
    fn eat_ident(&mut self) -> String {
        // NOTE: do not use self.take_while here cause we want to skip the whitespace after reading a word
        self.input
            .by_ref()
            .take_while(|&ch| ch.is_ascii_alphabetic() || ch == '_')
            .collect::<String>()
    }

    #[inline]
    fn eat_n_idents(&mut self, n: u32) {
        (0..n).for_each(|_| {
            self.eat_ident();
        })
    }

    fn eat_potential_double_char_op(
        &mut self,
        expected: char,
        one_char_kind: Token,
        double_char_kind: Token,
    ) -> Token {
        if let Some(next) = self.peek() {
            if next == expected {
                self.consume();
                return double_char_kind;
            }
            return one_char_kind;
        }
        one_char_kind
    }

    fn eat_potential_double_char_or_err(
        &mut self,
        expected: char,
        token: Token,
    ) -> Result<Token, LexError> {
        if let Some(found) = self.peek() {
            if found == expected {
                self.consume();
                return Ok(token);
            }
            return Err(LexError::MissingExpectedChar { expected, found });
        }
        Err(LexError::UnexpectedChar(char::default()))
    }

    fn eat_punctuation(&mut self) -> Result<Token, LexError> {
        let ch = self.consume().unwrap();

        let token = match ch {
            '+' => KeyWord::Sum.into(),
            '-' => match self.peek() {
                Some('0'..='9') => match self.eat_number() {
                    Token::Literal(Literal::Float(float_num)) => {
                        Token::Literal(Literal::Float(-float_num))
                    }
                    Token::Literal(Literal::Int(int_num)) => Token::Literal(Literal::Int(-int_num)),
                    _ => unreachable!(),
                },
                _ => KeyWord::Sub.into(),
            },
            '*' => KeyWord::Mul.into(),
            '/' => KeyWord::Div.into(),
            '%' => KeyWord::Mod.into(),

            ';' => KeyWord::SemiColon.into(),
            '{' => KeyWord::LeftBrace.into(),
            '}' => KeyWord::RightBrace.into(),

            '<' => self.eat_potential_double_char_op(
                '=',
                KeyWord::LessThan.into(),
                KeyWord::LessThanEqual.into(),
            ),
            '>' => self.eat_potential_double_char_op(
                '=',
                KeyWord::GreaterThan.into(),
                KeyWord::GreaterThanEqual.into(),
            ),

            '=' => self.eat_potential_double_char_or_err('=', KeyWord::Equal.into())?,
            ':' => self.eat_potential_double_char_or_err('=', KeyWord::DeclareAlt.into())?,
            '!' => match self.eat_potential_double_char_or_err('=', KeyWord::NotEqual.into()) {
                Ok(token) => token,
                _ => match self.peek() {
                    Some('!') => {
                        self.consume();
                        Token::Comment(self.eat_line())
                    }
                    ch => return Err(LexError::UnexpectedChar(ch.unwrap_or_default())),
                },
            },

            ch => return Err(LexError::UnexpectedChar(ch)),
        };
        Ok(token)
    }

    fn peek_n_words(&self, n: u32) -> String {
        let mut num_spaces = 0;
        self.input
            .clone()
            .take_while(|&ch| {
                if ch == ' ' {
                    num_spaces += 1;
                }
                (ch.is_ascii_alphabetic() || ch == '_' || ch == ' ') && num_spaces <= (n - 1)
            })
            .collect::<String>()
    }

    fn match_potential_single_word_kw(&self) -> Option<KeyWord> {
        match self.peek_n_words(1).as_str() {
            const { KeyWord::Print.as_str() } => KeyWord::Print.into(),
            const { KeyWord::EndFunc.as_str() } => KeyWord::EndFunc.into(),
            const { KeyWord::ForStart.as_str() } => KeyWord::ForStart.into(),
            const { KeyWord::BoolTrue.as_str() } => KeyWord::BoolTrue.into(),
            const { KeyWord::BoolFalse.as_str() } => KeyWord::BoolFalse.into(),
            const { KeyWord::ProgramEnd.as_str() } => KeyWord::ProgramEnd.into(),
            _ => None,
        }
    }

    fn match_potential_double_word_kw(&self) -> Option<KeyWord> {
        match self.peek_n_words(2).as_str() {
            const { KeyWord::Assign.as_str() } => KeyWord::Assign.into(),
            const { KeyWord::Declare.as_str() } => KeyWord::Declare.into(),
            const { KeyWord::EndBlock.as_str() } => KeyWord::EndBlock.into(),
            const { KeyWord::FuncCall.as_str() } => KeyWord::FuncCall.into(),
            const { KeyWord::BreakLoop.as_str() } => KeyWord::BreakLoop.into(),
            const { KeyWord::ProgramStart.as_str() } => KeyWord::ProgramStart.into(),
            const { KeyWord::StartDeclare.as_str() } => KeyWord::StartDeclare.into(),
            const { KeyWord::ForRangeStart.as_str() } => KeyWord::ForRangeStart.into(),
            _ => None,
        }
    }

    fn match_potential_triple_word_kw(&self) -> Option<KeyWord> {
        match self.peek_n_words(3).as_str() {
            const { KeyWord::IfCond.as_str() } => KeyWord::IfCond.into(),
            const { KeyWord::WhileLoop.as_str() } => KeyWord::WhileLoop.into(),
            const { KeyWord::FuncReturn.as_str() } => KeyWord::FuncReturn.into(),
            const { KeyWord::ForRangeEnd.as_str() } => KeyWord::ForRangeEnd.into(),
            _ => None,
        }
    }

    fn match_potential_four_word_kw(&self) -> Option<KeyWord> {
        match self.peek_n_words(4).as_str() {
            const { KeyWord::ElseCond.as_str() } => KeyWord::ElseCond.into(),
            const { KeyWord::FuncDeclare.as_str() } => KeyWord::FuncDeclare.into(),
            _ => None,
        }
    }

    fn match_keyword(&mut self) -> Token {
        let kw = None
            .or_else(|| {
                self.match_potential_four_word_kw().and_then(|kw| {
                    self.eat_n_idents(4);
                    Some(kw)
                })
            })
            .or_else(|| {
                self.match_potential_triple_word_kw().and_then(|kw| {
                    self.eat_n_idents(3);
                    Some(kw)
                })
            })
            .or_else(|| {
                self.match_potential_double_word_kw().and_then(|kw| {
                    self.eat_n_idents(2);
                    Some(kw)
                })
            })
            .or_else(|| {
                self.match_potential_single_word_kw().and_then(|kw| {
                    self.eat_ident();
                    Some(kw)
                })
            });
        kw.map_or_else(|| Token::Ident(self.eat_ident()), |kw| Token::KeyWord(kw))
    }

    pub(crate) fn advance_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();

        let ch = match self.peek() {
            Some(ch) => ch,
            None => return Ok(Token::Eof),
        };

        let token = match ch {
            '"' => Token::Literal(Literal::Str(self.eat_literal_str()?)),
            '\'' => Token::Literal(Literal::Char(self.eat_literal_char()?)),
            '0'..='9' => self.eat_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.match_keyword(),
            _ => self.eat_punctuation()?,
        };

        Ok(token)
    }
}

#[cfg(test)]
mod tests {
    use super::{KeyWord::*, Lexer, Literal::*, Token::*};

    #[test]
    fn test_fizz_buzz() {
        let program = r#"
            EN VAZHI THANI VAZHI myfunc_one
                DOT "Hello from myfunc_one!";
                AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 100;
                DOT "returning ix =" ix "to main";
                IDHU EPDI IRUKKU ix;
            MARAKKADHINGA

            LAKSHMI START
                !! checking exprs
                AANDAVAN SOLLRAN addvar ARUNACHALAM SEIYARAN 25 + 15;
                AANDAVAN SOLLRAN subvar ARUNACHALAM SEIYARAN 25 - 15;
                AANDAVAN SOLLRAN mulvar ARUNACHALAM SEIYARAN 5 * 5;
                AANDAVAN SOLLRAN divvar ARUNACHALAM SEIYARAN 5 / 5;
                AANDAVAN SOLLRAN modvar ARUNACHALAM SEIYARAN 51 % 5;

                !! testing literals
                AANDAVAN SOLLRAN x ARUNACHALAM SEIYARAN 5.5;
                AANDAVAN SOLLRAN y ARUNACHALAM SEIYARAN 15;
                AANDAVAN SOLLRAN a ARUNACHALAM SEIYARAN x;
                AANDAVAN SOLLRAN b ARUNACHALAM SEIYARAN y;

                !! testing while loop
                BABA COUNTING STARTS True{
                    DOT ix;
                    ix BHAJJI SAAPDU ix + 1;
                    EN PEAR MANICKAM ix > 5{
                        DOT "breaking out of loop...";
                        BLACK SHEEP;
                    }KATHAM KATHAM;
                }KATHAM KATHAM;

                y CHUMMA ADHURUDHULA myfunc_one;

                !! declare variables
                AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 1;
                AANDAVAN SOLLRAN range ARUNACHALAM SEIYARAN 16;

                NAA 1 THADAVA SONNA range THADAVA SONNA MADHRI{
                    EN PEAR MANICKAM ix%15==0{
                        DOT "FizzBuzz";
                    } ENAKKU INNURU PEAR IRUKKU{
                        EN PEAR MANICKAM ix%3==0{
                            DOT "Fizz";
                        } ENAKKU INNURU PEAR IRUKKU{
                            EN PEAR MANICKAM ix%5==0{
                                DOT "Buzz";
                            } ENAKKU INNURU PEAR IRUKKU{
                                DOT ix;
                            }KATHAM KATHAM;
                        }KATHAM KATHAM;
                    }KATHAM KATHAM;
                    ix BHAJJI SAAPDU ix + 1;
                !! End Loop
                }KATHAM KATHAM;
            MAGIZHCHI
        "#;

        let tokens = vec![
            KeyWord(FuncDeclare),
            Ident("myfunc_one".to_string()),
            KeyWord(Print),
            Literal(Str("Hello from myfunc_one!".to_string())),
            KeyWord(SemiColon),
            KeyWord(StartDeclare),
            Ident("ix".to_string()),
            KeyWord(Declare),
            Literal(Int(100)),
            KeyWord(SemiColon),
            KeyWord(Print),
            Literal(Str("returning ix =".to_string())),
            Ident("ix".to_string()),
            Literal(Str("to main".to_string())),
            KeyWord(SemiColon),
            KeyWord(FuncReturn),
            Ident("ix".to_string()),
            KeyWord(EndFunc),
            KeyWord(ProgramStart),
            Comment(" checking exprs".to_string()),
            KeyWord(StartDeclare),
            Ident("addvar".to_string()),
            KeyWord(Declare),
            Literal(Int(25)),
            KeyWord(Sum),
            Literal(Int(15)),
            KeyWord(SemiColon),
            KeyWord(StartDeclare),
            Ident("subvar".to_string()),
            KeyWord(Declare),
            Literal(Int(25)),
            KeyWord(Sub),
            Literal(Int(15)),
            KeyWord(SemiColon),
            KeyWord(StartDeclare),
            Ident("mulvar".to_string()),
            KeyWord(Declare),
            Literal(Int(5)),
            KeyWord(Mul),
            Literal(Int(5)),
            KeyWord(SemiColon),
            KeyWord(StartDeclare),
            Ident("divvar".to_string()),
            KeyWord(Declare),
            Literal(Int(5)),
            KeyWord(Div),
            Literal(Int(5)),
            KeyWord(SemiColon),
            KeyWord(StartDeclare),
            Ident("modvar".to_string()),
            KeyWord(Declare),
            Literal(Int(51)),
            KeyWord(Mod),
            Literal(Int(5)),
            KeyWord(SemiColon),
            Comment(" testing literals".to_string()),
            KeyWord(StartDeclare),
            Ident("x".to_string()),
            KeyWord(Declare),
            Literal(Float(5.5)),
            KeyWord(SemiColon),
            KeyWord(StartDeclare),
            Ident("y".to_string()),
            KeyWord(Declare),
            Literal(Int(15)),
            KeyWord(SemiColon),
            KeyWord(StartDeclare),
            Ident("a".to_string()),
            KeyWord(Declare),
            Ident("x".to_string()),
            KeyWord(StartDeclare),
            Ident("b".to_string()),
            KeyWord(Declare),
            Ident("y".to_string()),
            Comment(" testing while loop".to_string()),
            KeyWord(WhileLoop),
            KeyWord(BoolTrue),
            KeyWord(Print),
            Ident("ix".to_string()),
            Ident("ix".to_string()),
            KeyWord(Assign),
            Ident("ix".to_string()),
            KeyWord(Sum),
            Literal(Int(1)),
            KeyWord(SemiColon),
            KeyWord(IfCond),
            Ident("ix".to_string()),
            KeyWord(GreaterThan),
            Literal(Int(5)),
            KeyWord(LeftBrace),
            KeyWord(Print),
            Literal(Str("breaking out of loop...".to_string())),
            KeyWord(SemiColon),
            KeyWord(BreakLoop),
            KeyWord(RightBrace),
            KeyWord(EndBlock),
            KeyWord(RightBrace),
            KeyWord(EndBlock),
            Ident("y".to_string()),
            KeyWord(FuncCall),
            Ident("myfunc_one".to_string()),
            Comment(" declare variables".to_string()),
            KeyWord(StartDeclare),
            Ident("ix".to_string()),
            KeyWord(Declare),
            Literal(Int(1)),
            KeyWord(SemiColon),
            KeyWord(StartDeclare),
            Ident("range".to_string()),
            KeyWord(Declare),
            Literal(Int(16)),
            KeyWord(SemiColon),
            KeyWord(ForStart),
            Literal(Int(1)),
            KeyWord(ForRangeStart),
            Ident("range".to_string()),
            KeyWord(ForRangeEnd),
            KeyWord(IfCond),
            Ident("ix".to_string()),
            Literal(Int(15)),
            KeyWord(Equal),
            Literal(Int(0)),
            KeyWord(LeftBrace),
            KeyWord(Print),
            Literal(Str("FizzBuzz".to_string())),
            KeyWord(SemiColon),
            KeyWord(RightBrace),
            KeyWord(ElseCond),
            KeyWord(IfCond),
            Ident("ix".to_string()),
            Literal(Int(3)),
            KeyWord(Equal),
            Literal(Int(0)),
            KeyWord(LeftBrace),
            KeyWord(Print),
            Literal(Str("Fizz".to_string())),
            KeyWord(SemiColon),
            KeyWord(RightBrace),
            KeyWord(ElseCond),
            KeyWord(IfCond),
            Ident("ix".to_string()),
            Literal(Int(5)),
            KeyWord(Equal),
            Literal(Int(0)),
            KeyWord(LeftBrace),
            KeyWord(Print),
            Literal(Str("Buzz".to_string())),
            KeyWord(SemiColon),
            KeyWord(RightBrace),
            KeyWord(ElseCond),
            KeyWord(Print),
            Ident("ix".to_string()),
            KeyWord(RightBrace),
            KeyWord(EndBlock),
            KeyWord(RightBrace),
            KeyWord(EndBlock),
            KeyWord(RightBrace),
            KeyWord(EndBlock),
            Ident("ix".to_string()),
            KeyWord(Assign),
            Ident("ix".to_string()),
            KeyWord(Sum),
            Literal(Int(1)),
            KeyWord(SemiColon),
            Comment(" End Loop".to_string()),
            KeyWord(RightBrace),
            KeyWord(EndBlock),
            KeyWord(ProgramEnd),
        ];

        let mut lexer = Lexer::new(program);
        for token in tokens {
            let lex_token = lexer.advance_token();
            assert_eq!(token, lex_token.unwrap());
        }
    }
}
