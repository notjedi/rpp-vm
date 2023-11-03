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

    pub(crate) fn tokenize(program: &str) -> Result<Vec<Token>, LexError> {
        let mut lexer = Lexer::new(program);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.advance_token()?;
            if token == Token::Eof {
                break;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }

    fn consume(&mut self) -> Option<char> {
        self.input.next()
    }

    #[inline]
    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    #[inline]
    fn take_while(&mut self, predicate: impl Fn(&char) -> bool) -> String {
        self.input.take_while_ref(predicate).collect::<String>()
    }

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

    fn eat_line(&mut self) -> String {
        self.take_while(|&ch| ch == '\n')
    }

    fn eat_ident(&mut self) -> String {
        self.take_while(|&ch| ch.is_ascii_alphabetic() || ch == '_')
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

    fn peek_n_words(&mut self, n: u32) -> String {
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

    fn match_potential_single_word_kw(&mut self) -> Option<KeyWord> {
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

    fn match_potential_double_word_kw(&mut self) -> Option<KeyWord> {
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

    fn match_potential_triple_word_kw(&mut self) -> Option<KeyWord> {
        match self.peek_n_words(3).as_str() {
            const { KeyWord::IfCond.as_str() } => KeyWord::IfCond.into(),
            const { KeyWord::WhileLoop.as_str() } => KeyWord::WhileLoop.into(),
            const { KeyWord::FuncReturn.as_str() } => KeyWord::FuncReturn.into(),
            const { KeyWord::ForRangeEnd.as_str() } => KeyWord::ForRangeEnd.into(),
            _ => None,
        }
    }

    fn match_potential_four_word_kw(&mut self) -> Option<KeyWord> {
        match self.peek_n_words(4).as_str() {
            const { KeyWord::ElseCond.as_str() } => KeyWord::ElseCond.into(),
            const { KeyWord::FuncDeclare.as_str() } => KeyWord::FuncDeclare.into(),
            _ => None,
        }
    }

    fn match_keyword(&mut self) -> Option<Token> {
        let kw = self
            .match_potential_four_word_kw()
            .or(self.match_potential_triple_word_kw())
            .or(self.match_potential_double_word_kw())
            .or(self.match_potential_single_word_kw());
        kw.map_or_else(|| None, |kw| Some(Token::KeyWord(kw)))
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
            'a'..='z' | 'A'..='Z' | '_' => {
                let token = self.match_keyword();
                let mut call_n_times = |n: u32| {
                    (0..n).for_each(|_| {
                        self.eat_ident();
                    })
                };
                if let Some(Token::KeyWord(ref kw)) = token {
                    match kw {
                        KeyWord::ElseCond | KeyWord::FuncDeclare => call_n_times(4),

                        KeyWord::IfCond
                        | KeyWord::WhileLoop
                        | KeyWord::FuncReturn
                        | KeyWord::ForRangeEnd => call_n_times(3),

                        KeyWord::Assign
                        | KeyWord::Declare
                        | KeyWord::EndBlock
                        | KeyWord::FuncCall
                        | KeyWord::BreakLoop
                        | KeyWord::ProgramStart
                        | KeyWord::StartDeclare
                        | KeyWord::ForRangeStart => call_n_times(2),

                        KeyWord::Print
                        | KeyWord::EndFunc
                        | KeyWord::ForStart
                        | KeyWord::BoolTrue
                        | KeyWord::BoolFalse
                        | KeyWord::ProgramEnd => call_n_times(1),

                        _ => unreachable!(),
                    }
                }
                token.map_or_else(|| Token::Ident(self.eat_ident()), |token| token)
            }

            _ => self.eat_punctuation()?,
        };

        Ok(token)
    }
}
