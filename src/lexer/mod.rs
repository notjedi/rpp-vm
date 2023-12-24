mod tokens;
pub(crate) use tokens::{KeyWord, Literal, Span, Token, TokenKind};

use std::{collections::VecDeque, i64, iter::Peekable, str::Chars};
use thiserror::Error;

#[derive(Debug, Error)]
pub(crate) enum LexError {
    #[error("unexpected character {0}")]
    UnexpectedChar(char),
    // TODO: `expected` can be multiple characters
    #[error("expected: {expected}, found: {found}")]
    MissingExpectedChar { expected: char, found: char },
}

pub(crate) struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    row: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            row: 0,
            col: 0,
        }
    }

    pub(crate) fn tokensize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.advance_token()?;
            if token.kind == TokenKind::Eof {
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
        let ch = self.input.next();
        if let Some(ch) = ch {
            if ch == '\n' {
                self.row += 1;
                self.col = 0
            } else {
                self.col += 1;
            }
        }
        ch
    }

    #[inline]
    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    #[inline]
    fn take_while(&mut self, predicate: impl Fn(&char) -> bool) -> String {
        let mut string = String::with_capacity(64);
        while let Some(ch) = self.peek() {
            if predicate(&ch) {
                self.consume();
                string.push(ch);
            } else {
                break;
            }
        }
        string
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        self.take_while(|&ch| ch.is_ascii_whitespace());
    }

    fn eat_number(&mut self) -> TokenKind {
        let mut has_dot = false;
        let mut num_str = String::with_capacity(64);

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
            num_str.push(ch);
        }

        // SAFETY: it's safe to unwrap here because we've made sure that the array only has digits and at max of one `.`
        if has_dot {
            let num = num_str.parse::<f64>().unwrap();
            TokenKind::Literal(tokens::Literal::Float(num))
        } else {
            let num = num_str.parse::<i64>().unwrap();
            TokenKind::Literal(tokens::Literal::Int(num))
        }
    }

    fn eat_literal_str(&mut self) -> Result<String, LexError> {
        // NOTE: does not support `"` inside the string
        self.consume();
        let line = self.take_while(|&ch| ch != '"');
        // NOTE: next char will be either '"' or Token::Eof
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
        let ident = self.take_while(|&ch| ch.is_ascii_alphabetic() || ch == '_');
        if let Some(' ') = self.peek() {
            self.consume();
        }
        ident
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
        one_char_kind: TokenKind,
        double_char_kind: TokenKind,
    ) -> TokenKind {
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
        token: TokenKind,
    ) -> Result<TokenKind, LexError> {
        if let Some(found) = self.peek() {
            if found == expected {
                self.consume();
                return Ok(token);
            }
            return Err(LexError::MissingExpectedChar { expected, found });
        }
        Err(LexError::UnexpectedChar(char::default()))
    }

    fn eat_punctuation(&mut self) -> Result<TokenKind, LexError> {
        let ch = self.consume().unwrap();

        let token = match ch {
            '+' => KeyWord::Sum.into(),
            '-' => KeyWord::Sub.into(),
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
                        TokenKind::Comment(self.eat_line())
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

    fn match_potential_single_word_kw(&mut self) -> Option<KeyWord> {
        let kw = match self.peek_n_words(1).as_str() {
            const { KeyWord::Print.as_str() } => KeyWord::Print.into(),
            const { KeyWord::EndFunc.as_str() } => KeyWord::EndFunc.into(),
            const { KeyWord::ForStart.as_str() } => KeyWord::ForStart.into(),
            const { KeyWord::ProgramEnd.as_str() } => KeyWord::ProgramEnd.into(),
            _ => None,
        };
        kw.map(|kw| {
            self.eat_ident();
            kw
        })
    }

    fn match_potential_double_word_kw(&mut self) -> Option<KeyWord> {
        let kw = match self.peek_n_words(2).as_str() {
            const { KeyWord::Assign.as_str() } => KeyWord::Assign.into(),
            const { KeyWord::Declare.as_str() } => KeyWord::Declare.into(),
            const { KeyWord::EndBlock.as_str() } => KeyWord::EndBlock.into(),
            const { KeyWord::FuncCall.as_str() } => KeyWord::FuncCall.into(),
            const { KeyWord::BreakLoop.as_str() } => KeyWord::BreakLoop.into(),
            const { KeyWord::ProgramStart.as_str() } => KeyWord::ProgramStart.into(),
            const { KeyWord::StartDeclare.as_str() } => KeyWord::StartDeclare.into(),
            const { KeyWord::ForRangeStart.as_str() } => KeyWord::ForRangeStart.into(),
            _ => None,
        };
        kw.map(|kw| {
            self.eat_n_idents(2);
            kw
        })
    }

    fn match_potential_triple_word_kw(&mut self) -> Option<KeyWord> {
        let kw = match self.peek_n_words(3).as_str() {
            const { KeyWord::IfCond.as_str() } => KeyWord::IfCond.into(),
            const { KeyWord::WhileLoop.as_str() } => KeyWord::WhileLoop.into(),
            const { KeyWord::FuncReturn.as_str() } => KeyWord::FuncReturn.into(),
            const { KeyWord::ForRangeEnd.as_str() } => KeyWord::ForRangeEnd.into(),
            _ => None,
        };
        kw.map(|kw| {
            self.eat_n_idents(3);
            kw
        })
    }

    fn match_potential_four_word_kw(&mut self) -> Option<KeyWord> {
        let kw = match self.peek_n_words(4).as_str() {
            const { KeyWord::ElseCond.as_str() } => KeyWord::ElseCond.into(),
            const { KeyWord::FuncDeclare.as_str() } => KeyWord::FuncDeclare.into(),
            _ => None,
        };
        kw.map(|kw| {
            self.eat_n_idents(4);
            kw
        })
    }

    fn match_keyword(&mut self) -> TokenKind {
        // NOTE: there is a difference b/w `or` and `or_else`, use only `or_else`.
        // with `or` you are supposed to give it an Option which gets evaluated before anything even begins.
        // but with `or_else`, it's a function which will be called only when the result is None.
        let kw = self
            .match_potential_four_word_kw()
            .or_else(|| self.match_potential_triple_word_kw())
            .or_else(|| self.match_potential_double_word_kw())
            .or_else(|| self.match_potential_single_word_kw());
        kw.map_or_else(|| TokenKind::Ident(self.eat_ident()), TokenKind::KeyWord)
    }

    pub(crate) fn advance_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();
        let (start_row, start_col) = (self.row, self.col);
        let ch = match self.peek() {
            Some(ch) => ch,
            None => return Ok(Token::default()),
        };

        let kind = match ch {
            '"' => TokenKind::Literal(Literal::Str(self.eat_literal_str()?)),
            '\'' => TokenKind::Literal(Literal::Char(self.eat_literal_char()?)),
            '0'..='9' => self.eat_number(),
            'a'..='z' | 'A'..='Z' | '_' => match self.peek_n_words(1).as_str() {
                "True" => {
                    self.eat_n_idents(1);
                    TokenKind::Literal(Literal::BoolTrue)
                }
                "False" => {
                    self.eat_n_idents(1);
                    TokenKind::Literal(Literal::BoolFalse)
                }
                _ => self.match_keyword(),
            },
            _ => self.eat_punctuation()?,
        };

        let token = Token {
            kind,
            span: Span {
                row: start_row,
                col: start_col,
                length: self.col - start_col,
            },
        };
        Ok(token)
    }

    // https://github.com/tjdevries/vim9jit/blob/9a530e1f0f346f86784eef8ff7026849b1b9ed64/crates/vim9-lexer/src/lib.rs#L1039
    pub(crate) fn snapshot_lexing(input: &'a str) -> Result<String, LexError> {
        let mut lexer = Self::new(input);
        let tokens = lexer.tokensize()?;
        let mut tokens = VecDeque::from(tokens);

        let mut output = String::new();
        for (row, line) in input.lines().enumerate() {
            output += line;
            output += "\n";

            while let Some(tok) = tokens.pop_front() {
                if tok.span.row != row {
                    tokens.push_front(tok);
                    break;
                }

                output += &" ".repeat(tok.span.col);
                output += &"^".repeat(tok.span.length);
                output += &format!(" {tok:?}");
                output += "\n"
            }
        }
        Ok(output)
    }
}

#[cfg(test)]
mod tests {

    use super::{LexError, Lexer};

    #[test]
    fn test_fizz_buzz() -> Result<(), LexError> {
        let program = include_str!("../../testdata/snapshots/test.rpp");
        let snapshot_string = Lexer::snapshot_lexing(program)?;

        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_path("../../testdata/output/");
        settings.bind(|| {
            insta::assert_snapshot!(snapshot_string);
        });
        Ok(())
    }
}
