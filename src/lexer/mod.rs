mod tokens;
pub(crate) use tokens::{KeyWord, Token};

use std::{i64, iter::Peekable, str::Chars};
use thiserror::Error;

#[derive(Debug, Error)]
#[allow(dead_code)]
pub(crate) enum LexError {
    #[error("unexpected character {0}")]
    UnexpectedChar(char),
    #[error("expected: {expected:?}, found: {found:?}")]
    MissingExpectedSymbol { expected: Token, found: Token },
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

    fn consume(&mut self) -> Option<char> {
        self.input.next()
    }

    pub(crate) fn is_eof(&mut self) -> bool {
        self.peek().is_none()
    }

    pub(crate) fn skip_whitespace(&mut self) {
        while let Some(ch) = self.consume() {
            if !ch.is_ascii_whitespace() {
                break;
            }
        }
    }

    #[inline]
    pub(crate) fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    pub(crate) fn eat_number(&mut self) -> Token {
        // TODO: mare clean way to do this?
        let mut len = 0;
        let mut has_dot = false;
        let mut num_chars = [0 as char; 128];

        if let Some('-') = self.peek() {
            num_chars[len] = '-';
            len += 1;
            self.consume();
        }

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

        let num_str = num_chars[..len].into_iter().collect::<String>();
        // SAFETY: it's safe to unwrap here because we've made sure that the array only has digits and at max of one `.`
        if has_dot {
            let num = num_str.parse::<f64>().unwrap();
            Token::Literal(tokens::Literal::Float(num))
        } else {
            let num = num_str.parse::<i64>().unwrap();
            Token::Literal(tokens::Literal::Int(num))
        }
    }

    pub(crate) fn advance_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();

        let ch = match self.consume() {
            Some(ch) => ch,
            None => return Ok(Token::Eof),
        };

        let token = match ch {
            '+' => Token::KeyWord(KeyWord::Sum),
            '-' => match self.peek().unwrap_or(0 as char).is_ascii_digit() {
                true => self.eat_number(),
                false => Token::KeyWord(KeyWord::Sub),
            },
            '*' => Token::KeyWord(KeyWord::Mul),
            '/' => Token::KeyWord(KeyWord::Div),
            '%' => Token::KeyWord(KeyWord::Mod),

            ';' => Token::KeyWord(KeyWord::SemiColon),
            '{' => Token::KeyWord(KeyWord::LeftBrace),
            '}' => Token::KeyWord(KeyWord::RightBrace),

            _ => todo!(),
        };

        Ok(token)
    }
}
