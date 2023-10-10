mod tokens;
pub use tokens::Token;

use anyhow::{bail, Result};
use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub struct OutofInputError;
impl Display for OutofInputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ran out of input")
    }
}
impl Error for OutofInputError {}

pub struct Lexer {
    input: Vec<char>,
    read_pos: usize, // moving reading position
    pos: usize,      // reading position
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input: input.chars().collect(),
            read_pos: 0,
            pos: 0,
            ch: 0 as char,
        };
        let _ = lexer.read_char();
        lexer
    }

    fn read_char(&mut self) -> Result<()> {
        if self.read_pos < self.input.len() {
            self.ch = self.input[self.read_pos];
            self.pos = self.read_pos;
            self.read_pos += 1;
            return Ok(());
        }
        bail!(OutofInputError)
        // Err(OutofInputError).into()
    }

    fn skip_whitespace(&mut self) -> Result<()> {
        while self.ch == ' ' || self.ch == '\n' {
            self.read_char()?;
        }
        Ok(())
    }

    fn read_ident(&mut self) -> Result<String> {
        let pos = self.pos;
        while self.ch.is_ascii_alphabetic() || self.ch == '_' {
            self.read_char()?;
        }

        Ok(String::from_utf8_lossy(
            &self.input[pos..self.pos]
                .iter()
                .map(|c| *c as u8)
                .collect::<Vec<u8>>(),
        )
        .to_string())
    }

    fn read_int(&mut self) -> Result<String> {
        let pos = self.pos;
        while self.ch.is_ascii_digit() {
            self.read_char()?;
        }

        Ok(String::from_utf8_lossy(
            &self.input[pos..self.pos]
                .iter()
                .map(|c| *c as u8)
                .collect::<Vec<u8>>(),
        )
        .to_string())
    }

    fn read_line(&mut self) -> Result<String> {
        let pos = self.pos;
        while self.ch != '\n' {
            self.read_char()?;
        }

        Ok(String::from_utf8_lossy(
            &self.input[pos..self.pos]
                .iter()
                .map(|c| *c as u8)
                .collect::<Vec<u8>>(),
        )
        .to_string())
    }

    fn read_literal(&mut self) -> Result<String> {
        let pos = self.pos;
        self.read_char()?;
        while self.ch != '"' {
            self.read_char()?;
        }
        self.read_char()?;

        Ok(String::from_utf8_lossy(
            &self.input[pos..self.pos]
                .iter()
                .map(|c| *c as u8)
                .collect::<Vec<u8>>(),
        )
        .to_string())
    }

    fn peek_next_word(&mut self) -> Result<String> {
        let read_pos = self.read_pos;
        let pos = self.pos;
        let ch = self.ch;

        let _ = self.skip_whitespace();
        let next_word = self.read_ident();

        self.ch = ch;
        self.pos = pos;
        self.read_pos = read_pos;
        next_word
    }

    fn skip_next_word(&mut self) {
        let _ = self.skip_whitespace();
        let _ = self.read_ident();
    }

    pub fn peek(&mut self) -> char {
        if self.read_pos >= self.input.len() {
            0 as char
        } else {
            self.input[self.read_pos]
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace()?;
        if self.read_pos >= self.input.len() {
            bail!(OutofInputError);
        }

        let token = match self.ch {
            '+' => Token::Sum,
            '-' => Token::Sub,
            '*' => Token::Mul,
            '/' => Token::Div,
            '%' => Token::Mod,

            ';' => Token::SemiColon,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,

            '<' => {
                if self.peek() == '=' {
                    self.read_char()?;
                    Token::LessThanEqual
                } else {
                    Token::LessThan
                }
            }

            '>' => {
                if self.peek() == '=' {
                    self.read_char()?;
                    Token::GreaterThanEqual
                } else {
                    Token::GreaterThan
                }
            }

            '!' => {
                let next_char = self.peek();
                if next_char == '!' {
                    return Ok(Token::Comment(self.read_line()?));
                } else if next_char == '=' {
                    self.read_char()?;
                    Token::NotEqual
                } else {
                    unreachable!()
                }
            }

            '=' => {
                if self.peek() == '=' {
                    self.read_char()?;
                    Token::Equal
                } else {
                    unreachable!()
                }
            }

            ':' => {
                if self.peek() == '=' {
                    self.read_char()?;
                    Token::DeclareAlt
                } else {
                    unreachable!()
                }
            }

            '"' => return Ok(Token::Literal(self.read_literal()?)),

            'a'..='z' | 'A'..='Z' | '_' => {
                let tok = match self.read_ident()?.as_str() {
                    "MARAKKADHINGA" => Token::EndFunc,
                    "MAGIZHCHI" => Token::ProgramEnd,
                    "NAA" => Token::ForStart,
                    "DOT" => Token::Print,

                    "true" => Token::BoolTrue,
                    "false" => Token::BoolFalse,

                    "LAKSHMI" => {
                        if self.peek_next_word().is_ok_and(|x| x == "START") {
                            self.skip_next_word();
                            Token::ProgramStart
                        } else {
                            Token::Illegal
                        }
                    }
                    "AANDAVAN" => {
                        if self.peek_next_word().is_ok_and(|x| x == "SOLLRAN") {
                            self.skip_next_word();
                            Token::StartDeclare
                        } else {
                            Token::Illegal
                        }
                    }
                    "ARUNACHALAM" => {
                        if self.peek_next_word().is_ok_and(|x| x == "SEIYARAN") {
                            self.skip_next_word();
                            Token::Declare
                        } else {
                            Token::Illegal
                        }
                    }
                    "BHAJJI" => {
                        if self.peek_next_word().is_ok_and(|x| x == "SAAPDU") {
                            self.skip_next_word();
                            Token::Assign
                        } else {
                            Token::Illegal
                        }
                    }
                    "THADAVA" => {
                        if self.peek_next_word().is_ok_and(|x| x == "SONNA") {
                            self.skip_next_word();
                            if self.peek_next_word().is_ok_and(|x| x == "MADHRI") {
                                self.skip_next_word();
                                Token::ForRangeEnd
                            } else {
                                Token::ForRangeStart
                            }
                        } else {
                            Token::Illegal
                        }
                    }
                    "KATHAM" => {
                        if self.peek_next_word().is_ok_and(|x| x == "KATHAM") {
                            self.skip_next_word();
                            Token::EndBlock
                        } else {
                            Token::Illegal
                        }
                    }
                    "BLACK" => {
                        if self.peek_next_word().is_ok_and(|x| x == "SHEEP") {
                            self.skip_next_word();
                            Token::BreakLoop
                        } else {
                            Token::Illegal
                        }
                    }
                    "CHUMMA" => {
                        if self.peek_next_word().is_ok_and(|x| x == "ADHURUDHULA") {
                            self.skip_next_word();
                            Token::FuncCall
                        } else {
                            Token::Illegal
                        }
                    }

                    // i'm crying inside after writing this
                    "EN" => {
                        if self.peek_next_word().is_ok_and(|x| x == "PEAR") {
                            self.skip_next_word();
                            if self.peek_next_word().is_ok_and(|x| x == "MANICKAM") {
                                self.skip_next_word();
                                Token::IfCond
                            } else {
                                Token::Illegal
                            }
                        } else if self.peek_next_word().is_ok_and(|x| x == "VAZHI") {
                            self.skip_next_word();
                            if self.peek_next_word().is_ok_and(|x| x == "THANI") {
                                self.skip_next_word();
                                if self.peek_next_word().is_ok_and(|x| x == "VAZHI") {
                                    self.skip_next_word();
                                    Token::FuncDeclare
                                } else {
                                    Token::Illegal
                                }
                            } else {
                                Token::Illegal
                            }
                        } else {
                            Token::Illegal
                        }
                    }

                    // i'm crying inside after writing this
                    "BABA" => {
                        if self.peek_next_word().is_ok_and(|x| x == "COUNTING") {
                            self.skip_next_word();
                            if self.peek_next_word().is_ok_and(|x| x == "STARTS") {
                                self.skip_next_word();
                                Token::WhileLoop
                            } else {
                                Token::Illegal
                            }
                        } else {
                            Token::Illegal
                        }
                    }

                    // i'm crying inside after writing this
                    "IDHU" => {
                        if self.peek_next_word().is_ok_and(|x| x == "EPDI") {
                            self.skip_next_word();
                            if self.peek_next_word().is_ok_and(|x| x == "IRUKKU") {
                                self.skip_next_word();
                                Token::FuncReturn
                            } else {
                                Token::Illegal
                            }
                        } else {
                            Token::Illegal
                        }
                    }

                    // i'm crying inside after writing this
                    "ENAKKU" => {
                        if self.peek_next_word().is_ok_and(|x| x == "INNURU") {
                            self.skip_next_word();
                            if self.peek_next_word().is_ok_and(|x| x == "PEAR") {
                                self.skip_next_word();
                                if self.peek_next_word().is_ok_and(|x| x == "IRUKKU") {
                                    self.skip_next_word();
                                    Token::ElseCond
                                } else {
                                    Token::Illegal
                                }
                            } else {
                                Token::Illegal
                            }
                        } else {
                            Token::Illegal
                        }
                    }

                    ident => Token::Ident(ident.to_string()),
                };
                return Ok(tok);
            }

            '0'..='9' => return Ok(Token::Number(self.read_int()?)),
            _ => unreachable!(),
        };

        let _ = self.read_char();
        Ok(token)
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use super::Token::*;
    use anyhow::Result;

    #[test]
    fn test_fizz_buzz() -> Result<()> {
        let program = r#"
            LAKSHMI START

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
            ProgramStart,
            Comment(String::from("!! declare variables")),
            StartDeclare,
            Ident(String::from("ix")),
            Declare,
            Number(String::from("1")),
            SemiColon,
            StartDeclare,
            Ident(String::from("range")),
            Declare,
            Number(String::from("16")),
            SemiColon,
            ForStart,
            Number(String::from("1")),
            ForRangeStart,
            Ident(String::from("range")),
            ForRangeEnd,
            LeftBrace,
            IfCond,
            Ident(String::from("ix")),
            Mod,
            Number(String::from("15")),
            Equal,
            Number(String::from("0")),
            LeftBrace,
            Print,
            Literal(String::from(r#""FizzBuzz""#)),
            SemiColon,
            RightBrace,
            ElseCond,
            LeftBrace,
            IfCond,
            Ident(String::from("ix")),
            Mod,
            Number(String::from("3")),
            Equal,
            Number(String::from("0")),
            LeftBrace,
            Print,
            Literal(String::from(r#""Fizz""#)),
            SemiColon,
            RightBrace,
            ElseCond,
            LeftBrace,
            IfCond,
            Ident(String::from("ix")),
            Mod,
            Number(String::from("5")),
            Equal,
            Number(String::from("0")),
            LeftBrace,
            Print,
            Literal(String::from(r#""Buzz""#)),
            SemiColon,
            RightBrace,
            ElseCond,
            LeftBrace,
            Print,
            Ident(String::from("ix")),
            SemiColon,
            RightBrace,
            EndBlock,
            SemiColon,
            RightBrace,
            EndBlock,
            SemiColon,
            RightBrace,
            EndBlock,
            SemiColon,
            Ident(String::from("ix")),
            Assign,
            Ident(String::from("ix")),
            Sum,
            Number(String::from("1")),
            SemiColon,
            Comment(String::from("!! End Loop")),
            RightBrace,
            EndBlock,
            SemiColon,
            ProgramEnd,
        ];

        let mut lexer = Lexer::new(program.to_string());
        for token in tokens {
            let lex_token = lexer.next_token()?;
            assert_eq!(token, lex_token);
        }
        assert!(lexer.next_token().is_err());
        Ok(())
    }
}
