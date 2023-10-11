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

#[derive(Debug)]
enum Number {
    Int(i64),
    Float(f64),
}

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
    }

    fn skip_whitespace(&mut self) -> Result<()> {
        while self.ch.is_ascii_whitespace() {
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

    fn read_number(&mut self) -> Result<Number> {
        let pos = self.pos;
        let mut has_dot = false;
        while self.ch.is_ascii_digit() || self.ch == '-' || (self.ch == '.' && !has_dot) {
            // if self.ch == '.' {
            //     has_dot = true;
            // }
            has_dot = has_dot || self.ch == '.';
            self.read_char()?;
        }

        if has_dot {
            Ok(Number::Float(
                String::from_utf8_lossy(
                    &self.input[pos..self.pos]
                        .iter()
                        .map(|c| *c as u8)
                        .collect::<Vec<u8>>(),
                )
                .parse::<f64>()?,
            ))
        } else {
            Ok(Number::Int(
                String::from_utf8_lossy(
                    &self.input[pos..self.pos]
                        .iter()
                        .map(|c| *c as u8)
                        .collect::<Vec<u8>>(),
                )
                .parse::<i64>()?,
            ))
        }
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
        self.read_char()?;
        let pos = self.pos;
        while self.ch != '"' {
            self.read_char()?;
        }
        let end_pos = self.pos;
        self.read_char()?;

        Ok(String::from_utf8_lossy(
            &self.input[pos..end_pos]
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

        let token = match self.ch {
            '+' => Token::Sum,
            '-' => {
                if self.peek().is_ascii_digit() {
                    match self.read_number()? {
                        Number::Int(int_num) => return Ok(Token::Number(int_num)),
                        Number::Float(float_num) => return Ok(Token::Float(float_num)),
                    }
                } else {
                    Token::Sub
                }
            }
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

            '0'..='9' => match self.read_number()? {
                Number::Int(int_num) => return Ok(Token::Number(int_num)),
                Number::Float(float_num) => return Ok(Token::Float(float_num)),
            },
            ch => unreachable!("got character: {}", ch),
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
            Number(1),
            SemiColon,
            StartDeclare,
            Ident(String::from("range")),
            Declare,
            Number(16),
            SemiColon,
            ForStart,
            Number(1),
            ForRangeStart,
            Ident(String::from("range")),
            ForRangeEnd,
            LeftBrace,
            IfCond,
            Ident(String::from("ix")),
            Mod,
            Number(15),
            Equal,
            Number(0),
            LeftBrace,
            Print,
            Literal(String::from("FizzBuzz")),
            SemiColon,
            RightBrace,
            ElseCond,
            LeftBrace,
            IfCond,
            Ident(String::from("ix")),
            Mod,
            Number(3),
            Equal,
            Number(0),
            LeftBrace,
            Print,
            Literal(String::from("Fizz")),
            SemiColon,
            RightBrace,
            ElseCond,
            LeftBrace,
            IfCond,
            Ident(String::from("ix")),
            Mod,
            Number(5),
            Equal,
            Number(0),
            LeftBrace,
            Print,
            Literal(String::from("Buzz")),
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
            Number(1),
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

    #[test]
    fn test_math_ops() -> Result<()> {
        let program = r#"
            LAKSHMI START
            AANDAVAN SOLLRAN addvar ARUNACHALAM SEIYARAN 25 + 15;
            AANDAVAN SOLLRAN subvar ARUNACHALAM SEIYARAN 25 - 15;
            AANDAVAN SOLLRAN mulvar ARUNACHALAM SEIYARAN 5 * 5;
            AANDAVAN SOLLRAN divvar ARUNACHALAM SEIYARAN 5 / 5;
            AANDAVAN SOLLRAN modvar ARUNACHALAM SEIYARAN 51 % 5;
            DOT "addvar = " addvar;
            DOT "subvar = " subvar;
            DOT "mulvar = " mulvar;
            DOT "divvar = " divvar;
            DOT "modvar = " modvar;
            MAGIZHCHI
        "#;

        let tokens = vec![
            ProgramStart,
            StartDeclare,
            Ident(String::from("addvar")),
            Declare,
            Number(25),
            Sum,
            Number(15),
            SemiColon,
            StartDeclare,
            Ident(String::from("subvar")),
            Declare,
            Number(25),
            Sub,
            Number(15),
            SemiColon,
            StartDeclare,
            Ident(String::from("mulvar")),
            Declare,
            Number(5),
            Mul,
            Number(5),
            SemiColon,
            StartDeclare,
            Ident(String::from("divvar")),
            Declare,
            Number(5),
            Div,
            Number(5),
            SemiColon,
            StartDeclare,
            Ident(String::from("modvar")),
            Declare,
            Number(51),
            Mod,
            Number(5),
            SemiColon,
            Print,
            Literal(String::from("addvar = ")),
            Ident(String::from("addvar")),
            SemiColon,
            Print,
            Literal(String::from("subvar = ")),
            Ident(String::from("subvar")),
            SemiColon,
            Print,
            Literal(String::from("mulvar = ")),
            Ident(String::from("mulvar")),
            SemiColon,
            Print,
            Literal(String::from("divvar = ")),
            Ident(String::from("divvar")),
            SemiColon,
            Print,
            Literal(String::from("modvar = ")),
            Ident(String::from("modvar")),
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

    #[test]
    fn test_while_loop() -> Result<()> {
        let program = r#"
            LAKSHMI START
            DOT "While Loop Example";
            AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 1;
            BABA COUNTING STARTS True{
                DOT ix;
                ix BHAJJI SAAPDU ix + 1;
                EN PEAR MANICKAM ix > 5{
                    DOT "breaking out of loop...";
                    BLACK SHEEP;
                }KATHAM KATHAM;
            }KATHAM KATHAM;
            MAGIZHCHI
        "#;

        let tokens = vec![
            ProgramStart,
            Print,
            Literal(String::from("While Loop Example")),
            SemiColon,
            StartDeclare,
            Ident(String::from("ix")),
            Declare,
            Number(1),
            SemiColon,
            WhileLoop,
            Ident(String::from("True")),
            LeftBrace,
            Print,
            Ident(String::from("ix")),
            SemiColon,
            Ident(String::from("ix")),
            Assign,
            Ident(String::from("ix")),
            Sum,
            Number(1),
            SemiColon,
            IfCond,
            Ident(String::from("ix")),
            GreaterThan,
            Number(5),
            LeftBrace,
            Print,
            Literal(String::from("breaking out of loop...")),
            SemiColon,
            BreakLoop,
            SemiColon,
            RightBrace,
            EndBlock,
            SemiColon,
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

    #[test]
    fn test_logical_ops() -> Result<()> {
        let program = r#"
            LAKSHMI START
            AANDAVAN SOLLRAN x ARUNACHALAM SEIYARAN 5.5;
            AANDAVAN SOLLRAN y ARUNACHALAM SEIYARAN 15;
            AANDAVAN SOLLRAN a ARUNACHALAM SEIYARAN x;
            AANDAVAN SOLLRAN b ARUNACHALAM SEIYARAN y;
            DOT "This is Rajini++. Every command is a famous dialogues of Rajinikanth!";
            DOT "Addition Output:" x "+" y "=" x+y;
            DOT "Assigning new value to y. Setting y = 100";
            y BHAJJI SAAPDU 100;
            DOT "New y = " y;
            AANDAVAN SOLLRAN boolvar ARUNACHALAM SEIYARAN True;
            DOT "boolvar = " boolvar;
            DOT "x: " x "y: " y;
            DOT "a: " a "b: " b;
            DOT "x > y: " x > y;
            DOT "x >= y: " x >= y;
            DOT "x < y: " x < y;
            DOT "x <= y: " x <= y;
            DOT "x == a: " x == a;
            DOT "x != b: " x != b;
            MAGIZHCHI
        "#;
        let tokens = vec![
            ProgramStart,
            StartDeclare,
            Ident(String::from("x")),
            Declare,
            Float(5.5),
            SemiColon,
            StartDeclare,
            Ident(String::from("y")),
            Declare,
            Number(15),
            SemiColon,
            StartDeclare,
            Ident(String::from("a")),
            Declare,
            Ident(String::from("x")),
            SemiColon,
            StartDeclare,
            Ident(String::from("b")),
            Declare,
            Ident(String::from("y")),
            SemiColon,
            Print,
            Literal(String::from(
                "This is Rajini++. Every command is a famous dialogues of Rajinikanth!",
            )),
            SemiColon,
            Print,
            Literal(String::from("Addition Output:")),
            Ident(String::from("x")),
            Literal(String::from("+")),
            Ident(String::from("y")),
            Literal(String::from("=")),
            Ident(String::from("x")),
            Sum,
            Ident(String::from("y")),
            SemiColon,
            Print,
            Literal(String::from("Assigning new value to y. Setting y = 100")),
            SemiColon,
            Ident(String::from("y")),
            Assign,
            Number(100),
            SemiColon,
            Print,
            Literal(String::from("New y = ")),
            Ident(String::from("y")),
            SemiColon,
            StartDeclare,
            Ident(String::from("boolvar")),
            Declare,
            Ident(String::from("True")),
            SemiColon,
            Print,
            Literal(String::from("boolvar = ")),
            Ident(String::from("boolvar")),
            SemiColon,
            Print,
            Literal(String::from("x: ")),
            Ident(String::from("x")),
            Literal(String::from("y: ")),
            Ident(String::from("y")),
            SemiColon,
            Print,
            Literal(String::from("a: ")),
            Ident(String::from("a")),
            Literal(String::from("b: ")),
            Ident(String::from("b")),
            SemiColon,
            Print,
            Literal(String::from("x > y: ")),
            Ident(String::from("x")),
            GreaterThan,
            Ident(String::from("y")),
            SemiColon,
            Print,
            Literal(String::from("x >= y: ")),
            Ident(String::from("x")),
            GreaterThanEqual,
            Ident(String::from("y")),
            SemiColon,
            Print,
            Literal(String::from("x < y: ")),
            Ident(String::from("x")),
            LessThan,
            Ident(String::from("y")),
            SemiColon,
            Print,
            Literal(String::from("x <= y: ")),
            Ident(String::from("x")),
            LessThanEqual,
            Ident(String::from("y")),
            SemiColon,
            Print,
            Literal(String::from("x == a: ")),
            Ident(String::from("x")),
            Equal,
            Ident(String::from("a")),
            SemiColon,
            Print,
            Literal(String::from("x != b: ")),
            Ident(String::from("x")),
            NotEqual,
            Ident(String::from("b")),
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
