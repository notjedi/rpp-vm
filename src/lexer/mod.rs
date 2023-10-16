mod tokens;
pub use tokens::Token;

use anyhow::{bail, Result};
use std::{
    error::Error,
    fmt::{Debug, Display},
};

#[derive(Debug)]
pub enum LexError {
    OutofInputError,
    UnexpectedChar(char),
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OutofInputError => write!(f, "OutofInputError"),
            Self::UnexpectedChar(ch) => write!(f, "UnexpectedChar({})", ch),
        }
    }
}

impl Error for LexError {}

#[derive(Debug)]
enum Number {
    Int(i64),
    Float(f64),
}

pub struct Lexer<'a> {
    input: &'a [u8],
    input_str: &'a str,
    read_pos: usize, // moving reading position
    pos: usize,      // reading position
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input_str: input,
            input: input.as_bytes(),
            pos: 0,
            read_pos: 0,
            ch: 0 as char,
        };
        let _ = lexer.read_char();
        lexer
    }

    fn read_char(&mut self) -> Result<()> {
        if self.read_pos < self.input.len() {
            self.ch = self.input[self.read_pos] as char;
            self.pos = self.read_pos;
            self.read_pos += 1;
            return Ok(());
        }
        bail!(LexError::OutofInputError)
    }

    fn skip_whitespace(&mut self) -> Result<()> {
        while self.ch.is_ascii_whitespace() {
            self.read_char()?;
        }
        Ok(())
    }

    fn read_ident(&mut self) -> Result<&'a str> {
        let pos = self.pos;
        while self.ch.is_ascii_alphabetic() || self.ch == '_' {
            self.read_char()?;
        }

        Ok(&self.input_str[pos..self.pos])
    }

    fn read_number(&mut self) -> Result<Number> {
        let pos = self.pos;
        let mut has_dot = false;
        while self.ch.is_ascii_digit() || self.ch == '-' || (self.ch == '.' && !has_dot) {
            if self.ch == '.' {
                has_dot = true;
            }
            // has_dot = has_dot || self.ch == '.';
            self.read_char()?;
        }

        if has_dot {
            Ok(Number::Float(self.input_str[pos..self.pos].parse::<f64>()?))
        } else {
            Ok(Number::Int(self.input_str[pos..self.pos].parse::<i64>()?))
        }
    }

    fn read_line(&mut self) -> Result<&'a str> {
        let pos = self.pos;
        while self.ch != '\n' {
            self.read_char()?;
        }

        Ok(&self.input_str[pos..self.pos])
    }

    fn read_literal(&mut self) -> Result<&'a str> {
        self.read_char()?;
        let pos = self.pos;
        while self.ch != '"' {
            self.read_char()?;
        }
        let end_pos = self.pos;
        self.read_char()?;

        Ok(&self.input_str[pos..end_pos])
    }

    fn peek_next_word(&mut self) -> Result<&'a str> {
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
            self.input[self.read_pos] as char
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
                    bail!(LexError::UnexpectedChar(next_char))
                }
            }

            '=' => {
                if self.peek() == '=' {
                    self.read_char()?;
                    Token::Equal
                } else {
                    bail!(LexError::UnexpectedChar(self.peek()))
                }
            }

            ':' => {
                if self.peek() == '=' {
                    self.read_char()?;
                    Token::DeclareAlt
                } else {
                    bail!(LexError::UnexpectedChar(self.peek()))
                }
            }

            '"' => return Ok(Token::Literal(self.read_literal()?)),

            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.read_ident()?;
                let tok = match ident {
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

                    ident => Token::Ident(ident),
                };
                return Ok(tok);
            }

            '0'..='9' => match self.read_number()? {
                Number::Int(int_num) => return Ok(Token::Number(int_num)),
                Number::Float(float_num) => return Ok(Token::Float(float_num)),
            },
            ch => bail!(LexError::UnexpectedChar(ch)),
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
            Comment("!! declare variables"),
            StartDeclare,
            Ident("ix"),
            Declare,
            Number(1),
            SemiColon,
            StartDeclare,
            Ident("range"),
            Declare,
            Number(16),
            SemiColon,
            ForStart,
            Number(1),
            ForRangeStart,
            Ident("range"),
            ForRangeEnd,
            LeftBrace,
            IfCond,
            Ident("ix"),
            Mod,
            Number(15),
            Equal,
            Number(0),
            LeftBrace,
            Print,
            Literal("FizzBuzz"),
            SemiColon,
            RightBrace,
            ElseCond,
            LeftBrace,
            IfCond,
            Ident("ix"),
            Mod,
            Number(3),
            Equal,
            Number(0),
            LeftBrace,
            Print,
            Literal("Fizz"),
            SemiColon,
            RightBrace,
            ElseCond,
            LeftBrace,
            IfCond,
            Ident("ix"),
            Mod,
            Number(5),
            Equal,
            Number(0),
            LeftBrace,
            Print,
            Literal("Buzz"),
            SemiColon,
            RightBrace,
            ElseCond,
            LeftBrace,
            Print,
            Ident("ix"),
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
            Ident("ix"),
            Assign,
            Ident("ix"),
            Sum,
            Number(1),
            SemiColon,
            Comment("!! End Loop"),
            RightBrace,
            EndBlock,
            SemiColon,
            ProgramEnd,
        ];

        let mut lexer = Lexer::new(program);
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
            Ident("addvar"),
            Declare,
            Number(25),
            Sum,
            Number(15),
            SemiColon,
            StartDeclare,
            Ident("subvar"),
            Declare,
            Number(25),
            Sub,
            Number(15),
            SemiColon,
            StartDeclare,
            Ident("mulvar"),
            Declare,
            Number(5),
            Mul,
            Number(5),
            SemiColon,
            StartDeclare,
            Ident("divvar"),
            Declare,
            Number(5),
            Div,
            Number(5),
            SemiColon,
            StartDeclare,
            Ident("modvar"),
            Declare,
            Number(51),
            Mod,
            Number(5),
            SemiColon,
            Print,
            Literal("addvar = "),
            Ident("addvar"),
            SemiColon,
            Print,
            Literal("subvar = "),
            Ident("subvar"),
            SemiColon,
            Print,
            Literal("mulvar = "),
            Ident("mulvar"),
            SemiColon,
            Print,
            Literal("divvar = "),
            Ident("divvar"),
            SemiColon,
            Print,
            Literal("modvar = "),
            Ident("modvar"),
            SemiColon,
            ProgramEnd,
        ];

        let mut lexer = Lexer::new(program);
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
            Literal("While Loop Example"),
            SemiColon,
            StartDeclare,
            Ident("ix"),
            Declare,
            Number(1),
            SemiColon,
            WhileLoop,
            Ident("True"),
            LeftBrace,
            Print,
            Ident("ix"),
            SemiColon,
            Ident("ix"),
            Assign,
            Ident("ix"),
            Sum,
            Number(1),
            SemiColon,
            IfCond,
            Ident("ix"),
            GreaterThan,
            Number(5),
            LeftBrace,
            Print,
            Literal("breaking out of loop..."),
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

        let mut lexer = Lexer::new(program);
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
            Ident("x"),
            Declare,
            Float(5.5),
            SemiColon,
            StartDeclare,
            Ident("y"),
            Declare,
            Number(15),
            SemiColon,
            StartDeclare,
            Ident("a"),
            Declare,
            Ident("x"),
            SemiColon,
            StartDeclare,
            Ident("b"),
            Declare,
            Ident("y"),
            SemiColon,
            Print,
            Literal("This is Rajini++. Every command is a famous dialogues of Rajinikanth!"),
            SemiColon,
            Print,
            Literal("Addition Output:"),
            Ident("x"),
            Literal("+"),
            Ident("y"),
            Literal("="),
            Ident("x"),
            Sum,
            Ident("y"),
            SemiColon,
            Print,
            Literal("Assigning new value to y. Setting y = 100"),
            SemiColon,
            Ident("y"),
            Assign,
            Number(100),
            SemiColon,
            Print,
            Literal("New y = "),
            Ident("y"),
            SemiColon,
            StartDeclare,
            Ident("boolvar"),
            Declare,
            Ident("True"),
            SemiColon,
            Print,
            Literal("boolvar = "),
            Ident("boolvar"),
            SemiColon,
            Print,
            Literal("x: "),
            Ident("x"),
            Literal("y: "),
            Ident("y"),
            SemiColon,
            Print,
            Literal("a: "),
            Ident("a"),
            Literal("b: "),
            Ident("b"),
            SemiColon,
            Print,
            Literal("x > y: "),
            Ident("x"),
            GreaterThan,
            Ident("y"),
            SemiColon,
            Print,
            Literal("x >= y: "),
            Ident("x"),
            GreaterThanEqual,
            Ident("y"),
            SemiColon,
            Print,
            Literal("x < y: "),
            Ident("x"),
            LessThan,
            Ident("y"),
            SemiColon,
            Print,
            Literal("x <= y: "),
            Ident("x"),
            LessThanEqual,
            Ident("y"),
            SemiColon,
            Print,
            Literal("x == a: "),
            Ident("x"),
            Equal,
            Ident("a"),
            SemiColon,
            Print,
            Literal("x != b: "),
            Ident("x"),
            NotEqual,
            Ident("b"),
            SemiColon,
            ProgramEnd,
        ];

        let mut lexer = Lexer::new(program);
        for token in tokens {
            let lex_token = lexer.next_token()?;
            assert_eq!(token, lex_token);
        }
        assert!(lexer.next_token().is_err());
        Ok(())
    }

    #[test]
    fn test_function() -> Result<()> {
        let program = r#"
            EN VAZHI THANI VAZHI myfunc_one
                DOT "Hello from myfunc_one!";
                AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 100;
                DOT "returning ix =" ix "to main";
                IDHU EPDI IRUKKU ix;
            MARAKKADHINGA

            LAKSHMI START
            DOT "Hi from main!";
            y CHUMMA ADHURUDHULA myfunc_one;
            DOT "Value returned from myfunc_one:" y;
            MAGIZHCHI
        "#;
        let tokens = vec![
            FuncDeclare,
            Ident("myfunc_one"),
            Print,
            Literal("Hello from myfunc_one!"),
            SemiColon,
            StartDeclare,
            Ident("ix"),
            Declare,
            Number(100),
            SemiColon,
            Print,
            Literal("returning ix ="),
            Ident("ix"),
            Literal("to main"),
            SemiColon,
            FuncReturn,
            Ident("ix"),
            SemiColon,
            EndFunc,
            ProgramStart,
            Print,
            Literal("Hi from main!"),
            SemiColon,
            Ident("y"),
            FuncCall,
            Ident("myfunc_one"),
            SemiColon,
            Print,
            Literal("Value returned from myfunc_one:"),
            Ident("y"),
            SemiColon,
            ProgramEnd,
        ];

        let mut lexer = Lexer::new(program);
        for token in tokens {
            let lex_token = lexer.next_token()?;
            assert_eq!(token, lex_token);
        }
        assert!(lexer.next_token().is_err());
        Ok(())
    }
}
