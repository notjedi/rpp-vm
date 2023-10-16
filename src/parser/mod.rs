use std::{
    error::Error,
    fmt::{Debug, Display},
};

use anyhow::{bail, Result};

use crate::lexer::Token;

#[derive(Debug)]
pub enum ParseError<'a> {
    OutOfTokens,
    UnexpectedToken(&'a Token<'a>),
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OutOfTokens => write!(f, "OutOfTokens"),
            Self::UnexpectedToken(arg0) => f.debug_tuple("UnexpectedToken").field(arg0).finish(),
        }
    }
}

impl<'a> Error for ParseError<'a> {}

pub struct Program<'a> {
    functions: Vec<Function<'a>>,
    statments: Vec<Statement<'a>>,
}

pub struct Function<'a> {
    func_name: String,
    func_body: Vec<Statement<'a>>,
}

pub struct Statement<'a> {
    body: Vec<Token<'a>>,
}

pub struct ForStatement<'a> {
    init: Statement<'a>,
    condition: Statement<'a>,
    update: Statement<'a>,
    body: Vec<Statement<'a>>,
}

// pub struct Expression {}

pub struct RDParser<'a> {
    tokens: &'a [Token<'a>],
    curr_token: &'a Token<'a>,
    read_pos: usize,
}

impl<'a> RDParser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        let mut parser = Self {
            tokens,
            curr_token: &tokens[0],
            read_pos: 0,
        };
        let _ = parser.consume();
        parser
    }

    // pub fn consume(&mut self) -> Result<()> {
    pub fn consume(&mut self) -> Result<(), ParseError<'a>> {
        if self.read_pos < self.tokens.len() {
            self.curr_token = &self.tokens[self.read_pos];
            self.read_pos += 1;
            return Ok(());
        }
        Err(ParseError::OutOfTokens)
        // bail!(ParseError::OutOfTokens);
    }

    pub fn peek(&self) -> Token<'a> {
        if self.read_pos < self.tokens.len() {}
        todo!()
    }

    pub fn eat(&mut self, token: &Token) -> Result<(), ParseError<'a>> {
        if self.curr_token == token {
            self.consume()?;
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken(self.curr_token))
        }
    }

    fn parse_functions(&mut self) -> Result<Vec<Function<'a>>, ParseError<'a>> {
        todo!()
    }

    fn parse_main(&mut self) -> Result<Vec<Statement<'a>>, ParseError<'a>> {
        todo!()
    }

    pub fn parse(&mut self) -> Result<Program<'a>, ParseError<'a>> {
        Ok(Program {
            functions: self.parse_functions()?,
            statments: self.parse_main()?,
        })
    }
}
