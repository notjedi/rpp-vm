use std::{
    error::Error,
    fmt::{Debug, Display},
};

use anyhow::Result;

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

pub enum StmtEnum<'a> {
    BreakLoop,
    FuncCall(String),
    Print(Expression<'a>),
    FuncReturn(Expression<'a>),
    Assign(String, Expression<'a>),
}

pub struct Program<'a> {
    functions: Vec<Function<'a>>,
    statments: Vec<Statement<'a>>,
}

pub struct Function<'a> {
    func_name: String,
    func_body: Vec<Statement<'a>>,
}

pub struct Statement<'a> {
    body: StmtEnum<'a>,
}

pub struct ForStatement<'a> {
    init: Statement<'a>,
    condition: Statement<'a>,
    update: Statement<'a>,
    body: Vec<Statement<'a>>,
}

pub struct Expression<'a> {
    exprs: Vec<Token<'a>>,
}

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
    }

    pub fn peek(&self) -> Result<&'a Token<'a>, ParseError<'a>> {
        if self.read_pos < self.tokens.len() {
            Ok(&self.tokens[self.read_pos])
        } else {
            Err(ParseError::OutOfTokens)
        }
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
        let mut functions: Vec<Function<'a>> = Vec::new();
        while self.curr_token == &Token::FuncDeclare {
            self.consume()?;
            let statement = self.parse_statement()?;
            // functions.push(func);
        }
        Ok(functions)
    }

    fn parse_main(&mut self) -> Result<Vec<Statement<'a>>, ParseError<'a>> {
        todo!()
    }

    // fn parse_statement(&mut self) -> Result<Vec<Statement<'a>>, ParseError<'a>> {}
    fn parse_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        // IF_COND logical_expression L_BRACE statements R_BRACE
        // IF_COND logical_expression L_BRACE statements R_BRACE END_BLOCK SEMI_COLON
        // FOR_START forvar FOR_RANGE_START forvar FOR_RANGE_END
        // WHILE_LOOP expression L_BRACE statements R_BRACE END_BLOCK SEMI_COLON
        // expression SEMI_COLON
        // variable ASSIGN expression SEMI_COLON
        // variable FUNC_CALL func_name SEMI_COLON
        let body = match *self.curr_token {
            Token::BreakLoop => {
                // BREAK_LOOP SEMI_COLON
                self.consume()?;
                self.eat(&Token::SemiColon)?;
                StmtEnum::BreakLoop
            }
            Token::Print => {
                // PRINT printexprs SEMI_COLON
                self.consume()?;
                let expr = self.parse_expression()?;
                self.eat(&Token::SemiColon)?;
                StmtEnum::Print(expr)
            }
            Token::FuncCall => {
                // FUNC_CALL func_name SEMI_COLON
                self.consume()?;
                let Token::Literal(func_name) = self.curr_token else {
                    return Err(ParseError::UnexpectedToken(self.curr_token));
                };
                self.eat(&Token::SemiColon)?;
                StmtEnum::FuncCall(func_name.to_string())
            }
            Token::FuncReturn => {
                // FUNC_RETURN expression SEMI_COLON
                self.consume()?;
                let expr = self.parse_expression()?;
                self.eat(&Token::SemiColon)?;
                StmtEnum::FuncReturn(expr)
            }
            Token::StartDeclare => {
                // START_DECLARE variable DECLARE expression SEMI_COLON
                // START_DECLARE variable DECLARE_ALT expression SEMI_COLON
                self.consume()?;
                let Token::Ident(var) = self.curr_token else {
                    return Err(ParseError::UnexpectedToken(self.curr_token));
                };
                self.eat(&Token::Declare).or(self.eat(&Token::DeclareAlt))?;
                let expr = self.parse_expression()?;
                self.eat(&Token::SemiColon)?;
                StmtEnum::Assign(var.to_string(), expr)
            }
            Token::IfCond => {
                todo!()
            }
            Token::ForStart => {
                todo!()
            }
            Token::WhileLoop => {
                todo!()
            }
            // TOOD: expression
            Token::Ident(var) => {
                todo!()
            }
            _ => return Err(ParseError::UnexpectedToken(self.curr_token)),
        };
        Ok(Statement { body })
    }

    fn parse_expression(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let exprs = match *self.curr_token {
            Token::Sum | Token::Sub => {
                let mut tokens = vec![*self.curr_token];
                self.consume()?;
                tokens.append(&mut self.parse_expression()?.exprs);
                tokens
            }

            Token::Number(_) | Token::Float(_) | Token::BoolTrue | Token::BoolFalse => {
                let mut tokens = vec![*self.curr_token];
                self.consume()?;
                let next_token = self.peek()?;
                if Token::is_mathop(next_token) || Token::is_logicalop(next_token) {
                    tokens.push(*next_token);
                    self.consume()?;
                    tokens.append(&mut self.parse_expression()?.exprs);
                }
                tokens
            }

            Token::Literal(_) => vec![*self.curr_token],
            Token::Ident(_) => vec![*self.curr_token],
            _ => return Err(ParseError::UnexpectedToken(self.curr_token)),
        };

        Ok(Expression { exprs })
    }

    pub fn parse(&mut self) -> Result<Program<'a>, ParseError<'a>> {
        Ok(Program {
            functions: self.parse_functions()?,
            statments: self.parse_main()?,
        })
    }
}
