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
    Expression(Expression<'a>),
    Assign {
        lhs: String,
        rhs: Expression<'a>,
    },
    AssignFuncCall {
        lhs: String,
        rhs: String,
    },
    IfCond {
        condition: Expression<'a>,
        body: Vec<StmtEnum<'a>>,
    },
    ForLoop {
        start: Token<'a>,
        end: Token<'a>,
    },
    WhileLoop {
        condition: Expression<'a>,
        body: Vec<StmtEnum<'a>>,
    },
}

pub struct Program<'a> {
    functions: Vec<Function<'a>>,
    statments: Vec<StmtEnum<'a>>,
}

pub struct Function<'a> {
    name: String,
    body: Vec<StmtEnum<'a>>,
}

pub struct ForStatement<'a> {
    init: StmtEnum<'a>,
    condition: StmtEnum<'a>,
    update: StmtEnum<'a>,
    body: Vec<StmtEnum<'a>>,
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
        // FUNC_DECLARE func_name statements END_FUNC
        let mut functions: Vec<Function<'a>> = Vec::new();
        while self.curr_token == &Token::FuncDeclare {
            self.consume()?;
            let Token::Literal(func_name) = self.curr_token else {
                return Err(ParseError::UnexpectedToken(self.curr_token));
            };
            let mut body = Vec::new();
            while self.curr_token != &Token::EndFunc {
                let statement = self.parse_statement()?;
                body.push(statement);
            }
            self.consume()?;
            functions.push(Function {
                name: func_name.to_string(),
                body,
            })
        }
        Ok(functions)
    }

    fn parse_main(&mut self) -> Result<Vec<StmtEnum<'a>>, ParseError<'a>> {
        todo!()
    }

    // fn parse_statement(&mut self) -> Result<Vec<Statement<'a>>, ParseError<'a>> {}
    fn parse_statement(&mut self) -> Result<StmtEnum<'a>, ParseError<'a>> {
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
                StmtEnum::Assign {
                    lhs: var.to_string(),
                    rhs: expr,
                }
            }
            Token::IfCond => {
                // | IF_COND logical_expression L_BRACE statements R_BRACE
                // | IF_COND logical_expression L_BRACE statements R_BRACE END_BLOCK SEMI_COLON
                self.consume()?;
                let mut statements = Vec::new();
                let expr = self.parse_expression()?;
                self.eat(&Token::LeftBrace)?;

                while self.curr_token == &Token::RightBrace {
                    self.consume()?;
                    let statement = self.parse_statement()?;
                    statements.push(statement);
                }
                self.eat(&Token::RightBrace)?;
                if *self.curr_token == Token::EndBlock {
                    self.consume()?;
                    self.eat(&Token::SemiColon)?;
                }
                StmtEnum::IfCond {
                    condition: expr,
                    body: statements,
                }
            }
            Token::ForStart => {
                // FOR_START forvar FOR_RANGE_START forvar FOR_RANGE_END
                self.consume()?;
                let for_start = match *self.curr_token {
                    Token::Ident(_) | Token::Number(_) => *self.curr_token,
                    _ => return Err(ParseError::UnexpectedToken(self.curr_token)),
                };
                self.eat(&Token::ForRangeStart)?;
                let for_end = match *self.curr_token {
                    Token::Ident(_) | Token::Number(_) => *self.curr_token,
                    _ => return Err(ParseError::UnexpectedToken(self.curr_token)),
                };
                self.eat(&Token::ForRangeEnd)?;
                StmtEnum::ForLoop {
                    start: for_start,
                    end: for_end,
                }
            }
            Token::WhileLoop => {
                // WHILE_LOOP expression L_BRACE statements R_BRACE END_BLOCK SEMI_COLON
                self.consume()?;
                let expr = self.parse_expression()?;
                self.eat(&Token::LeftBrace)?;

                let mut statements = Vec::new();
                while self.curr_token == &Token::RightBrace {
                    self.consume()?;
                    let statement = self.parse_statement()?;
                    statements.push(statement);
                }

                self.consume()?;
                self.eat(&Token::EndBlock)?;
                self.eat(&Token::SemiColon)?;
                StmtEnum::WhileLoop {
                    condition: expr,
                    body: statements,
                }
            }
            Token::Ident(var) => {
                // variable ASSIGN expression SEMI_COLON
                // variable FUNC_CALL func_name SEMI_COLON
                self.consume()?;
                let ret_value = match *self.curr_token {
                    Token::Assign => {
                        self.consume()?;
                        let expr = self.parse_expression()?;
                        self.eat(&Token::SemiColon)?;
                        StmtEnum::Assign {
                            lhs: var.to_string(),
                            rhs: expr,
                        }
                    }
                    Token::FuncCall => {
                        self.consume()?;
                        let Token::Literal(func_name) = self.curr_token else {
                            return Err(ParseError::UnexpectedToken(self.curr_token));
                        };
                        self.eat(&Token::SemiColon)?;
                        StmtEnum::AssignFuncCall {
                            lhs: var.to_string(),
                            rhs: func_name.to_string(),
                        }
                    }
                    _ => return Err(ParseError::UnexpectedToken(self.curr_token)),
                };
                ret_value
            }
            _ => {
                // expression SEMI_COLON
                StmtEnum::Expression(self.parse_expression()?)
            }
        };
        Ok(body)
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

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    #[test]
    fn test_parse_functions() {
        let program = r#"
            EN VAZHI THANI VAZHI myfunc_one
                DOT "Hello from myfunc_one!";
                AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 100;
                DOT "returning ix =" ix "to main";
                IDHU EPDI IRUKKU ix;
            MARAKKADHINGA
        "#;
        let mut lexer = Lexer::new(program);
        let mut tokens = Vec::new();
        loop {
            match lexer.next_token() {
                Some(lex_token) => tokens.push(lex_token),
                None => break,
            }
        }
        // while let Some(token) = lexer.next_token() {
        //     tokens.push(token.clone())
        // }
        // for token in tokens {
        //     let lex_token = lexer.next_token();
        //     assert_eq!(Some(token), lex_token);
        // }
    }
}
