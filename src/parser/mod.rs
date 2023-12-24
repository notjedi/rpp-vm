use crate::lexer::{KeyWord, Literal, Token};

use std::{iter::Peekable, vec::IntoIter};
use thiserror::Error;

type BoxExpr = Box<Expr>;
type BoxStr = Box<String>;
type BoxForVar = Box<ForVar>;
type BoxVecStmtKind = Box<Vec<StmtKind>>;

#[derive(Debug, Error)]
pub(crate) enum ParseError {
    #[error("expected: {expected:?}, found: {found:?}")]
    MissingExpectedToken { expected: Token, found: Token },
    #[error("unexpected token: {0:?}")]
    UnexpectedToken(Token),
    #[error("invalid expr: {0:?}")]
    InvalidExpr(Expr),
}

#[derive(Debug)]
pub(crate) enum ForVar {
    Int(i64),
    Float(f64),
    Ident(BoxStr),
}

#[derive(Debug)]
pub(crate) enum StmtKind {
    BreakLoop,
    Expr(Expr),
    Comment(String),
    Print(Vec<Expr>),
    FuncCall(String),
    FuncReturn(Expr),
    Assign {
        lhs: BoxStr,
        rhs: BoxExpr,
    },
    AssignFuncCall {
        lhs: BoxStr,
        rhs: BoxStr,
    },
    IfCond {
        condition: BoxExpr,
        body: BoxVecStmtKind,
        else_body: BoxVecStmtKind,
    },
    ForLoop {
        start: BoxForVar,
        end: BoxForVar,
        body: BoxVecStmtKind,
    },
    WhileLoop {
        condition: BoxExpr,
        body: BoxVecStmtKind,
    },
}

#[derive(Debug)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinaryOp {
    fn from_token(token: &Token) -> Option<Self> {
        let op = match token {
            Token::KeyWord(KeyWord::Sum) => Self::Add,
            Token::KeyWord(KeyWord::Sub) => Self::Sub,
            Token::KeyWord(KeyWord::Mul) => Self::Mul,
            Token::KeyWord(KeyWord::Div) => Self::Div,
            Token::KeyWord(KeyWord::Mod) => Self::Mod,
            _ => return None,
        };
        Some(op)
    }
}

#[derive(Debug)]
pub(crate) enum LogicalOp {
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    Equal,
    NotEqual,
}

impl LogicalOp {
    fn from_token(token: &Token) -> Option<Self> {
        let op = match token {
            Token::KeyWord(KeyWord::GreaterThan) => Self::GreaterThan,
            Token::KeyWord(KeyWord::LessThan) => Self::LessThan,
            Token::KeyWord(KeyWord::GreaterThanEqual) => Self::GreaterThanEqual,
            Token::KeyWord(KeyWord::LessThanEqual) => Self::LessThanEqual,
            Token::KeyWord(KeyWord::Equal) => Self::Equal,
            Token::KeyWord(KeyWord::NotEqual) => Self::NotEqual,
            _ => return None,
        };
        Some(op)
    }
}

#[derive(Debug)]
pub(crate) enum ExprLeaf {
    BoolTrue,
    BoolFalse,
    Int(i64),
    Float(f64),
    Char(char),
    Str(BoxStr),
}

impl ExprLeaf {
    fn from_literal(literal: Literal) -> Self {
        match literal {
            Literal::Char(ch) => Self::Char(ch),
            Literal::Str(string) => Self::Str(Box::new(string)),
            Literal::Int(num) => Self::Int(num),
            Literal::Float(num) => Self::Float(num),
            Literal::BoolTrue => Self::BoolTrue,
            Literal::BoolFalse => Self::BoolFalse,
        }
    }
}

#[derive(Debug)]
pub(crate) enum Op {
    BinaryOp(BinaryOp),
    LogicalOp(LogicalOp),
}

#[derive(Debug)]
pub(crate) enum Expr {
    BinaryExpr {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    LogicalExpr {
        op: LogicalOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    UnaryExpr {
        op: BinaryOp,
        child: Box<Expr>,
    },
    ExprLeaf(ExprLeaf),
    Ident(BoxStr),
}

#[derive(Debug)]
pub(crate) struct Function {
    name: BoxStr,
    body: BoxVecStmtKind,
}

#[derive(Debug)]
pub(crate) struct Program {
    pub(crate) functions: Vec<Function>,
    pub(crate) main_stmts: BoxVecStmtKind,
}

pub(crate) struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    fn missing_expected_token(expected: Token, found: Token) -> ParseError {
        ParseError::MissingExpectedToken { expected, found }
    }

    fn get_binding_power(tok: &Op) -> u32 {
        // https://stackoverflow.com/questions/3114107/modulo-in-order-of-operation
        // https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
        // https://i.stack.imgur.com/foy5H.png
        match tok {
            Op::LogicalOp(log_op) => match log_op {
                LogicalOp::Equal | LogicalOp::NotEqual => 1,
                LogicalOp::LessThan
                | LogicalOp::LessThanEqual
                | LogicalOp::GreaterThan
                | LogicalOp::GreaterThanEqual => 3,
            },
            Op::BinaryOp(bin_op) => match bin_op {
                BinaryOp::Add | BinaryOp::Sub => 5,
                BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 7,
            },
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn consume(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn expect(&mut self, token: Token) -> Result<(), ParseError> {
        if Some(&token) == self.peek() {
            self.consume();
            return Ok(());
        }
        Err(Self::missing_expected_token(
            token,
            self.peek().unwrap_or_default().clone(),
        ))
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        match self.consume() {
            Some(Token::Ident(ident)) => Ok(ident),
            token => Err(Self::missing_expected_token(
                Token::Ident("".to_string()),
                token.unwrap_or_default(),
            )),
        }
    }

    pub(crate) fn parse(&mut self) -> Result<Program, ParseError> {
        let mut functions = Vec::new();
        while let Some(Token::KeyWord(KeyWord::FuncDeclare)) = self.peek() {
            let func = self.parse_function()?;
            functions.push(func);
        }
        let mut main_stmts = Vec::new();
        // main block is optional
        if self.peek() == Some(&Token::KeyWord(KeyWord::ProgramStart)) {
            self.consume();
            while self.peek() != Some(&Token::KeyWord(KeyWord::ProgramEnd)) {
                let stmt = self.parse_statement()?;
                main_stmts.push(stmt);
            }
            self.consume();
        }
        // TODO: should i assert EOF?
        Ok(Program {
            functions,
            main_stmts: Box::new(main_stmts),
        })
    }

    fn parse_function(&mut self) -> Result<Function, ParseError> {
        // function := FUNC_DECLARE func_name statements END_FUNC
        self.expect(Token::KeyWord(KeyWord::FuncDeclare))?;
        let func_name = match self.consume() {
            Some(Token::Ident(func_name)) => func_name,
            token => {
                return Err(Self::missing_expected_token(
                    Token::Ident("func_name".to_string()),
                    token.unwrap_or_default(),
                ));
            }
        };
        let mut statements = Vec::new();
        while let Some(token) = self.peek()
            && token != &Token::KeyWord(KeyWord::EndFunc)
        {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }
        self.expect(Token::KeyWord(KeyWord::EndFunc))?;
        Ok(Function {
            name: Box::new(func_name),
            body: Box::new(statements),
        })
    }

    fn parse_statement(&mut self) -> Result<StmtKind, ParseError> {
        let stmtkind = match self.peek().unwrap_or_default() {
            Token::Comment(_) => {
                let Token::Comment(comment) = self.consume().unwrap() else {
                    unreachable!()
                };
                StmtKind::Comment(comment)
            }
            Token::KeyWord(KeyWord::BreakLoop) => {
                // BREAK_LOOP SEMI_COLON
                self.consume();
                self.expect(KeyWord::SemiColon.into())?;
                StmtKind::BreakLoop
            }
            Token::KeyWord(KeyWord::Print) => {
                // PRINT printexprs SEMI_COLON
                // printexprs := printexprs expression
                self.consume();
                let mut exprs = Vec::new();
                while Some(&Token::KeyWord(KeyWord::SemiColon)) != self.peek() {
                    let expr = self.parse_expression(0)?;
                    exprs.push(expr);
                }
                self.consume();
                StmtKind::Print(exprs)
            }
            Token::KeyWord(KeyWord::FuncCall) => {
                // FUNC_CALL func_name SEMI_COLON
                self.consume();
                let func_name = self.expect_ident()?;
                self.expect(Token::KeyWord(KeyWord::SemiColon))?;
                StmtKind::FuncCall(func_name)
            }
            Token::KeyWord(KeyWord::FuncReturn) => {
                // FUNC_RETURN expression SEMI_COLON
                self.consume();
                let expr = self.parse_expression(0)?;
                self.expect(Token::KeyWord(KeyWord::SemiColon))?;
                StmtKind::FuncReturn(expr)
            }
            Token::KeyWord(KeyWord::StartDeclare) => {
                // START_DECLARE variable DECLARE expression SEMI_COLON
                // START_DECLARE variable DECLARE_ALT expression SEMI_COLON
                self.consume();
                let var = self.expect_ident()?;
                self.expect(Token::KeyWord(KeyWord::Declare))
                    .or_else(|_| self.expect(Token::KeyWord(KeyWord::DeclareAlt)))?;
                let expr = self.parse_expression(0)?;
                self.expect(Token::KeyWord(KeyWord::SemiColon))?;
                StmtKind::Assign {
                    lhs: Box::new(var),
                    rhs: Box::new(expr),
                }
            }
            Token::KeyWord(KeyWord::IfCond) => {
                // IF_COND logical_expression L_BRACE statements R_BRACE END_BLOCK SEMI_COLON
                // IF_COND logical_expression L_BRACE statements R_BRACE ELSE_COND L_BRACE statements R_BRACE END_BLOCK SEMI_COLON
                self.consume();
                let expr = self.parse_expression(0)?;
                match expr {
                    Expr::LogicalExpr { .. }
                    | Expr::ExprLeaf(ExprLeaf::BoolTrue)
                    | Expr::ExprLeaf(ExprLeaf::BoolFalse) => {}
                    _ => return Err(ParseError::InvalidExpr(expr)),
                }
                self.expect(Token::KeyWord(KeyWord::LeftBrace))?;
                let mut statements = Vec::new();
                let mut else_statements = Vec::new();

                while let Some(token) = self.peek()
                    && token != &Token::KeyWord(KeyWord::RightBrace)
                {
                    let stmt = self.parse_statement()?;
                    statements.push(stmt);
                }
                self.consume(); // consume the R_BRACE

                match self.consume() {
                    Some(Token::KeyWord(KeyWord::EndBlock)) => {
                        self.expect(Token::KeyWord(KeyWord::SemiColon))?;
                    }
                    Some(Token::KeyWord(KeyWord::ElseCond)) => {
                        self.expect(Token::KeyWord(KeyWord::LeftBrace))?;
                        while let Some(token) = self.peek()
                            && token != &Token::KeyWord(KeyWord::RightBrace)
                        {
                            let stmt = self.parse_statement()?;
                            else_statements.push(stmt);
                        }
                        self.expect(Token::KeyWord(KeyWord::RightBrace))?;
                        self.expect(Token::KeyWord(KeyWord::EndBlock))?;
                        self.expect(Token::KeyWord(KeyWord::SemiColon))?;
                    }
                    tok => {
                        return Err(Self::missing_expected_token(
                            Token::KeyWord(KeyWord::EndBlock),
                            tok.unwrap_or_default(),
                        ));
                    }
                }
                StmtKind::IfCond {
                    condition: Box::new(expr),
                    body: Box::new(statements),
                    else_body: Box::new(else_statements),
                }
            }
            Token::KeyWord(KeyWord::ForStart) => {
                // FOR_START forvar FOR_RANGE_START forvar FOR_RANGE_END L_BRACE statements R_BRACE END_BLOCK SEMI_COLON"
                // forvar := NUMBER | WORD
                self.consume();
                let for_start = match self.consume().unwrap_or_default() {
                    Token::Ident(ident) => ForVar::Ident(Box::new(ident)),
                    Token::Literal(Literal::Int(num)) => ForVar::Int(num),
                    Token::Literal(Literal::Float(num)) => ForVar::Float(num),
                    tok => {
                        return Err(Self::missing_expected_token(
                            Token::KeyWord(KeyWord::Assign),
                            tok,
                        ));
                    }
                };
                self.expect(Token::KeyWord(KeyWord::ForRangeStart))?;
                let for_end = match self.consume().unwrap_or_default() {
                    Token::Ident(ident) => ForVar::Ident(Box::new(ident)),
                    Token::Literal(Literal::Int(num)) => ForVar::Int(num),
                    Token::Literal(Literal::Float(num)) => ForVar::Float(num),
                    tok => {
                        return Err(Self::missing_expected_token(
                            Token::KeyWord(KeyWord::Assign),
                            tok,
                        ));
                    }
                };
                self.expect(Token::KeyWord(KeyWord::ForRangeEnd))?;
                self.expect(Token::KeyWord(KeyWord::LeftBrace))?;
                let mut statements = Vec::new();
                while let Some(token) = self.peek()
                    && token != &Token::KeyWord(KeyWord::RightBrace)
                {
                    let stmt = self.parse_statement()?;
                    statements.push(stmt);
                }
                self.consume();
                self.expect(Token::KeyWord(KeyWord::EndBlock))?;
                self.expect(Token::KeyWord(KeyWord::SemiColon))?;
                StmtKind::ForLoop {
                    start: Box::new(for_start),
                    end: Box::new(for_end),
                    body: Box::new(statements),
                }
            }
            Token::KeyWord(KeyWord::WhileLoop) => {
                // WHILE_LOOP expression L_BRACE statements R_BRACE END_BLOCK SEMI_COLON
                self.consume();
                let expr = self.parse_expression(0)?;
                match expr {
                    Expr::LogicalExpr { .. }
                    | Expr::ExprLeaf(ExprLeaf::BoolTrue)
                    | Expr::ExprLeaf(ExprLeaf::BoolFalse) => {}
                    _ => return Err(ParseError::InvalidExpr(expr)),
                }
                self.expect(Token::KeyWord(KeyWord::LeftBrace))?;
                let mut statements = Vec::new();
                while let Some(token) = self.peek()
                    && token != &Token::KeyWord(KeyWord::RightBrace)
                {
                    let stmt = self.parse_statement()?;
                    statements.push(stmt);
                }
                self.consume();
                self.expect(Token::KeyWord(KeyWord::EndBlock))?;
                self.expect(Token::KeyWord(KeyWord::SemiColon))?;
                StmtKind::WhileLoop {
                    condition: Box::new(expr),
                    body: Box::new(statements),
                }
            }
            Token::Ident(_) => {
                // variable ASSIGN expression SEMI_COLON
                // variable FUNC_CALL func_name SEMI_COLON
                let Token::Ident(ident) = self.consume().unwrap() else {
                    unreachable!()
                };
                match self.consume().unwrap_or_default() {
                    Token::KeyWord(KeyWord::Assign) => {
                        let expr = self.parse_expression(0)?;
                        self.expect(Token::KeyWord(KeyWord::SemiColon))?;
                        StmtKind::Assign {
                            lhs: Box::new(ident),
                            rhs: Box::new(expr),
                        }
                    }
                    Token::KeyWord(KeyWord::FuncCall) => {
                        let next_tok = self.consume().unwrap_or_default();
                        if let Token::Ident(func_name) = next_tok {
                            self.expect(Token::KeyWord(KeyWord::SemiColon))?;
                            StmtKind::AssignFuncCall {
                                lhs: Box::new(ident),
                                rhs: Box::new(func_name),
                            }
                        } else {
                            return Err(Self::missing_expected_token(
                                Token::KeyWord(KeyWord::Assign),
                                next_tok,
                            ));
                        }
                    }
                    peek_tok => {
                        return Err(Self::missing_expected_token(
                            Token::KeyWord(KeyWord::Assign),
                            peek_tok,
                        ));
                    }
                }
            }
            _ => {
                // expression SEMI_COLON
                let expr = self.parse_expression(0)?;
                self.expect(Token::KeyWord(KeyWord::SemiColon))?;
                StmtKind::Expr(expr)
            }
        };
        Ok(stmtkind)
    }

    fn parse_expression(&mut self, precedence_limit: u32) -> Result<Expr, ParseError> {
        // Pratt parsing
        // https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html
        // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
        // https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy
        let mut lhs = match self.consume().unwrap_or_default() {
            Token::Literal(literal) => Expr::ExprLeaf(ExprLeaf::from_literal(literal)),
            Token::Ident(ident) => Expr::Ident(Box::new(ident)),
            op @ Token::KeyWord(KeyWord::Sub) | op @ Token::KeyWord(KeyWord::Sum) => {
                let op = BinaryOp::from_token(&op).unwrap();
                Expr::UnaryExpr {
                    op,
                    child: Box::new(self.parse_expression(0)?),
                }
            }
            tok => return Err(ParseError::UnexpectedToken(tok)),
        };

        while let Some(op @ Token::KeyWord(_)) = self.peek() {
            let op = if let Some(bin_op) = BinaryOp::from_token(op) {
                Op::BinaryOp(bin_op)
            } else if let Some(log_op) = LogicalOp::from_token(op) {
                Op::LogicalOp(log_op)
            } else {
                break;
            };

            let precedence = Self::get_binding_power(&op);
            if precedence <= precedence_limit {
                break;
            }
            self.consume();

            let rhs = self.parse_expression(precedence)?;
            match op {
                Op::BinaryOp(bin_op) => {
                    lhs = Expr::BinaryExpr {
                        op: bin_op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                }
                Op::LogicalOp(log_op) => {
                    lhs = Expr::LogicalExpr {
                        op: log_op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                }
            };
        }

        Ok(lhs)
    }
}

#[cfg(test)]
mod tests {
    use color_eyre::eyre::Result;

    use crate::lexer::Lexer;

    use super::Parser;

    #[test]
    fn test_parser() -> Result<()> {
        color_eyre::install()?;
        let program = r#"
            EN VAZHI THANI VAZHI myfunc_one
                AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 100;
                DOT "returning ix =" ix "to main";
                IDHU EPDI IRUKKU ix;
            MARAKKADHINGA

            LAKSHMI START
                !! checking exprs
                25 + 15;
                25 - 15;
                5.5 * -5;
                5 / 5;
                51 % 5;

                !! testing while loop
                BABA COUNTING STARTS True{
                    DOT ix;
                    ix BHAJJI SAAPDU ix + 1;
                    EN PEAR MANICKAM ix >= 5{
                        DOT "breaking out of loop...";
                        BLACK SHEEP;
                    }KATHAM KATHAM;
                }KATHAM KATHAM;

                y CHUMMA ADHURUDHULA myfunc_one;

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
                    ix BHAJJI SAAPDU ix+1;
                }KATHAM KATHAM;
            MAGIZHCHI
        "#;

        let tokens = Lexer::tokenize_str(program).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        dbg!(ast.functions);
        dbg!(ast.main_stmts);
        // assert!(false);
        Ok(())
    }
}
