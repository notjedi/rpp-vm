use color_eyre::eyre::Result;
use std::{borrow::Cow, cell::RefCell};
use thiserror::Error;

use crate::lexer::{KeyWord, Literal, TokenKind};

type BoxExpr<'a> = Box<Expr<'a>>;
type BoxForVar<'a> = Box<ForVar<'a>>;
type BoxVecStmtKind<'a> = Box<Vec<StmtKind<'a>>>;

#[derive(Debug, Error)]
pub(crate) enum ParseError<'a> {
    #[error("expected: {expected:?}, found: {found:?}")]
    MissingExpectedToken {
        expected: TokenKind<'a>,
        found: TokenKind<'a>,
    },
    #[error("unexpected token: {0:?}")]
    UnexpectedToken(TokenKind<'a>),
    #[error("invalid expr: {0:?}")]
    InvalidExpr(Expr<'a>),
}

#[derive(Clone, Debug)]
pub(crate) enum Value<'a> {
    Unit,
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    Str(Cow<'a, str>),
}

#[derive(Clone, Debug)]
pub(crate) enum ForVar<'a> {
    Int(i64),
    // TODO: no use of float in for loops?
    Float(f64),
    Ident(&'a str),
}

// https://adeschamps.github.io/enum-size
// https://nnethercote.github.io/perf-book/type-sizes.html
// https://web.archive.org/web/20230530145515/https://boshen.github.io/javascript-parser-in-rust/docs/ast
#[derive(Clone, Debug)]
pub(crate) enum StmtKind<'a> {
    BreakLoop,
    Expr(Expr<'a>),
    #[allow(dead_code)]
    Comment(&'a str),
    Print(Vec<Expr<'a>>),
    FuncCall(&'a str),
    FuncReturn(Expr<'a>),
    Declare {
        lhs: &'a str,
        rhs: BoxExpr<'a>,
    },
    Assign {
        lhs: &'a str,
        rhs: BoxExpr<'a>,
    },
    AssignFuncCall {
        // TODO: convert this to Expr::ExprLeaf(ExprLeaf::Ident)?
        var_name: &'a str,
        func_name: &'a str,
    },
    IfCond {
        condition: BoxExpr<'a>,
        body: BoxVecStmtKind<'a>,
        else_body: BoxVecStmtKind<'a>,
    },
    ForLoop {
        start: BoxForVar<'a>,
        end: BoxForVar<'a>,
        body: BoxVecStmtKind<'a>,
    },
    WhileLoop {
        condition: BoxExpr<'a>,
        body: BoxVecStmtKind<'a>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinaryOp {
    fn from_token(token: &TokenKind) -> Option<Self> {
        let op = match token {
            TokenKind::KeyWord(KeyWord::Sum) => Self::Add,
            TokenKind::KeyWord(KeyWord::Sub) => Self::Sub,
            TokenKind::KeyWord(KeyWord::Mul) => Self::Mul,
            TokenKind::KeyWord(KeyWord::Div) => Self::Div,
            TokenKind::KeyWord(KeyWord::Mod) => Self::Mod,
            _ => return None,
        };
        Some(op)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LogicalOp {
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    Equal,
    NotEqual,
}

impl LogicalOp {
    fn from_token(token: &TokenKind) -> Option<Self> {
        let op = match token {
            TokenKind::KeyWord(KeyWord::GreaterThan) => Self::GreaterThan,
            TokenKind::KeyWord(KeyWord::LessThan) => Self::LessThan,
            TokenKind::KeyWord(KeyWord::GreaterThanEqual) => Self::GreaterThanEqual,
            TokenKind::KeyWord(KeyWord::LessThanEqual) => Self::LessThanEqual,
            TokenKind::KeyWord(KeyWord::Equal) => Self::Equal,
            TokenKind::KeyWord(KeyWord::NotEqual) => Self::NotEqual,
            _ => return None,
        };
        Some(op)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum ExprLeaf<'a> {
    BoolTrue,
    BoolFalse,
    Int(i64),
    Float(f64),
    Char(char),
    Str(&'a str),
}

impl<'a> ExprLeaf<'a> {
    fn from_literal(literal: &'a Literal<'a>) -> Self {
        match *literal {
            Literal::Char(ch) => Self::Char(ch),
            Literal::Str(string) => Self::Str(string),
            Literal::Int(num) => Self::Int(num),
            Literal::Float(num) => Self::Float(num),
            Literal::BoolTrue => Self::BoolTrue,
            Literal::BoolFalse => Self::BoolFalse,
        }
    }

    pub(crate) fn to_value(expr_leaf: &'a ExprLeaf<'a>) -> Value<'a> {
        match expr_leaf {
            ExprLeaf::BoolTrue => Value::Bool(true),
            ExprLeaf::BoolFalse => Value::Bool(false),
            ExprLeaf::Int(int) => Value::Int(*int),
            ExprLeaf::Float(float) => Value::Float(*float),
            ExprLeaf::Char(ch) => Value::Char(*ch),
            ExprLeaf::Str(str_val) => Value::Str(Cow::Borrowed(str_val)),
        }
    }
}

#[derive(Debug)]
pub(crate) enum Op {
    BinaryOp(BinaryOp),
    LogicalOp(LogicalOp),
}

#[derive(Clone, Debug)]
pub(crate) enum Expr<'a> {
    BinaryExpr {
        op: BinaryOp,
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
    },
    LogicalExpr {
        op: LogicalOp,
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
    },
    UnaryExpr {
        op: BinaryOp,
        child: Box<Expr<'a>>,
    },
    ExprLeaf(ExprLeaf<'a>),
    Ident(&'a str),
}

#[derive(Clone, Debug)]
pub(crate) struct Function<'a> {
    pub(crate) name: &'a str,
    pub(crate) body: BoxVecStmtKind<'a>,
}

#[derive(Debug)]
pub(crate) struct Program<'a> {
    pub(crate) functions: Vec<Function<'a>>,
    pub(crate) main_stmts: BoxVecStmtKind<'a>,
}

pub(crate) struct Parser<'a> {
    tokens: &'a [TokenKind<'a>],
    current_pos: RefCell<usize>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: &'a [TokenKind<'a>]) -> Self {
        Self {
            tokens,
            current_pos: RefCell::new(0),
        }
    }

    fn missing_expected_token(expected: TokenKind<'a>, found: TokenKind<'a>) -> ParseError<'a> {
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

    #[must_use]
    fn peek(&self) -> Option<&'a TokenKind<'a>> {
        self.tokens.get(*self.current_pos.borrow())
    }

    fn consume(&self) -> Option<&'a TokenKind<'a>> {
        let res = self.tokens.get(*self.current_pos.borrow());
        *self.current_pos.borrow_mut() += 1;
        res
    }

    fn expect(&self, kind: TokenKind<'a>) -> Result<(), ParseError<'a>> {
        if Some(&kind) == self.peek() {
            self.consume();
            return Ok(());
        }
        Err(Self::missing_expected_token(
            kind,
            self.peek().unwrap_or_default().clone(),
        ))
    }

    fn expect_ident(&self) -> Result<&'a str, ParseError<'a>> {
        match self.consume() {
            Some(&TokenKind::Ident(ident)) => Ok(ident),
            token => Err(Self::missing_expected_token(
                TokenKind::Ident(""),
                token.unwrap_or_default().clone(),
            )),
        }
    }

    pub(crate) fn parse(&self) -> Result<Program<'a>, ParseError<'a>> {
        let mut functions = Vec::new();
        while let Some(TokenKind::KeyWord(KeyWord::FuncDeclare)) = self.peek() {
            let func = self.parse_function()?;
            functions.push(func);
        }
        let mut main_stmts = Vec::new();
        // main block is optional
        if self.peek() == Some(&TokenKind::KeyWord(KeyWord::ProgramStart)) {
            self.consume();
            while self.peek() != Some(&TokenKind::KeyWord(KeyWord::ProgramEnd)) {
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

    pub(crate) fn parse_tokens(tokens: &'a [TokenKind<'a>]) -> Result<Program<'a>, ParseError<'a>> {
        let parser = Parser::new(tokens);
        // parser.parse().unwrap()
        parser.parse()
    }

    fn parse_function(&self) -> Result<Function<'a>, ParseError<'a>> {
        // function := FUNC_DECLARE func_name statements END_FUNC
        self.expect(TokenKind::KeyWord(KeyWord::FuncDeclare))?;
        let func_name = match self.consume() {
            Some(TokenKind::Ident(func_name)) => func_name,
            token => {
                return Err(Self::missing_expected_token(
                    TokenKind::Ident("func_name"),
                    token.unwrap_or_default().clone(),
                ));
            }
        };
        let mut statements = Vec::new();
        while let Some(token) = self.peek()
            && token != &TokenKind::KeyWord(KeyWord::EndFunc)
        {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }
        self.expect(TokenKind::KeyWord(KeyWord::EndFunc))?;
        Ok(Function {
            name: func_name,
            body: Box::new(statements),
        })
    }

    fn parse_statement(&self) -> Result<StmtKind<'a>, ParseError<'a>> {
        let stmtkind = match self.peek().unwrap_or_default() {
            TokenKind::Comment(_) => {
                let TokenKind::Comment(comment) = self.consume().unwrap() else {
                    unreachable!()
                };
                StmtKind::Comment(comment)
            }
            TokenKind::KeyWord(KeyWord::BreakLoop) => {
                // BREAK_LOOP SEMI_COLON
                self.consume();
                self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                StmtKind::BreakLoop
            }
            TokenKind::KeyWord(KeyWord::Print) => {
                // PRINT printexprs SEMI_COLON
                // printexprs := printexprs expression
                self.consume();
                let mut exprs = Vec::new();
                while Some(&TokenKind::KeyWord(KeyWord::SemiColon)) != self.peek() {
                    let expr = self.parse_expression(0)?;
                    exprs.push(expr);
                }
                self.consume();
                StmtKind::Print(exprs)
            }
            TokenKind::KeyWord(KeyWord::FuncCall) => {
                // FUNC_CALL func_name SEMI_COLON
                self.consume();
                let func_name = self.expect_ident()?;
                self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                StmtKind::FuncCall(func_name)
            }
            TokenKind::KeyWord(KeyWord::FuncReturn) => {
                // FUNC_RETURN expression SEMI_COLON
                self.consume();
                let expr = self.parse_expression(0)?;
                self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                StmtKind::FuncReturn(expr)
            }
            TokenKind::KeyWord(KeyWord::StartDeclare) => {
                // START_DECLARE variable DECLARE expression SEMI_COLON
                // START_DECLARE variable DECLARE_ALT expression SEMI_COLON
                self.consume();
                let var = self.expect_ident()?;
                self.expect(TokenKind::KeyWord(KeyWord::Declare))
                    .or_else(|_| self.expect(TokenKind::KeyWord(KeyWord::DeclareAlt)))?;
                let expr = self.parse_expression(0)?;
                self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                StmtKind::Declare {
                    lhs: var,
                    rhs: Box::new(expr),
                }
            }
            TokenKind::KeyWord(KeyWord::IfCond) => {
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
                self.expect(TokenKind::KeyWord(KeyWord::LeftBrace))?;
                let mut statements = Vec::new();
                let mut else_statements = Vec::new();

                while let Some(token) = self.peek()
                    && token != &TokenKind::KeyWord(KeyWord::RightBrace)
                {
                    let stmt = self.parse_statement()?;
                    statements.push(stmt);
                }
                self.consume(); // consume the R_BRACE

                match self.consume() {
                    Some(TokenKind::KeyWord(KeyWord::EndBlock)) => {
                        self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                    }
                    Some(TokenKind::KeyWord(KeyWord::ElseCond)) => {
                        self.expect(TokenKind::KeyWord(KeyWord::LeftBrace))?;
                        while let Some(token) = self.peek()
                            && token != &TokenKind::KeyWord(KeyWord::RightBrace)
                        {
                            let stmt = self.parse_statement()?;
                            else_statements.push(stmt);
                        }
                        self.expect(TokenKind::KeyWord(KeyWord::RightBrace))?;
                        self.expect(TokenKind::KeyWord(KeyWord::EndBlock))?;
                        self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                    }
                    tok => {
                        return Err(Self::missing_expected_token(
                            TokenKind::KeyWord(KeyWord::EndBlock),
                            tok.unwrap_or_default().clone(),
                        ));
                    }
                }
                StmtKind::IfCond {
                    condition: Box::new(expr),
                    body: Box::new(statements),
                    else_body: Box::new(else_statements),
                }
            }
            TokenKind::KeyWord(KeyWord::ForStart) => {
                // FOR_START forvar FOR_RANGE_START forvar FOR_RANGE_END L_BRACE statements R_BRACE END_BLOCK SEMI_COLON"
                // forvar := NUMBER | WORD
                self.consume();
                let for_start = match self.consume().unwrap_or_default() {
                    TokenKind::Ident(ident) => ForVar::Ident(ident),
                    TokenKind::Literal(Literal::Int(num)) => ForVar::Int(*num),
                    TokenKind::Literal(Literal::Float(num)) => ForVar::Float(*num),
                    tok => {
                        return Err(Self::missing_expected_token(
                            TokenKind::KeyWord(KeyWord::Assign),
                            tok.clone(),
                        ));
                    }
                };
                self.expect(TokenKind::KeyWord(KeyWord::ForRangeStart))?;
                let for_end = match self.consume().unwrap_or_default() {
                    TokenKind::Ident(ident) => ForVar::Ident(ident),
                    TokenKind::Literal(Literal::Int(num)) => ForVar::Int(*num),
                    TokenKind::Literal(Literal::Float(num)) => ForVar::Float(*num),
                    tok => {
                        return Err(Self::missing_expected_token(
                            TokenKind::KeyWord(KeyWord::Assign),
                            tok.clone(),
                        ));
                    }
                };
                self.expect(TokenKind::KeyWord(KeyWord::ForRangeEnd))?;
                self.expect(TokenKind::KeyWord(KeyWord::LeftBrace))?;
                let mut statements = Vec::new();
                while let Some(token) = self.peek()
                    && token != &TokenKind::KeyWord(KeyWord::RightBrace)
                {
                    let stmt = self.parse_statement()?;
                    statements.push(stmt);
                }
                self.consume();
                self.expect(TokenKind::KeyWord(KeyWord::EndBlock))?;
                self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                StmtKind::ForLoop {
                    start: Box::new(for_start),
                    end: Box::new(for_end),
                    body: Box::new(statements),
                }
            }
            TokenKind::KeyWord(KeyWord::WhileLoop) => {
                // WHILE_LOOP expression L_BRACE statements R_BRACE END_BLOCK SEMI_COLON
                self.consume();
                let expr = self.parse_expression(0)?;
                match expr {
                    Expr::LogicalExpr { .. }
                    | Expr::ExprLeaf(ExprLeaf::BoolTrue)
                    | Expr::ExprLeaf(ExprLeaf::BoolFalse) => {}
                    _ => return Err(ParseError::InvalidExpr(expr)),
                }
                self.expect(TokenKind::KeyWord(KeyWord::LeftBrace))?;
                let mut statements = Vec::new();
                while let Some(token) = self.peek()
                    && token != &TokenKind::KeyWord(KeyWord::RightBrace)
                {
                    let stmt = self.parse_statement()?;
                    statements.push(stmt);
                }
                self.consume();
                self.expect(TokenKind::KeyWord(KeyWord::EndBlock))?;
                self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                StmtKind::WhileLoop {
                    condition: Box::new(expr),
                    body: Box::new(statements),
                }
            }
            TokenKind::Ident(_) => {
                // variable ASSIGN expression SEMI_COLON
                // variable FUNC_CALL func_name SEMI_COLON
                let TokenKind::Ident(ident) = self.consume().unwrap() else {
                    unreachable!()
                };
                match self.consume().unwrap_or_default() {
                    TokenKind::KeyWord(KeyWord::Assign) => {
                        let expr = self.parse_expression(0)?;
                        self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                        StmtKind::Assign {
                            lhs: &ident,
                            rhs: Box::new(expr),
                        }
                    }
                    TokenKind::KeyWord(KeyWord::FuncCall) => {
                        let next_tok = self.consume().unwrap_or_default();
                        if let TokenKind::Ident(func_name) = next_tok {
                            self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                            StmtKind::AssignFuncCall {
                                var_name: &ident,
                                func_name,
                            }
                        } else {
                            return Err(Self::missing_expected_token(
                                TokenKind::KeyWord(KeyWord::Assign),
                                next_tok.clone(),
                            ));
                        }
                    }
                    peek_tok => {
                        return Err(Self::missing_expected_token(
                            TokenKind::KeyWord(KeyWord::Assign),
                            peek_tok.clone(),
                        ));
                    }
                }
            }
            _ => {
                // expression SEMI_COLON
                let expr = self.parse_expression(0)?;
                self.expect(TokenKind::KeyWord(KeyWord::SemiColon))?;
                StmtKind::Expr(expr)
            }
        };
        Ok(stmtkind)
    }

    fn parse_expression(&self, precedence_limit: u32) -> Result<Expr<'a>, ParseError<'a>> {
        // Pratt parsing
        // https://martin.janiczek.cz/2023/07/03/demystifying-pratt-parsers.html
        // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
        // https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy
        let mut lhs = match self.consume().unwrap_or_default() {
            TokenKind::Literal(literal) => Expr::ExprLeaf(ExprLeaf::from_literal(&literal)),
            TokenKind::Ident(ident) => Expr::Ident(ident),
            op @ TokenKind::KeyWord(KeyWord::Sub) | op @ TokenKind::KeyWord(KeyWord::Sum) => {
                let op = BinaryOp::from_token(&op).unwrap();
                Expr::UnaryExpr {
                    op,
                    child: Box::new(self.parse_expression(0)?),
                }
            }
            tok => return Err(ParseError::UnexpectedToken(tok.clone())),
        };

        while let Some(op @ TokenKind::KeyWord(_)) = self.peek() {
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
            // Consume the operator iff precedence <= precedence_limit
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

    use super::Parser;
    use crate::lexer::{Lexer, TokenKind};

    #[test]
    fn test_parser() -> Result<()> {
        let program = include_str!("../testdata/snapshots/test.rpp");

        let tokens = Lexer::tokenize_str(program)?;
        let token_kinds = tokens
            .into_iter()
            .map(|tok| tok.kind)
            .collect::<Vec<TokenKind>>();

        let parser = Parser::new(&token_kinds);
        let ast = parser.parse().unwrap();

        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_path("../testdata/output/");
        settings.bind(|| {
            insta::assert_snapshot!(format!("{ast:#?}"));
        });
        Ok(())
    }
}
