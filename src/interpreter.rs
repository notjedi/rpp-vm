use color_eyre::eyre::Result;
use itertools::Itertools;
use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display, Write},
    ops::{Add, Div, Mul, Rem, Sub},
};
use thiserror::Error;

use crate::parser::{
    BinaryOp, Expr, ExprLeaf, ForVar, Function, LogicalOp, Program, StmtKind, Value,
};

#[derive(Debug, Error)]
pub(crate) enum RuntimeError<'a> {
    #[error("variable not declared: {0:?}")]
    VariableNotDeclared(String),
    #[error("function not declared: {0:?}")]
    FunctionNotDeclared(String),
    #[error("operations on {0} and {1} are not supported")]
    TypeError(Value<'a>, Value<'a>),
    #[error("usage of `break` outside loop")]
    BreakOutsideLoop,
    #[error("division by zero")]
    DivisionByZero,
    #[error("bad operand for unary op: {0}")]
    BadOperandforUnaryOp(Value<'a>),
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => f.write_fmt(format_args!("unit")),
            Self::Str(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Int(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Bool(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Char(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Float(arg0) => f.write_fmt(format_args!("{}", arg0)),
        }
    }
}

impl<'a> PartialOrd for Value<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Some(Ordering::Equal),
            (Value::Int(lhs), Value::Int(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Char(lhs), Value::Char(rhs)) => lhs.partial_cmp(rhs),
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs.partial_cmp(rhs),
            (Value::Int(lhs), Value::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Int(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            _ => None,
        }
    }
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Unit, Value::Unit) => true,
            (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Str(lhs), Value::Str(rhs)) => lhs == rhs,
            (Value::Int(lhs), Value::Float(rhs)) => &(*lhs as f64) == rhs,
            (Value::Float(lhs), Value::Int(rhs)) => lhs == &(*rhs as f64),
            _ => false,
        }
    }
}

impl<'a> Add<Value<'a>> for Value<'a> {
    type Output = Value<'a>;

    fn add(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 + rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs + rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            (Value::Str(lhs), rhs) => {
                Value::Str(Cow::Owned(lhs.to_string() + " " + &rhs.to_string()))
            }
            (lhs, Value::Str(rhs)) => Value::Str(Cow::Owned(lhs.to_string() + " " + &rhs)),
            _ => unreachable!(),
        }
    }
}

impl<'a> Add<&Value<'a>> for Value<'a> {
    type Output = Value<'a>;

    fn add(self, rhs: &Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + *rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 + *rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs + *rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + *rhs),
            (Value::Str(lhs), rhs) => {
                Value::Str(Cow::Owned(lhs.to_string() + " " + &rhs.to_string()))
            }
            (lhs, Value::Str(rhs)) => Value::Str(Cow::Owned(lhs.to_string() + " " + &rhs)),
            _ => unreachable!(),
        }
    }
}

impl<'a> Add<Value<'a>> for &Value<'a> {
    type Output = Value<'a>;

    fn add(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(*lhs + rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(*lhs as f64 + rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(*lhs + rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(*lhs + rhs),
            (Value::Str(lhs), rhs) => {
                Value::Str(Cow::Owned(lhs.to_string() + " " + &rhs.to_string()))
            }
            (lhs, Value::Str(rhs)) => Value::Str(Cow::Owned(lhs.to_string() + " " + &rhs)),
            _ => unreachable!(),
        }
    }
}

impl<'a> Add<&Value<'a>> for &Value<'a> {
    type Output = Value<'a>;

    fn add(self, rhs: &Value) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Value::Int(*lhs + *rhs),
            (Value::Int(lhs), Value::Float(rhs)) => Value::Float(*lhs as f64 + *rhs),
            (Value::Float(lhs), Value::Int(rhs)) => Value::Float(*lhs + *rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(*lhs + *rhs),
            (Value::Str(lhs), rhs) => {
                Value::Str(Cow::Owned(lhs.to_string() + " " + &rhs.to_string()))
            }
            (lhs, Value::Str(rhs)) => Value::Str(Cow::Owned(lhs.to_string() + " " + &rhs)),
            _ => unreachable!(),
        }
    }
}

macro_rules! impl_bin_ops {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl<'a> $op_trait<Value<'a>> for Value<'a> {
            type Output = Value<'a>;

            fn $op_fn(self, rhs: Value) -> Self::Output {
                match (self, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs $op rhs),
                    (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 $op rhs),
                    (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs $op rhs as f64),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs $op rhs),
                    _ => unreachable!(),
                }
            }
        }

        impl<'a> $op_trait<&Value<'a>> for Value<'a> {
            type Output = Value<'a>;

            fn $op_fn(self, rhs: &Value) -> Self::Output {
                match (self, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs $op *rhs),
                    (Value::Int(lhs), Value::Float(rhs)) => Value::Float(lhs as f64 $op *rhs),
                    (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs $op *rhs as f64),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs $op *rhs),
                    _ => unreachable!(),
                }
            }
        }

        impl<'a> $op_trait<Value<'a>> for &Value<'a> {
            type Output = Value<'a>;

            fn $op_fn(self, rhs: Value) -> Self::Output {
                match (self, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Value::Int(*lhs $op rhs),
                    (Value::Int(lhs), Value::Float(rhs)) => Value::Float(*lhs as f64 $op rhs),
                    (Value::Float(lhs), Value::Int(rhs)) => Value::Float(*lhs $op rhs as f64),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(*lhs $op rhs),
                    _ => unreachable!(),
                }
            }
        }

        impl<'a> $op_trait<&Value<'a>> for &Value<'a> {
            type Output = Value<'a>;

            fn $op_fn(self, rhs: &Value) -> Self::Output {
                match (self, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Value::Int(*lhs $op *rhs),
                    (Value::Int(lhs), Value::Float(rhs)) => Value::Float(*lhs as f64 $op *rhs),
                    (Value::Float(lhs), Value::Int(rhs)) => Value::Float(*lhs $op *rhs as f64),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(*lhs $op *rhs),
                    _ => unreachable!(),
                }
            }
        }
    };
}

pub(crate) fn forvar_as_int(var: &ForVar, env: &Environment) -> Option<i64> {
    match var {
        ForVar::Int(val) => Some(*val),
        ForVar::Float(_) => None,
        ForVar::Ident(var_name) => {
            if let Some(Value::Int(val)) = env.get_val_of_var(var_name) {
                Some(*val)
            } else {
                None
            }
        }
    }
}

pub(crate) fn forvar_diff(start: &ForVar, end: &ForVar, env: &Environment) -> i64 {
    match (forvar_as_int(start, env), forvar_as_int(end, env)) {
        (Some(start), Some(end)) => end - start,
        _ => 0,
    }
}

// impl_bin_ops!(Add, add, +);
impl_bin_ops!(Sub, sub, -);
impl_bin_ops!(Mul, mul, *);
impl_bin_ops!(Div, div, /);
impl_bin_ops!(Rem, rem, %);

#[derive(Debug, PartialEq)]
pub(crate) enum ControlFlow<'a> {
    // NOTE: easy to add support of `continue` in the future
    Nop,
    Break,
    Return(Value<'a>),
}

#[derive(Debug)]
pub(crate) struct Scope<'a> {
    variables: HashMap<String, Value<'a>>,
}

impl<'a> Scope<'a> {
    pub(crate) fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Environment<'a> {
    functions: Vec<&'a Function<'a>>,
    scopes: Vec<Scope<'a>>,
}

impl<'a> Environment<'a> {
    pub(crate) fn new() -> Self {
        Self {
            functions: vec![],
            scopes: vec![Scope::new()],
        }
    }

    fn get_idx_of_func(&self, name: &str) -> Option<usize> {
        self.functions
            .iter()
            .rev()
            .find_position(|func| *func.name == *name)
            .map(|(idx, _)| idx)
    }

    fn get_func(&self, name: &str) -> Option<&'a Function<'a>> {
        if let Some(idx) = self.get_idx_of_func(name) {
            return Some(&self.functions[idx]);
        }
        None
    }

    pub(crate) fn start_scope(&mut self) {
        self.scopes.push(Scope::new())
    }

    pub(crate) fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub(crate) fn register_function(&mut self, func: &'a Function<'a>) {
        self.functions.push(func)
    }

    pub(crate) fn register_variable(&mut self, name: &str, value: Value<'a>) {
        self.scopes
            .last_mut()
            .unwrap()
            .variables
            .insert(name.to_string(), value);
    }

    pub(crate) fn update_variable(
        &mut self,
        name: &str,
        value: Value<'a>,
    ) -> Result<(), RuntimeError<'a>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.variables.get_mut(name) {
                *val = value;
                return Ok(());
            }
        }
        Err(RuntimeError::VariableNotDeclared(name.to_string()))
    }

    // TODO: this is now how we should be getting the value?
    // functions shouldn't have the ability to access global vars defined in main
    // only sub-scopes we have are if, else, for and while statements
    pub(crate) fn get_val_of_var(&self, name: &str) -> Option<&Value<'a>> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.variables.get(name) {
                return Some(val);
            }
        }
        None
    }
}

pub(crate) trait Visitor<'a> {
    fn register_function(&mut self, func: &'a Function<'a>) -> Result<(), RuntimeError<'a>>;

    fn register_variable(&mut self, name: &str, value: Value<'a>) -> Result<(), RuntimeError<'a>>;

    fn visit_function(&mut self, func: &'a Function<'a>) -> Result<Value<'a>, RuntimeError<'a>>;

    fn visit_stmt(&mut self, stmt: &'a StmtKind<'a>) -> Result<ControlFlow<'a>, RuntimeError<'a>>;

    fn visit_expr(&mut self, expr: &'a Expr<'a>) -> Result<Value<'a>, RuntimeError<'a>>;

    fn visit_expr_leaf(
        &mut self,
        expr_leaf: &'a ExprLeaf<'a>,
    ) -> Result<Value<'a>, RuntimeError<'a>>;
}

#[derive(Debug)]
pub(crate) struct Interpreter<'a> {
    environment: Environment<'a>,
}

impl<'a> Interpreter<'a> {
    pub(crate) fn new() -> Self {
        Self {
            environment: Environment::new(),
        }
    }

    fn check_if_compatible(lhs: &Value<'a>, rhs: &Value<'a>) -> Result<(), RuntimeError<'a>> {
        match (lhs, rhs) {
            (Value::Int(_), Value::Int(_)) => Ok(()),
            (Value::Int(_), Value::Float(_)) => Ok(()),
            (Value::Float(_), Value::Int(_)) => Ok(()),
            (Value::Float(_), Value::Float(_)) => Ok(()),
            (lhs, rhs) => Err(RuntimeError::TypeError(lhs.clone(), rhs.clone())),
        }
    }
}

impl<'a> Visitor<'a> for Interpreter<'a> {
    fn register_function(&mut self, func: &'a Function<'a>) -> Result<(), RuntimeError<'a>> {
        self.environment.register_function(func);
        Ok(())
    }

    fn register_variable(&mut self, name: &str, value: Value<'a>) -> Result<(), RuntimeError<'a>> {
        self.environment.register_variable(name, value);
        Ok(())
    }

    fn visit_function(&mut self, func: &'a Function<'a>) -> Result<Value<'a>, RuntimeError<'a>> {
        if self.environment.get_idx_of_func(&func.name).is_some() {
            self.environment.start_scope();
            let res = func.body.visit(self)?;
            self.environment.end_scope();
            match res {
                ControlFlow::Return(val) => return Ok(val),
                _ => return Ok(Value::Unit),
            }
        }
        Err(RuntimeError::FunctionNotDeclared(func.name.to_string()))
    }

    fn visit_stmt(&mut self, stmt: &'a StmtKind<'a>) -> Result<ControlFlow<'a>, RuntimeError<'a>> {
        let ctrl_flow = match stmt {
            StmtKind::BreakLoop => ControlFlow::Break,
            StmtKind::Expr(expr) => {
                let val = expr.visit(self)?;
                ControlFlow::Return(val)
            }
            StmtKind::Comment(_) => ControlFlow::Nop,
            StmtKind::Print(exprs) => {
                let mut print_str = String::new();
                for expr in exprs {
                    let val = expr.visit(self)?;
                    // TODO: how to use ? here
                    write!(&mut print_str, "{} ", val).expect("can't write to string buffer");
                }
                // NOTE: prints \n as new line instead of raw \n literal
                println!("{}", &print_str.replace("\\n", "\n"));
                ControlFlow::Nop
            }
            StmtKind::FuncCall(func_name) => {
                // TODO: start a new env here
                if let Some(func) = self.environment.get_func(func_name) {
                    // TODO: any other way than cloning?
                    func.visit(self)?;
                }
                ControlFlow::Nop
            }
            StmtKind::FuncReturn(expr) => {
                let ret_val = expr.visit(self)?;
                ControlFlow::Return(ret_val)
            }
            StmtKind::Declare { lhs, rhs } => {
                let val = rhs.visit(self)?;
                self.environment.register_variable(lhs, val);
                ControlFlow::Nop
            }
            StmtKind::Assign { lhs, rhs } => {
                let val = rhs.visit(self)?;
                self.environment.update_variable(lhs, val)?;
                ControlFlow::Nop
            }
            StmtKind::AssignFuncCall {
                var_name,
                func_name,
            } => {
                // TODO: start a new env here
                if let Some(func) = self.environment.get_func(func_name) {
                    // TODO: any other way than cloning?
                    let res = func.visit(self)?;
                    self.environment.register_variable(var_name, res);
                }
                ControlFlow::Nop
            }
            StmtKind::IfCond {
                condition,
                body,
                else_body,
            } => {
                // TODO: start a new scope here
                let res = condition.visit(self)?;
                if let Value::Bool(val) = res {
                    match val {
                        true => body.visit(self)?,
                        false => else_body.visit(self)?,
                    }
                } else {
                    ControlFlow::Nop
                }
            }
            StmtKind::ForLoop { start, end, body } => {
                let mut res = ControlFlow::Nop;
                let diff = forvar_diff(start, end, &self.environment);
                for _ in 0..diff {
                    res = body.visit(self)?;
                    match res {
                        ControlFlow::Nop => {}
                        ControlFlow::Break => return Ok(ControlFlow::Nop),
                        ControlFlow::Return(_) => return Ok(res),
                    }
                }
                res
            }
            StmtKind::WhileLoop { condition, body } => {
                let mut res = ControlFlow::Nop;
                while let Value::Bool(val) = condition.visit(self)?
                    && val
                {
                    res = body.visit(self)?;
                    match res {
                        ControlFlow::Nop => {}
                        ControlFlow::Break => return Ok(ControlFlow::Nop),
                        ControlFlow::Return(_) => return Ok(res),
                    }
                }
                res
            }
        };
        Ok(ctrl_flow)
    }

    fn visit_expr(&mut self, expr: &'a Expr) -> Result<Value<'a>, RuntimeError<'a>> {
        let val = match expr {
            Expr::BinaryExpr { op, lhs, rhs } => {
                let lhs_val = lhs.visit(self)?;
                let rhs_val = rhs.visit(self)?;
                Self::check_if_compatible(&lhs_val, &rhs_val)?;
                if *op == BinaryOp::Div {
                    if let Value::Int(0) | Value::Float(0.0) = rhs_val {
                        return Err(RuntimeError::DivisionByZero);
                    }
                }
                match op {
                    BinaryOp::Add => lhs_val + rhs_val,
                    BinaryOp::Sub => lhs_val - rhs_val,
                    BinaryOp::Mul => lhs_val * rhs_val,
                    BinaryOp::Div => lhs_val / rhs_val,
                    BinaryOp::Mod => lhs_val % rhs_val,
                }
            }
            Expr::LogicalExpr { op, lhs, rhs } => {
                let lhs_val = lhs.visit(self)?;
                let rhs_val = rhs.visit(self)?;
                let val = match op {
                    LogicalOp::GreaterThan => lhs_val > rhs_val,
                    LogicalOp::LessThan => lhs_val < rhs_val,
                    LogicalOp::GreaterThanEqual => lhs_val >= rhs_val,
                    LogicalOp::LessThanEqual => lhs_val <= rhs_val,
                    LogicalOp::Equal => lhs_val == rhs_val,
                    LogicalOp::NotEqual => lhs_val != rhs_val,
                };
                Value::Bool(val)
            }

            Expr::UnaryExpr { op, child } => {
                let val = child.visit(self)?;
                match op {
                    BinaryOp::Add => val,
                    BinaryOp::Sub => match val {
                        Value::Int(_) | Value::Float(_) => Value::Int(0) - val,
                        _ => return Err(RuntimeError::BadOperandforUnaryOp(val.clone())),
                    },
                    // would error in parse stage itself
                    _ => unreachable!(),
                }
            }
            Expr::ExprLeaf(expr_leaf) => self.visit_expr_leaf(expr_leaf)?,
            Expr::Ident(ident) => {
                let val = self.environment.get_val_of_var(ident).cloned();
                val.ok_or_else(|| RuntimeError::VariableNotDeclared(ident.to_string()))?
            }
        };
        Ok(val)
    }

    fn visit_expr_leaf(
        &mut self,
        expr_leaf: &'a ExprLeaf<'a>,
    ) -> Result<Value<'a>, RuntimeError<'a>> {
        let val = match expr_leaf {
            ExprLeaf::Char(ch) => Value::Char(*ch),
            ExprLeaf::BoolTrue => Value::Bool(true),
            ExprLeaf::BoolFalse => Value::Bool(false),
            ExprLeaf::Int(int_val) => Value::Int(*int_val),
            ExprLeaf::Float(float_val) => Value::Float(*float_val),
            ExprLeaf::Str(expr_string) => Value::Str(Cow::Borrowed(expr_string)),
        };
        Ok(val)
    }
}

pub(crate) trait Visitable<'ast> {
    type Output: 'ast;

    fn visit(&'ast self, v: &mut dyn Visitor<'ast>) -> Result<Self::Output, RuntimeError>;
}

impl<'ast> Visitable<'ast> for Vec<StmtKind<'ast>> {
    type Output = ControlFlow<'ast>;

    fn visit(&'ast self, v: &mut dyn Visitor<'ast>) -> Result<Self::Output, RuntimeError> {
        let mut res = ControlFlow::Nop;
        for elem in self {
            res = elem.visit(v)?;
            if res == ControlFlow::Break {
                return Ok(ControlFlow::Break);
            }
        }
        Ok(match res {
            ControlFlow::Return(_) => res,
            _ => ControlFlow::Nop,
        })
    }
}

impl<'ast> Visitable<'ast> for Function<'ast> {
    type Output = Value<'ast>;

    fn visit(&'ast self, v: &mut dyn Visitor<'ast>) -> Result<Self::Output, RuntimeError<'ast>> {
        v.visit_function(self)
    }
}

impl<'ast> Visitable<'ast> for Program<'ast> {
    type Output = ();

    fn visit(&'ast self, v: &mut dyn Visitor<'ast>) -> Result<Self::Output, RuntimeError> {
        for function in &self.functions {
            v.register_function(function)?;
        }
        for stmt in self.main_stmts.iter() {
            let stmt_ret = stmt.visit(v)?;
            // NOTE: `break` statements should not reach here, it should be caught inside a loop
            if stmt_ret == ControlFlow::Break {
                return Err(RuntimeError::BreakOutsideLoop);
            }
        }
        Ok(())
    }
}

impl<'ast> Visitable<'ast> for StmtKind<'ast> {
    type Output = ControlFlow<'ast>;

    fn visit(&'ast self, v: &mut dyn Visitor<'ast>) -> Result<Self::Output, RuntimeError> {
        v.visit_stmt(self)
    }
}

impl<'ast> Visitable<'ast> for Expr<'ast> {
    type Output = Value<'ast>;

    fn visit(&'ast self, v: &mut dyn Visitor<'ast>) -> Result<Self::Output, RuntimeError> {
        v.visit_expr(self)
    }
}

impl<'ast> Visitable<'ast> for ExprLeaf<'ast> {
    type Output = Value<'ast>;

    fn visit(&'ast self, v: &mut dyn Visitor<'ast>) -> Result<Self::Output, RuntimeError> {
        v.visit_expr_leaf(self)
    }
}
