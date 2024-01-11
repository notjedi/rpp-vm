use color_eyre::eyre::{eyre, Result};
use itertools::Itertools;
use std::{
    cmp::Ordering,
    fmt::{Debug, Display, Write},
    ops::{Add, Div, Mul, Rem, Sub},
};
use thiserror::Error;

use crate::parser::{BinaryOp, Expr, ExprLeaf, Function, LogicalOp, Program, StmtKind};

type BoxStr = Box<String>;

#[derive(Debug, Error)]
pub(crate) enum RuntimeError {
    #[error("variable not declared: {0:?}")]
    VariableNotDeclared(BoxStr),
    #[error("function not declared: {0:?}")]
    FunctionNotDeclared(BoxStr),
    #[error("operations on {0} and {1} are not supported")]
    TypeError(Value, Value),
    #[error("usage of `break` outside loop")]
    BreakOutsideLoop,
    #[error("division by zero")]
    DivisionByZero,
    #[error("bad operand for unary op: {0}")]
    BadOperandforUnaryOp(Value),
}

#[derive(Clone, Debug)]
pub(crate) enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    Str(Box<String>),
}

impl Display for Value {
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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Some(Ordering::Equal),
            (Value::Int(lhs), Value::Int(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Char(lhs), Value::Char(rhs)) => lhs.partial_cmp(rhs),
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs.partial_cmp(rhs),
            // (Value::Str(lhs), Value::Str(rhs)) => lhs.partial_cmp(rhs),
            (Value::Int(lhs), Value::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Int(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            _ => None,
        }
    }
}

impl PartialEq for Value {
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

macro_rules! impl_bin_ops {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl $op_trait<Value> for Value {
            type Output = Value;

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

        impl $op_trait<&Value> for Value {
            type Output = Value;

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

        impl $op_trait<Value> for &Value {
            type Output = Value;

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

        impl $op_trait<&Value> for &Value {
            type Output = Value;

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

impl_bin_ops!(Add, add, +);
impl_bin_ops!(Sub, sub, -);
impl_bin_ops!(Mul, mul, *);
impl_bin_ops!(Div, div, /);
impl_bin_ops!(Rem, rem, %);

#[derive(Debug, PartialEq)]
pub(crate) enum ControlFlow {
    // NOTE: easy to add support of `continue` in the future
    Nop,
    Break,
    Return(Value),
}

#[derive(Debug)]
pub(crate) struct Variable {
    name: String,
    value: Value,
}

impl Variable {
    fn new(name: &str, value: Value) -> Self {
        Self {
            name: name.to_string(),
            value,
        }
    }
}

#[derive(Debug)]
pub(crate) struct Environment {
    functions: Vec<Function>,
    variables: Vec<Variable>,
    scopes: Vec<usize>,
}

impl Environment {
    pub(crate) fn new() -> Self {
        Self {
            functions: vec![],
            variables: vec![],
            scopes: vec![],
        }
    }

    pub(crate) fn start_scope(&mut self) {
        self.scopes.push(self.variables.len())
    }

    pub(crate) fn end_scope(&mut self) {
        let num_vars = self.variables.len()
            - self
                .scopes
                .pop()
                .expect("calling end_scope without calling start_scope");
        (0..num_vars).for_each(|_| {
            self.variables.pop();
        });
    }

    pub(crate) fn register_function(&mut self, func: &Function) {
        self.functions.push(func.clone())
    }

    pub(crate) fn register_variable(&mut self, name: &str, value: Value) {
        let var = Variable::new(name, value);
        self.variables.push(var);
    }

    pub(crate) fn update_variable(&mut self, name: &str, value: Value) -> Result<()> {
        if let Some(idx) = self.get_idx_of_var(name) {
            self.variables[idx].value = value;
            return Ok(());
        }
        Err(eyre!(RuntimeError::VariableNotDeclared(Box::new(
            name.to_string()
        ))))
    }

    fn get_idx_of_func(&mut self, name: &str) -> Option<usize> {
        self.functions
            .iter()
            .rev()
            .find_position(|func| *func.name == name)
            .map(|(idx, _)| idx)
    }

    fn get_idx_of_var(&mut self, name: &str) -> Option<usize> {
        self.variables
            .iter()
            .rev()
            .find_position(|var| &var.name == name)
            .map(|(idx, _)| idx)
    }

    pub(crate) fn get_val_of_var(&mut self, name: &str) -> Option<Value> {
        if let Some(idx) = self.get_idx_of_var(name) {
            return Some(self.variables[idx].value.clone());
        }
        None
    }

    fn get_func(&mut self, name: &str) -> Option<&Function> {
        if let Some(idx) = self.get_idx_of_func(name) {
            return Some(&mut self.functions[idx]);
        }
        None
    }
}

pub(crate) trait Visitor {
    fn register_function(&mut self, func: &Function) -> Result<()>;

    fn register_variable(&mut self, name: &str, value: Value) -> Result<()>;

    fn visit_function(&mut self, func: &Function) -> Result<Value>;

    fn visit_stmt(&mut self, stmt: &StmtKind) -> Result<ControlFlow>;

    fn visit_expr(&mut self, expr: &Expr) -> Result<Value>;

    fn visit_expr_leaf(&mut self, expr_leaf: &ExprLeaf) -> Result<Value>;
}

#[derive(Debug)]
pub(crate) struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub(crate) fn new() -> Self {
        Self {
            environment: Environment::new(),
        }
    }

    fn check_if_compatible(lhs: &Value, rhs: &Value) -> Result<()> {
        match (lhs, rhs) {
            (Value::Int(_), Value::Int(_)) => Ok(()),
            (Value::Int(_), Value::Float(_)) => Ok(()),
            (Value::Float(_), Value::Int(_)) => Ok(()),
            (Value::Float(_), Value::Float(_)) => Ok(()),
            (lhs, rhs) => Err(eyre!(RuntimeError::TypeError(lhs.clone(), rhs.clone()))),
        }
    }
}

impl Visitor for Interpreter {
    fn register_function(&mut self, func: &Function) -> Result<()> {
        self.environment.register_function(func);
        Ok(())
    }

    fn register_variable(&mut self, name: &str, value: Value) -> Result<()> {
        self.environment.register_variable(name, value);
        Ok(())
    }

    fn visit_function(&mut self, func: &Function) -> Result<Value> {
        if let Some(_) = self.environment.get_idx_of_func(&func.name) {
            self.environment.start_scope();
            let res = func.body.visit(self)?;
            self.environment.end_scope();
            match res {
                ControlFlow::Return(val) => return Ok(val),
                _ => return Ok(Value::Unit),
            }
        }
        Err(eyre!(RuntimeError::FunctionNotDeclared(func.name.clone())))
    }

    #[allow(unused_variables)]
    fn visit_stmt(&mut self, stmt: &StmtKind) -> Result<ControlFlow> {
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
                    write!(&mut print_str, "{} ", val)?;
                }
                // NOTE: prints \n as new line instead of raw \n literal
                println!("{}", &print_str.replace("\\n", "\n"));
                ControlFlow::Nop
            }
            StmtKind::FuncCall(func_name) => {
                // TODO: check
                if let Some(func) = self.environment.get_func(func_name) {
                    // TODO: any other way than cloning?
                    func.clone().visit(self)?;
                }
                ControlFlow::Nop
            }
            StmtKind::FuncReturn(_) => todo!(),
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
                // TODO: check
                if let Some(func) = self.environment.get_func(func_name) {
                    // TODO: any other way than cloning?
                    let res = func.clone().visit(self)?;
                    // TODO: check if var is already availabe and update it?
                    self.environment.register_variable(var_name, res);
                }
                ControlFlow::Nop
            }
            StmtKind::IfCond {
                condition,
                body,
                else_body,
            } => {
                // TODO: check
                let res = condition.visit(self)?;
                // TODO: take care of return statements here, func might return value conditionally
                if let Value::Bool(val) = res {
                    match val {
                        true => body.visit(self)?,
                        false => else_body.visit(self)?,
                    }
                } else {
                    ControlFlow::Nop
                }
            }
            StmtKind::ForLoop { start, end, body } => todo!(),
            StmtKind::WhileLoop { condition, body } => todo!(),
        };
        Ok(ctrl_flow)
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<Value> {
        let val = match expr {
            Expr::BinaryExpr { op, lhs, rhs } => {
                let lhs_val = lhs.visit(self)?;
                let rhs_val = rhs.visit(self)?;
                Self::check_if_compatible(&lhs_val, &rhs_val)?;
                if *op == BinaryOp::Div {
                    if let Value::Int(0) | Value::Float(0.0) = rhs_val {
                        return Err(eyre!(RuntimeError::DivisionByZero));
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
                        _ => return Err(eyre!(RuntimeError::BadOperandforUnaryOp(val.clone()))),
                    },
                    // would error in parse stage itself
                    _ => unreachable!(),
                }
            }
            Expr::ExprLeaf(expr_leaf) => self.visit_expr_leaf(expr_leaf)?,
            Expr::Ident(ident) => {
                let val = self.environment.get_val_of_var(&ident);
                val.ok_or_else(|| RuntimeError::VariableNotDeclared(ident.clone()))?
            }
        };
        Ok(val)
    }

    fn visit_expr_leaf(&mut self, expr_leaf: &ExprLeaf) -> Result<Value> {
        let val = match expr_leaf {
            ExprLeaf::Char(ch) => Value::Char(*ch),
            ExprLeaf::BoolTrue => Value::Bool(true),
            ExprLeaf::BoolFalse => Value::Bool(false),
            ExprLeaf::Int(int_val) => Value::Int(*int_val),
            ExprLeaf::Float(float_val) => Value::Float(*float_val),
            ExprLeaf::Str(expr_string) => Value::Str(expr_string.clone()),
        };
        Ok(val)
    }
}

pub(crate) trait Visitable<T> {
    fn visit(&self, v: &mut dyn Visitor) -> Result<T>;
}

impl<T: Visitable<()>> Visitable<()> for Vec<T> {
    fn visit(&self, v: &mut dyn Visitor) -> Result<()> {
        for elem in self {
            elem.visit(v)?;
        }
        Ok(())
    }
}

impl<T: Visitable<ControlFlow>> Visitable<ControlFlow> for Vec<T> {
    fn visit(&self, v: &mut dyn Visitor) -> Result<ControlFlow> {
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

impl Visitable<Value> for Function {
    fn visit(&self, v: &mut dyn Visitor) -> Result<Value> {
        v.visit_function(self)
    }
}

impl Visitable<()> for Program {
    fn visit(&self, v: &mut dyn Visitor) -> Result<()> {
        for function in &self.functions {
            v.register_function(function)?;
        }
        for stmt in self.main_stmts.iter() {
            let stmt_ret = stmt.visit(v)?;
            // NOTE: `break` statements should not reach here, it should be caught inside a loop
            if stmt_ret == ControlFlow::Break {
                return Err(eyre!(RuntimeError::BreakOutsideLoop));
            }
        }
        Ok(())
    }
}

impl Visitable<ControlFlow> for StmtKind {
    fn visit(&self, v: &mut dyn Visitor) -> Result<ControlFlow> {
        v.visit_stmt(self)
    }
}

impl Visitable<Value> for Expr {
    fn visit(&self, v: &mut dyn Visitor) -> Result<Value> {
        v.visit_expr(self)
    }
}

impl Visitable<Value> for ExprLeaf {
    fn visit(&self, v: &mut dyn Visitor) -> Result<Value> {
        v.visit_expr_leaf(self)
    }
}
