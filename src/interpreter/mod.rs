use color_eyre::eyre::{eyre, Result};
use itertools::Itertools;
use std::fmt::{Debug, Display, Write};

use crate::parser::{Expr, ExprLeaf, Function, Program, StmtKind};

#[derive(Debug)]
pub(crate) enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Float(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Bool(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Str(arg0) => f.write_fmt(format_args!("{}", arg0)),
        }
    }
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
        // TODO: do not unwrap
        let num_vars = self.variables.len() - self.scopes.pop().unwrap();
        (0..num_vars).for_each(|_| {
            self.variables.pop();
        });
    }

    pub(crate) fn register_function(&mut self, func: &mut Function) {
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
        Err(eyre!("Cannot find variable with name {name}"))
    }

    pub(crate) fn get_idx_of_func(&mut self, name: &str) -> Option<usize> {
        self.functions
            .iter()
            .find_position(|func| *func.name == name)
            .map(|(idx, _)| idx)
    }

    pub(crate) fn get_idx_of_var(&mut self, name: &str) -> Option<usize> {
        self.variables
            .iter()
            .find_position(|var| &var.name == name)
            .map(|(idx, _)| idx)
    }
}

pub(crate) trait Visitor {
    fn register_function(&mut self, func: &mut Function) -> Result<()> {
        Ok(())
    }

    fn register_variable(&mut self, name: &str, value: Value) -> Result<()> {
        Ok(())
    }

    fn visit_function(&mut self, func: &mut Function) -> Result<()> {
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &mut StmtKind) -> Result<()> {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<Value>;

    fn visit_expr_leaf(&mut self, expr_leaf: &mut ExprLeaf) -> Result<Value>;
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
}

impl Visitor for Interpreter {
    fn register_function(&mut self, func: &mut Function) -> Result<()> {
        self.environment.register_function(func);
        Ok(())
    }

    fn register_variable(&mut self, name: &str, value: Value) -> Result<()> {
        self.environment.register_variable(name, value);
        Ok(())
    }

    fn visit_function(&mut self, func: &mut Function) -> Result<()> {
        if let Some(idx) = self.environment.get_idx_of_func(&func.name) {
            self.environment.start_scope();
            func.body.visit(self)?;
            self.environment.end_scope();
            return Ok(());
        }
        Err(eyre!("can't find function in scope"))
    }

    fn visit_stmt(&mut self, stmt: &mut StmtKind) -> Result<()> {
        match stmt {
            StmtKind::BreakLoop => todo!(),
            StmtKind::Expr(_) => todo!(),
            StmtKind::Comment(_) => {}
            StmtKind::Print(exprs) => {
                let mut print_str = String::new();
                for expr in exprs {
                    let val = expr.visit(self)?;
                    write!(&mut print_str, "{}", val)?;
                }
                println!("{}", &print_str);
            }
            StmtKind::FuncCall(_) => todo!(),
            StmtKind::FuncReturn(_) => todo!(),
            StmtKind::Assign { lhs, rhs } => todo!(),
            StmtKind::AssignFuncCall { lhs, rhs } => todo!(),
            StmtKind::IfCond {
                condition,
                body,
                else_body,
            } => todo!(),
            StmtKind::ForLoop { start, end, body } => todo!(),
            StmtKind::WhileLoop { condition, body } => todo!(),
        }
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Result<Value> {
        let val = match expr {
            Expr::BinaryExpr { op, lhs, rhs } => todo!(),
            Expr::LogicalExpr { op, lhs, rhs } => todo!(),
            Expr::UnaryExpr { op, child } => todo!(),
            Expr::ExprLeaf(expr_leaf) => self.visit_expr_leaf(expr_leaf)?,
            Expr::Ident(_) => todo!(),
        };
        Ok(val)
    }

    fn visit_expr_leaf(&mut self, expr_leaf: &mut ExprLeaf) -> Result<Value> {
        let val = match expr_leaf {
            ExprLeaf::BoolTrue => todo!(),
            ExprLeaf::BoolFalse => todo!(),
            ExprLeaf::Int(_) => todo!(),
            ExprLeaf::Float(_) => todo!(),
            ExprLeaf::Char(_) => todo!(),
            ExprLeaf::Str(expr_string) => Value::Str(*expr_string.clone()),
        };
        Ok(val)
    }
}

pub(crate) trait Visitable<T> {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<T>;
}

impl<T: Visitable<()>> Visitable<()> for Vec<T> {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<()> {
        for elem in self {
            elem.visit(v)?;
        }
        Ok(())
    }
}

impl Visitable<()> for Function {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<()> {
        v.visit_function(self)?;
        Ok(())
    }
}

impl Visitable<()> for Program {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<()> {
        for function in &mut self.functions {
            v.register_function(function)?;
        }
        self.main_stmts.visit(v)?;
        Ok(())
    }
}

impl Visitable<()> for StmtKind {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<()> {
        v.visit_stmt(self)?;
        Ok(())
    }
}

impl Visitable<Value> for Expr {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<Value> {
        v.visit_expr(self)
    }
}

impl Visitable<Value> for ExprLeaf {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<Value> {
        v.visit_expr_leaf(self)
    }
}
