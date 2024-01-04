use color_eyre::eyre::Result;
use itertools::Itertools;

use crate::parser::{Function, Program, StmtKind};

pub(crate) enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}

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
            self.variables[idx] = value;
            return Ok(());
        }
        // TODO: throw proper error
        Err(())
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
}

pub(crate) struct Interpreter {
    environment: Environment,
}

impl Visitor for Interpreter {
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
}

pub(crate) trait Visitable {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<()>;
}

impl<T: Visitable> Visitable for Vec<T> {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<()> {
        for elem in self {
            elem.visit(v)?;
        }
        Ok(())
    }
}

impl Visitable for Function {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<()> {
        v.visit_function(self)?;
        Ok(())
    }
}

impl Visitable for StmtKind {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<()> {
        v.visit_stmt(self)?;
        Ok(())
    }
}

impl Visitable for Program {
    fn visit(&mut self, v: &mut dyn Visitor) -> Result<()> {
        for function in &mut self.functions {
            v.register_function(function)?;
        }
        self.main_stmts.visit(v)?;
        Ok(())
    }
}
