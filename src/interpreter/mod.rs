use color_eyre::eyre::Result;

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

pub(crate) struct Environment {
    variables: Vec<Variable>,
    scopes: Vec<usize>,
}

impl Environment {
    pub(crate) fn new() -> Self {
        Self {
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
        for _ in 0..num_vars {
            self.variables.pop();
        }
    }
}

pub(crate) struct Interpreter {
    environment: Environment,
}

pub(crate) trait Visitor {
    fn register_function(&mut self, func: &mut Function) -> Result<()> {
        Ok(())
    }

    fn register_variable(&mut self, name: &mut str, value: Value) -> Result<()> {
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
