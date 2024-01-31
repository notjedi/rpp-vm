use crate::{
    interpreter::Value,
    parser::{BinaryOp, Expr, ExprLeaf, LogicalOp, Program, StmtKind},
};

#[derive(Clone, Debug)]
pub(crate) enum Instruction {
    Add,
    Constant(usize),
    Divide,
    Equal,
    False,
    GetLocal(usize),
    Greater,
    Jump(u16),
    JumpIfFalse(u16),
    Less,
    Loop(u16),
    Method(usize),
    Mod,
    Multiply,
    Not,
    Pop,
    Print,
    Return,
    SetLocal(usize),
    Substract,
    True,
}

#[derive(Clone, Debug)]
pub(crate) struct CompiledProgram {
    pub(crate) constants: Vec<Value>,
    pub(crate) instructions: Vec<Instruction>,
}

impl CompiledProgram {
    pub(crate) fn new() -> Self {
        Self {
            constants: vec![],
            instructions: vec![],
        }
    }

    pub(crate) fn write_instruction(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    pub(crate) fn push_constant(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ProgFunction {
    name: Box<String>,
    program: Vec<Instruction>,
}

impl ProgFunction {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            name: Box::new(name.to_string()),
            program: vec![],
        }
    }
}

#[derive(Debug)]
pub(crate) struct Local {
    name: Box<String>,
    depth: usize,
}

pub(crate) struct Compiler {
    bytecode_program: CompiledProgram,
    locals: Vec<Local>,
    scope_depth: usize,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            bytecode_program: CompiledProgram::new(),
            locals: vec![],
            scope_depth: 0,
        }
    }

    pub(crate) fn compile_program(mut self, program: &Program) -> CompiledProgram {
        for stmt in program.main_stmts.iter() {
            self.eval_stmt(stmt);
            if let StmtKind::Expr(_) = stmt {
                self.bytecode_program.write_instruction(Instruction::Pop);
            }
        }
        self.bytecode_program
    }

    fn get_idx_of_func(&self, func_name: &str) -> Option<usize> {
        for (i, val) in self.bytecode_program.constants.iter().enumerate() {
            if let Value::ProgFunction(func) = val
                && *func.name == func_name
            {
                return Some(i);
            }
        }
        None
    }

    fn get_idx_of_local(&self, name: &str) -> Option<usize> {
        for (idx, local) in self.locals.iter().rev().enumerate() {
            if *local.name == name {
                // TODO: get the idx right
                return Some(self.locals.len() - idx - 1);
            }
        }
        None
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        let mut locals_iter = self.locals.iter_mut().rev();
        while let Some(local) = locals_iter.next()
            && local.depth > self.scope_depth
        {
            self.bytecode_program.write_instruction(Instruction::Pop);
        }
    }

    fn eval_stmt(&mut self, stmt: &StmtKind) {
        match stmt {
            StmtKind::BreakLoop => todo!(),
            StmtKind::Expr(expr) => {
                self.eval_expr(expr);
            }
            StmtKind::Comment(_) => {}
            StmtKind::Print(exprs) => {
                // TODO: add support for Add instr for strings
                let mut expr_iter = exprs.iter();
                if let Some(expr) = expr_iter.next() {
                    self.eval_expr(expr);
                }
                for expr in expr_iter {
                    self.eval_expr(expr);
                    self.bytecode_program.write_instruction(Instruction::Add);
                }
                self.bytecode_program.write_instruction(Instruction::Print);
            }
            StmtKind::FuncCall(func_name) => match self.get_idx_of_func(func_name) {
                Some(idx) => {
                    self.bytecode_program
                        .write_instruction(Instruction::Method(idx));
                }
                None => todo!("return compile time error"),
            },
            StmtKind::FuncReturn(_) => todo!(),
            StmtKind::Declare { lhs, rhs } => {
                self.eval_expr(rhs);
                self.locals.push(Local {
                    name: lhs.clone(),
                    depth: self.scope_depth,
                });
                self.bytecode_program
                    .write_instruction(Instruction::SetLocal(self.locals.len() - 1));
            }
            StmtKind::Assign { lhs, rhs } => {
                self.eval_expr(rhs);
                if let Some(idx) = self.get_idx_of_local(lhs) {
                    self.bytecode_program
                        .write_instruction(Instruction::SetLocal(idx));
                } else {
                    // TODO: return error
                    todo!("return compile time error")
                }
            }
            StmtKind::AssignFuncCall {
                var_name,
                func_name,
            } => todo!(),
            StmtKind::IfCond {
                condition,
                body,
                else_body,
            } => todo!(),
            StmtKind::ForLoop { start, end, body } => todo!(),
            StmtKind::WhileLoop { condition, body } => todo!(),
        }
    }

    fn eval_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::BinaryExpr { op, lhs, rhs } => {
                self.eval_expr(lhs);
                self.eval_expr(rhs);
                let instr = match op {
                    BinaryOp::Add => Instruction::Add,
                    BinaryOp::Sub => Instruction::Substract,
                    BinaryOp::Mul => Instruction::Multiply,
                    BinaryOp::Div => Instruction::Divide,
                    BinaryOp::Mod => Instruction::Mod,
                };
                self.bytecode_program.write_instruction(instr);
            }
            Expr::LogicalExpr { op, lhs, rhs } => {
                self.eval_expr(lhs);
                self.eval_expr(rhs);
                match op {
                    LogicalOp::GreaterThan => {
                        self.bytecode_program
                            .write_instruction(Instruction::Greater);
                    }
                    LogicalOp::LessThan => {
                        self.bytecode_program.write_instruction(Instruction::Less);
                    }
                    LogicalOp::GreaterThanEqual => {
                        self.bytecode_program.write_instruction(Instruction::Less);
                        self.bytecode_program.write_instruction(Instruction::Not);
                    }
                    LogicalOp::LessThanEqual => {
                        self.bytecode_program
                            .write_instruction(Instruction::Greater);
                        self.bytecode_program.write_instruction(Instruction::Not);
                    }
                    LogicalOp::Equal => {
                        self.bytecode_program.write_instruction(Instruction::Equal);
                    }
                    LogicalOp::NotEqual => {
                        self.bytecode_program.write_instruction(Instruction::Not);
                    }
                };
            }
            Expr::UnaryExpr { op, child } => {
                self.eval_expr(child);
                let instr = match op {
                    BinaryOp::Add => Instruction::Add,
                    BinaryOp::Sub => Instruction::Substract,
                    BinaryOp::Mul => Instruction::Multiply,
                    BinaryOp::Div => Instruction::Divide,
                    BinaryOp::Mod => Instruction::Mod,
                };
                self.bytecode_program.write_instruction(instr);
            }
            Expr::ExprLeaf(expr_leaf) => {
                let val = ExprLeaf::to_value(expr_leaf);
                let idx = self.bytecode_program.push_constant(val);
                self.bytecode_program
                    .write_instruction(Instruction::Constant(idx));
            }
            Expr::Ident(name) => match self.get_idx_of_local(name) {
                Some(idx) => self
                    .bytecode_program
                    .write_instruction(Instruction::GetLocal(idx)),
                None => todo!("return compile time error"),
            },
        }
    }
}
