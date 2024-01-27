use crate::{
    interpreter::Value,
    parser::{Expr, ExprLeaf, Program, StmtKind},
};

#[derive(Debug)]
pub(crate) enum Instruction {
    Add,
    Call(u8),
    Closure(u8),
    Constant(usize),
    Divide,
    Equal,
    False,
    GetLocal(u8),
    Greater,
    Invoke((u8, u8)),
    Jump(u16),
    JumpIfFalse(u16),
    Less,
    Loop(u16),
    Method(u8),
    Multiply,
    Negate,
    Not,
    Pop,
    Print,
    Return,
    SetGlobal(u8),
    SetLocal(u8),
    Substract,
    True,
}

#[derive(Debug)]
pub(crate) struct CompiledProgram {
    pub(crate) constants: Vec<Value>,
    pub(crate) instructions: Vec<Instruction>,
}

pub(crate) struct Compiler {
    bytecode_program: CompiledProgram,
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

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            bytecode_program: CompiledProgram::new(),
        }
    }

    pub(crate) fn compile(mut self, program: &Program) -> CompiledProgram {
        for stmt in program.main_stmts.iter() {
            match stmt {
                StmtKind::BreakLoop => todo!(),
                StmtKind::Expr(expr) => {
                    self.eval_expr(expr);
                }
                StmtKind::Comment(_) => todo!(),
                StmtKind::Print(exprs) => todo!(),
                StmtKind::FuncCall(_) => todo!(),
                StmtKind::FuncReturn(_) => todo!(),
                StmtKind::Declare { lhs, rhs } => todo!(),
                StmtKind::Assign { lhs, rhs } => todo!(),
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
        self.bytecode_program
    }

    fn eval_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::BinaryExpr { op, lhs, rhs } => {
                self.eval_expr(lhs);
                self.eval_expr(rhs);
                self.bytecode_program.write_instruction(Instruction::Add);
            }
            Expr::LogicalExpr { op, lhs, rhs } => todo!(),
            Expr::UnaryExpr { op, child } => todo!(),
            Expr::ExprLeaf(expr_leaf) => {
                let val = ExprLeaf::to_value(expr_leaf);
                let idx = self.bytecode_program.push_constant(val);
                self.bytecode_program
                    .write_instruction(Instruction::Constant(idx));
            }
            Expr::Ident(_) => todo!(),
        }
    }
}
