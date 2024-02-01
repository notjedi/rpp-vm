use crate::{
    interpreter::Value,
    parser::{BinaryOp, Expr, ExprLeaf, ForVar, LogicalOp, Program, StmtKind},
};

#[derive(Clone, Debug)]
pub(crate) enum Instruction {
    Add,
    Constant(usize),
    Divide,
    Equal,
    GetLocal(usize),
    Greater,
    Jump(usize),
    JumpIfFalse(usize),
    Less,
    Loop(usize),
    Method(usize),
    Mod,
    Multiply,
    Not,
    Pop,
    Print,
    Return,
    SetLocal(usize),
    Substract,
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
        self.eval_stmts(&program.main_stmts);
        self.bytecode_program.write_instruction(Instruction::Return);
        self.bytecode_program
    }

    fn eval_stmts(&mut self, stmts: &Vec<StmtKind>) {
        for stmt in stmts.iter() {
            self.eval_stmt(stmt);
            if let StmtKind::Expr(_) = stmt {
                self.bytecode_program.write_instruction(Instruction::Pop);
            }
        }
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
                return Some(self.locals.len() - idx - 1);
            }
        }
        None
    }

    fn check_if_var_in_scope(&self, name: &str) -> bool {
        let mut locals_iter = self.locals.iter();
        while let Some(local) = locals_iter.next()
            && local.depth == self.scope_depth
        {
            if *local.name == name {
                return true;
            }
        }
        return false;
    }

    #[inline]
    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
        let mut count = 0;
        for local in self.locals.iter().rev() {
            if local.depth > self.scope_depth {
                self.bytecode_program.write_instruction(Instruction::Pop);
                count += 1;
            } else {
                break;
            }
        }
        (0..count).for_each(|_| {
            self.locals.pop();
        });
    }

    #[inline]
    fn instructions_len(&self) -> usize {
        self.bytecode_program.instructions.len()
    }

    fn emit_jump(&mut self, instr: Instruction) -> usize {
        self.bytecode_program
            .write_instruction(Instruction::JumpIfFalse(0));
        self.instructions_len() - 1
    }

    fn patch_jump(&mut self, offset: usize) {
        let curr_instr_len = self.instructions_len() - 1;
        let patch_offset = curr_instr_len - offset + 1;
        let instr = match &self.bytecode_program.instructions[offset] {
            Instruction::JumpIfFalse(_) => Instruction::JumpIfFalse(patch_offset),
            Instruction::Jump(_) => Instruction::Jump(patch_offset),
            _ => unreachable!(),
        };
        let instr = self.bytecode_program.instructions[offset] = instr;
    }

    fn forvar_to_expr(val: &ForVar) -> Expr {
        match val {
            ForVar::Int(int_val) => Expr::ExprLeaf(ExprLeaf::Int(*int_val)),
            ForVar::Ident(var_name) => Expr::Ident((*var_name).clone()),
            ForVar::Float(_) => todo!("return compile time error"),
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
                match self.check_if_var_in_scope(lhs) {
                    true => todo!("return compile time error"),
                    false => {
                        self.eval_expr(rhs);
                        self.locals.push(Local {
                            name: lhs.clone(),
                            depth: self.scope_depth,
                        });
                        self.bytecode_program
                            .write_instruction(Instruction::SetLocal(self.locals.len() - 1));
                    }
                };
            }
            StmtKind::Assign { lhs, rhs } => {
                self.eval_expr(rhs);
                if let Some(idx) = self.get_idx_of_local(lhs) {
                    self.bytecode_program
                        .write_instruction(Instruction::SetLocal(idx));
                    // TODO: is this right? refer to the book once;
                    self.bytecode_program.write_instruction(Instruction::Pop);
                } else {
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
            } => {
                // setup instructions for the condition expr
                self.eval_expr(condition);
                let if_offset = self.emit_jump(Instruction::JumpIfFalse(0));

                // generate instructions for the actual body of the if stmt
                self.begin_scope();
                // pop the result of the comparision
                self.bytecode_program.write_instruction(Instruction::Pop);
                self.eval_stmts(body);
                self.end_scope();

                if else_body.len() != 0 {
                    // this also comes as the final part of the if stmt when an else block is there so it can skip the else block
                    let else_offset = self.emit_jump(Instruction::Jump(0));
                    self.patch_jump(if_offset);

                    // generate instructions for the actual body of the else stmt
                    self.begin_scope();
                    // pop the result of the comparision
                    self.bytecode_program.write_instruction(Instruction::Pop);
                    self.eval_stmts(else_body);
                    self.end_scope();
                    self.patch_jump(else_offset);
                } else {
                    self.patch_jump(if_offset);
                    // self.bytecode_program.write_instruction(Instruction::Pop);
                    // TODO: when the if stmt fails and there is no else stmt, we need to pop the result of the comparision
                }
            }
            StmtKind::ForLoop { start, end, body } => {
                let mut loop_start = self.instructions_len();
                let start_expr = Self::forvar_to_expr(start);
                let end_expr = Self::forvar_to_expr(end);
                let incr_expr = Expr::ExprLeaf(ExprLeaf::Int(1));

                // setup instructions for the condition expr
                if let Expr::ExprLeaf(ExprLeaf::Int(_)) = start_expr {
                    let start_expr_declare = StmtKind::Declare {
                        lhs: Box::new(String::from("for_var_special")),
                        rhs: Box::new(start_expr.clone()),
                    };
                    self.eval_stmt(&start_expr_declare);
                    loop_start = self.instructions_len();
                    self.bytecode_program
                        .write_instruction(Instruction::GetLocal(self.locals.len() - 1));
                } else {
                    self.eval_expr(&start_expr);
                }
                self.eval_expr(&end_expr);
                self.bytecode_program.write_instruction(Instruction::Less);
                let exit_jump = self.emit_jump(Instruction::JumpIfFalse(0));

                // 1st stmt of the loop, pop the comparision result
                self.begin_scope();
                // pop the result of the comparision
                self.bytecode_program.write_instruction(Instruction::Pop);
                self.eval_stmts(&body);
                self.end_scope();

                // incr the start var
                match &(**start) {
                    ForVar::Int(int_val) => {
                        let incr_stmt = StmtKind::Assign {
                            lhs: Box::new(String::from("for_var_special")),
                            rhs: Box::new(Expr::BinaryExpr {
                                op: BinaryOp::Add,
                                lhs: Box::new(Expr::Ident(Box::new(String::from(
                                    "for_var_special",
                                )))),
                                rhs: Box::new(incr_expr),
                            }),
                        };
                        self.eval_stmt(&incr_stmt);
                    }
                    ForVar::Ident(var_name) => {
                        let incr_stmt = StmtKind::Assign {
                            lhs: (*var_name).clone(),
                            rhs: Box::new(Expr::BinaryExpr {
                                op: BinaryOp::Add,
                                lhs: Box::new(start_expr),
                                rhs: Box::new(incr_expr),
                            }),
                        };
                        self.eval_stmt(&incr_stmt);
                    }
                    ForVar::Float(_) => todo!("return compile time error"),
                };

                let loop_offset = self.instructions_len() - loop_start;
                self.bytecode_program
                    .write_instruction(Instruction::Loop(loop_offset));
                self.patch_jump(exit_jump);

                // 1st stmt after exiting the loop, pop the comparision result
                self.bytecode_program.write_instruction(Instruction::Pop);
            }
            StmtKind::WhileLoop { condition, body } => {
                // setup instructions for the condition expr
                let loop_start = self.instructions_len();
                self.eval_expr(&condition);
                let exit_jump = self.emit_jump(Instruction::JumpIfFalse(0));

                // generate instructions for the actual body of the loop
                self.begin_scope();
                // pop the result of the comparision
                self.bytecode_program.write_instruction(Instruction::Pop);
                self.eval_stmts(&body);
                self.end_scope();

                // emit loop Instruction so it can go back up to `loop_start`
                let loop_offset = self.instructions_len() - loop_start;
                self.bytecode_program
                    .write_instruction(Instruction::Loop(loop_offset));
                // path the exit jump so when the condition fails it can get out of the loop
                self.patch_jump(exit_jump);
                // pop the result of the comparision
                self.bytecode_program.write_instruction(Instruction::Pop);
            }
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
