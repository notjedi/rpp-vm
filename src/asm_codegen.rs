use std::{
    array::IntoIter,
    collections::HashMap,
    fmt::{Debug, Display},
    mem,
    ops::{Index, IndexMut},
    slice::Iter,
};

use crate::parser::{BinaryOp, Expr, ExprLeaf, ForVar, Program, StmtKind};

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub(crate) enum Register {
    #[default]
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RBP,
    RSP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Register {
    pub const REGS: [Register; mem::variant_count::<Register>()] = [
        Self::RAX,
        Self::RBX,
        Self::RCX,
        Self::RDX,
        Self::RSI,
        Self::RDI,
        Self::RBP,
        Self::RSP,
        Self::R8,
        Self::R9,
        Self::R10,
        Self::R11,
        Self::R12,
        Self::R13,
        Self::R14,
        Self::R15,
    ];

    fn iter() -> Iter<'static, Register> {
        Self::REGS.iter()
    }

    fn into_iter() -> IntoIter<Register, { mem::variant_count::<Register>() }> {
        Self::REGS.into_iter()
    }
}

impl Iterator for Register {
    type Item = Register;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Register {
    fn index(&self) -> usize {
        match self {
            Register::RAX => 0,
            Register::RBX => 1,
            Register::RCX => 2,
            Register::RDX => 3,
            Register::RSI => 4,
            Register::RDI => 5,
            Register::RBP => 6,
            Register::RSP => 7,
            Register::R8 => 8,
            Register::R9 => 9,
            Register::R10 => 10,
            Register::R11 => 11,
            Register::R12 => 12,
            Register::R13 => 13,
            Register::R14 => 14,
            Register::R15 => 15,
        }
    }
}

#[derive(Debug, Default)]
struct VarUsageTracker {
    var_usage_map: HashMap<String, usize>,
    stmt_count: usize,
}

impl VarUsageTracker {
    fn track(&mut self, ast: &Program) {
        self.visit_statements(&ast.main_stmts);
    }

    fn visit_statements(&mut self, stmts: &[StmtKind]) {
        for stmt in stmts.iter().rev() {
            match stmt {
                StmtKind::BreakLoop => {}
                StmtKind::Expr(expr) => self.visit_expr(expr),
                StmtKind::Comment(_) => {}
                StmtKind::Print(exprs) => {
                    for expr in exprs.iter() {
                        self.visit_expr(expr);
                    }
                }
                StmtKind::FuncCall(_) => todo!(),
                StmtKind::FuncReturn(_) => todo!(),
                StmtKind::Declare { lhs, rhs } => {
                    self.visit_expr(rhs);
                    if !self.var_usage_map.contains_key(*lhs) {
                        self.var_usage_map.insert(lhs.to_string(), self.stmt_count);
                    }
                }
                StmtKind::Assign { lhs, rhs } => {
                    self.visit_expr(rhs);
                    if !self.var_usage_map.contains_key(*lhs) {
                        self.var_usage_map.insert(lhs.to_string(), self.stmt_count);
                    }
                }
                StmtKind::AssignFuncCall {
                    var_name,
                    func_name,
                } => {
                    if !self.var_usage_map.contains_key(*var_name) {
                        self.var_usage_map
                            .insert(var_name.to_string(), self.stmt_count);
                    }
                }
                StmtKind::IfCond {
                    condition,
                    body,
                    else_body,
                } => {
                    let start_stmt_count = self.stmt_count;
                    self.visit_statements(else_body);
                    let else_stmt_count = self.stmt_count;
                    self.stmt_count = start_stmt_count;
                    self.visit_statements(body);
                    self.stmt_count = self.stmt_count.max(else_stmt_count);
                    self.visit_expr(&condition);
                }
                StmtKind::ForLoop { start, end, body } => {
                    if let ForVar::Ident(var_name) = **start {
                        if !self.var_usage_map.contains_key(var_name) {
                            self.var_usage_map
                                .insert(var_name.to_string(), self.stmt_count);
                        }
                    }
                    if let ForVar::Ident(var_name) = **end {
                        if !self.var_usage_map.contains_key(var_name) {
                            self.var_usage_map
                                .insert(var_name.to_string(), self.stmt_count);
                        }
                    }
                    self.visit_statements(body);
                }
                StmtKind::WhileLoop { condition, body } => {
                    self.visit_expr(&condition);
                    self.visit_statements(body);
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::BinaryExpr { op, lhs, rhs } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.stmt_count += 1;
            }
            Expr::LogicalExpr { op, lhs, rhs } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
                self.stmt_count += 1;
            }
            Expr::UnaryExpr { op, child } => {
                self.visit_expr(child);
                self.stmt_count += 1;
            }
            Expr::ExprLeaf(expr_leaf) => {}
            Expr::Ident(var_name) => {
                if !self.var_usage_map.contains_key(*var_name) {
                    self.var_usage_map
                        .insert(var_name.to_string(), self.stmt_count);
                }
            }
        }
    }
}

fn var_usage_logger(ast: &Program) -> VarUsageTracker {
    let mut var_tracker = VarUsageTracker::default();
    var_tracker.track(&ast);
    var_tracker
}

pub(crate) struct RegisterVariableMap<'a> {
    register_variable_map: [&'a str; mem::variant_count::<Register>()],
}

impl<'a> RegisterVariableMap<'a> {
    pub fn new() -> Self {
        Self {
            register_variable_map: [&""; mem::variant_count::<Register>()],
        }
    }

    pub fn find(&self, var: &str) -> Option<usize> {
        for i in 0..mem::variant_count::<Register>() {
            if self.register_variable_map[i].is_empty() {
                break;
            }
            if self.register_variable_map[i] == var {
                return Some(i);
            }
        }
        None
    }

    pub fn remove(&mut self, var: &str) {
        for i in 0..mem::variant_count::<Register>() {
            if self.register_variable_map[i].is_empty() {
                break;
            }
            if self.register_variable_map[i] == var {
                // left shift elems
                for j in i..(mem::variant_count::<Register>() - 1) {
                    self.register_variable_map[j] = self.register_variable_map[j + 1];
                    if self.register_variable_map[j + 1].is_empty() {
                        break;
                    }
                }
                break;
            }
        }
    }
}

impl<'a> Index<Register> for RegisterVariableMap<'a> {
    type Output = &'a str;

    fn index(&self, index: Register) -> &Self::Output {
        &self.register_variable_map[index.index()]
    }
}

impl<'a> IndexMut<Register> for RegisterVariableMap<'a> {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        &mut self.register_variable_map[index.index()]
    }
}

struct ArrayVec<T, const L: usize> {
    arr: [T; L],
    len: Option<usize>,
}

impl<T: Default + Copy + Eq, const L: usize> ArrayVec<T, L> {
    pub fn new() -> Self {
        Self {
            // TODO: use some other zero value, even if it's unsafe
            arr: [T::default(); L],
            len: None,
        }
    }

    pub fn push(&mut self, val: T) {
        if self.len() > L {
            panic!("out of bounds");
        }
        let end = self.len();
        self.arr[end] = val;
        self.len = self.len.map(|x| x + 1).or(Some(0))
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len() == 0 {
            return None;
        }
        self.len = Some(self.len.unwrap() - 1);
        Some(self.arr[self.len.unwrap()])
    }

    pub fn find(&self, elem: T) -> Option<usize> {
        if let Some(len) = self.len {
            for i in 0..len {
                if self.arr[i] == elem {
                    return Some(i);
                }
            }
        }
        None
    }

    pub fn remove(&mut self, elem: T) {
        if let Some(len) = self.len {
            for i in 0..len {
                if self.arr[i] == elem {
                    // left shift elems
                    for j in i..(len - 1) {
                        self.arr[j] = self.arr[j + 1];
                    }
                    self.len = if len - 1 == 0 { None } else { Some(len - 1) };
                    break;
                }
            }
        } else {
            panic!("hi, what do you want to remove?")
        }
    }

    pub fn len(&self) -> usize {
        match self.len {
            Some(idx) => idx + 1,
            None => 0,
        }
    }
}

impl<T, const L: usize> From<[T; L]> for ArrayVec<T, L> {
    fn from(value: [T; L]) -> Self {
        ArrayVec {
            arr: value,
            len: Some(L),
        }
    }
}

struct Codegen<'a> {
    register_variable_map: RegisterVariableMap<'a>,
    available_regs: ArrayVec<Register, { mem::variant_count::<Register>() }>,
    stack: Vec<&'a str>,
    code: String,
}

impl<'a> Codegen<'a> {
    pub fn new() -> Self {
        Self {
            register_variable_map: RegisterVariableMap::new(),
            available_regs: ArrayVec::from(Register::REGS),
            stack: vec![],
            code: String::new(),
        }
    }

    pub fn codegen(mut self, program: Program<'a>) -> String {
        self.code.push_str("SECTION .text\n");
        self.code.push_str("global _start\n");
        self.code.push_str("_start:\n");
        for stmt in program.main_stmts.into_iter() {
            match stmt {
                StmtKind::BreakLoop => todo!(),
                StmtKind::Expr(expr) => {
                    self.expr_codegen(expr);
                }
                StmtKind::Comment(_) => todo!(),
                StmtKind::Print(_) => todo!(),
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
        self.code.push_str("MOV RBX, 0\n");
        self.code.push_str("MOV RAX, 60\n");
        self.code.push_str("SYSCALL\n");
        self.code
    }

    fn emit_mov(&mut self, instr: &str, reg: Register, var_name: &'a str) {
        self.code.push_str(instr);
        self.register_variable_map[reg] = var_name;
    }

    fn get_available_reg(&mut self) -> Register {
        if let Some(reg) = self.available_regs.pop() {
            reg
        } else {
            for reg in Register::into_iter() {
                self.code.push_str(&format!("PUSH {}\n", reg));
                self.stack.push(self.register_variable_map[reg]);
                self.register_variable_map[reg] = &"";
                self.available_regs.push(reg);
            }
            self.get_available_reg()
        }
    }

    fn expr_codegen(&mut self, expr: Expr<'a>) {
        match &expr {
            Expr::BinaryExpr { .. } => self.binary_expr_codgen(expr),
            Expr::LogicalExpr { .. } => todo!(),
            Expr::UnaryExpr { .. } => todo!(),
            Expr::ExprLeaf(leaf) => match leaf {
                ExprLeaf::BoolTrue => todo!(),
                ExprLeaf::BoolFalse => todo!(),
                ExprLeaf::Int(val) => {
                    let reg = self.get_available_reg();
                    self.code.push_str(&format!("MOV {}, {}\n", reg, val));
                    self.register_variable_map[reg] = &"__imm_val_internal__";
                }
                ExprLeaf::Float(_) => todo!(),
                ExprLeaf::Char(_) => todo!(),
                ExprLeaf::Str(_) => todo!(),
            },
            Expr::Ident(_) => todo!(),
        }
    }

    fn binary_expr_codgen(&mut self, expr: Expr<'a>) {
        if let Expr::BinaryExpr { op, lhs, rhs } = expr {
            self.expr_codegen(*lhs);
            self.expr_codegen(*rhs);
            match op {
                BinaryOp::Add => todo!(),
                BinaryOp::Sub => todo!(),
                BinaryOp::Mul => todo!(),
                BinaryOp::Div => todo!(),
                BinaryOp::Mod => todo!(),
            }
        } else {
            unreachable!()
        }
    }

    fn add(&mut self) {}
}

#[cfg(test)]
mod tests {
    use color_eyre::eyre::Result;

    use super::Codegen;
    use crate::{
        compiler::Compiler,
        lexer::{Lexer, TokenKind},
        Parser,
    };

    #[test]
    fn test_codegen() -> Result<()> {
        let program = r#"
            LAKSHMI START
                40 + 10;
            MAGIZHCHI
        "#;

        let tokens = Lexer::tokenize_str(program)?;
        let token_kinds = tokens
            .into_iter()
            .map(|tok| tok.kind)
            .collect::<Vec<TokenKind>>();

        let parser = Parser::new(&token_kinds);
        let ast = parser.parse().unwrap();

        let compiler = Compiler::new();
        let bytecode_program = compiler.compile_program(&ast);

        let codegen = Codegen::new();
        let code = codegen.codegen(ast);
        println!("{}", code);
        assert!(false);
        Ok(())
    }
}
