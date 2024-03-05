use std::{
    array::IntoIter,
    fmt::{Debug, Display},
    mem,
    ops::{Index, IndexMut},
    slice::Iter,
};

use crate::parser::{BinaryOp, Expr, ExprLeaf, Program, StmtKind};

#[derive(Clone, Copy, Debug, Default)]
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

pub(crate) struct RegisterVariableMap<'a> {
    register_variable_map: [&'a str; mem::variant_count::<Register>()],
}

impl<'a> RegisterVariableMap<'a> {
    pub fn new() -> Self {
        Self {
            register_variable_map: [&""; mem::variant_count::<Register>()],
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

impl<T: Default + Copy, const L: usize> ArrayVec<T, L> {
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
    available_regs: ArrayVec<Register, 16>,
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

    fn expr_codegen(&mut self, expr: Expr<'a>) -> Register {
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
                    reg
                }
                ExprLeaf::Float(_) => todo!(),
                ExprLeaf::Char(_) => todo!(),
                ExprLeaf::Str(_) => todo!(),
            },
            Expr::Ident(_) => todo!(),
        }
    }

    fn binary_expr_codgen(&mut self, expr: Expr<'a>) -> Register {
        if let Expr::BinaryExpr { op, lhs, rhs } = expr {
            let reg1 = self.expr_codegen(*lhs);
            let reg2 = self.expr_codegen(*rhs);
            match op {
                BinaryOp::Add => {
                    self.code.push_str(&format!("ADD {}, {}\n", reg1, reg2));
                    reg1
                }
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

        let codegen = Codegen::new();
        let code = codegen.codegen(ast);
        println!("{}", code);
        assert!(false);
        Ok(())
    }
}
