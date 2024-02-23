use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{Add, Div, Mul, Rem, Sub},
};

use crate::compiler::{Bytecode, CompilerValue, Instruction, ProgFunction};

// TODO: how does python go about this?
// there is a local variable with same name as global function
// 1. user tries to mutate the local variable using the same identifier
// 2. user tries to call the global function using the same identifier
#[derive(Debug)]
pub(crate) struct CallFrame<'ast> {
    program: ProgFunction<'ast>,
    stack_offset: usize,
    ip: usize,
}

#[derive(Debug)]
pub(crate) struct Vm<'ast> {
    value_stack: Vec<CompilerValue<'ast>>,
    call_stack: Vec<CallFrame<'ast>>,
    globals: HashMap<String, CompilerValue<'ast>>,
}

impl<'a> Display for CompilerValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => f.write_fmt(format_args!("unit")),
            Self::Str(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Int(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Bool(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Char(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Float(arg0) => f.write_fmt(format_args!("{}", arg0)),
            Self::Function(_) => unreachable!(),
        }
    }
}

impl<'a> PartialOrd for CompilerValue<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (CompilerValue::Unit, CompilerValue::Unit) => Some(Ordering::Equal),
            (CompilerValue::Int(lhs), CompilerValue::Int(rhs)) => lhs.partial_cmp(rhs),
            (CompilerValue::Float(lhs), CompilerValue::Float(rhs)) => lhs.partial_cmp(rhs),
            (CompilerValue::Char(lhs), CompilerValue::Char(rhs)) => lhs.partial_cmp(rhs),
            (CompilerValue::Bool(lhs), CompilerValue::Bool(rhs)) => lhs.partial_cmp(rhs),
            (CompilerValue::Int(lhs), CompilerValue::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (CompilerValue::Float(lhs), CompilerValue::Int(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            _ => None,
        }
    }
}

impl<'a> PartialEq for CompilerValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (CompilerValue::Unit, CompilerValue::Unit) => true,
            (CompilerValue::Int(lhs), CompilerValue::Int(rhs)) => lhs == rhs,
            (CompilerValue::Float(lhs), CompilerValue::Float(rhs)) => lhs == rhs,
            (CompilerValue::Char(lhs), CompilerValue::Char(rhs)) => lhs == rhs,
            (CompilerValue::Bool(lhs), CompilerValue::Bool(rhs)) => lhs == rhs,
            (CompilerValue::Str(lhs), CompilerValue::Str(rhs)) => lhs == rhs,
            (CompilerValue::Int(lhs), CompilerValue::Float(rhs)) => &(*lhs as f64) == rhs,
            (CompilerValue::Float(lhs), CompilerValue::Int(rhs)) => lhs == &(*rhs as f64),
            _ => false,
        }
    }
}

impl<'a> Add<CompilerValue<'a>> for CompilerValue<'a> {
    type Output = CompilerValue<'a>;

    fn add(self, rhs: CompilerValue) -> Self::Output {
        match (self, rhs) {
            (CompilerValue::Int(lhs), CompilerValue::Int(rhs)) => CompilerValue::Int(lhs + rhs),
            (CompilerValue::Int(lhs), CompilerValue::Float(rhs)) => {
                CompilerValue::Float(lhs as f64 + rhs)
            }
            (CompilerValue::Float(lhs), CompilerValue::Int(rhs)) => {
                CompilerValue::Float(lhs + rhs as f64)
            }
            (CompilerValue::Float(lhs), CompilerValue::Float(rhs)) => {
                CompilerValue::Float(lhs + rhs)
            }
            (CompilerValue::Str(lhs), rhs) => {
                CompilerValue::Str(Cow::Owned(lhs.to_string() + " " + &rhs.to_string()))
            }
            (lhs, CompilerValue::Str(rhs)) => {
                CompilerValue::Str(Cow::Owned(lhs.to_string() + " " + &rhs))
            }
            _ => unreachable!(),
        }
    }
}

impl<'a> Add<&CompilerValue<'a>> for CompilerValue<'a> {
    type Output = CompilerValue<'a>;

    fn add(self, rhs: &CompilerValue) -> Self::Output {
        match (self, rhs) {
            (CompilerValue::Int(lhs), CompilerValue::Int(rhs)) => CompilerValue::Int(lhs + *rhs),
            (CompilerValue::Int(lhs), CompilerValue::Float(rhs)) => {
                CompilerValue::Float(lhs as f64 + *rhs)
            }
            (CompilerValue::Float(lhs), CompilerValue::Int(rhs)) => {
                CompilerValue::Float(lhs + *rhs as f64)
            }
            (CompilerValue::Float(lhs), CompilerValue::Float(rhs)) => {
                CompilerValue::Float(lhs + *rhs)
            }
            (CompilerValue::Str(lhs), rhs) => {
                CompilerValue::Str(Cow::Owned(lhs.to_string() + " " + &rhs.to_string()))
            }
            (lhs, CompilerValue::Str(rhs)) => {
                CompilerValue::Str(Cow::Owned(lhs.to_string() + " " + &rhs))
            }
            _ => unreachable!(),
        }
    }
}

impl<'a> Add<CompilerValue<'a>> for &CompilerValue<'a> {
    type Output = CompilerValue<'a>;

    fn add(self, rhs: CompilerValue) -> Self::Output {
        match (self, rhs) {
            (CompilerValue::Int(lhs), CompilerValue::Int(rhs)) => CompilerValue::Int(*lhs + rhs),
            (CompilerValue::Int(lhs), CompilerValue::Float(rhs)) => {
                CompilerValue::Float(*lhs as f64 + rhs)
            }
            (CompilerValue::Float(lhs), CompilerValue::Int(rhs)) => {
                CompilerValue::Float(*lhs + rhs as f64)
            }
            (CompilerValue::Float(lhs), CompilerValue::Float(rhs)) => {
                CompilerValue::Float(*lhs + rhs)
            }
            (CompilerValue::Str(lhs), rhs) => {
                CompilerValue::Str(Cow::Owned(lhs.to_string() + " " + &rhs.to_string()))
            }
            (lhs, CompilerValue::Str(rhs)) => {
                CompilerValue::Str(Cow::Owned(lhs.to_string() + " " + &rhs))
            }
            _ => unreachable!(),
        }
    }
}

impl<'a> Add<&CompilerValue<'a>> for &CompilerValue<'a> {
    type Output = CompilerValue<'a>;

    fn add(self, rhs: &CompilerValue) -> Self::Output {
        match (self, rhs) {
            (CompilerValue::Int(lhs), CompilerValue::Int(rhs)) => CompilerValue::Int(*lhs + *rhs),
            (CompilerValue::Int(lhs), CompilerValue::Float(rhs)) => {
                CompilerValue::Float(*lhs as f64 + *rhs)
            }
            (CompilerValue::Float(lhs), CompilerValue::Int(rhs)) => {
                CompilerValue::Float(*lhs + *rhs as f64)
            }
            (CompilerValue::Float(lhs), CompilerValue::Float(rhs)) => {
                CompilerValue::Float(*lhs + *rhs)
            }
            (CompilerValue::Str(lhs), rhs) => {
                CompilerValue::Str(Cow::Owned(lhs.to_string() + " " + &rhs.to_string()))
            }
            (lhs, CompilerValue::Str(rhs)) => {
                CompilerValue::Str(Cow::Owned(lhs.to_string() + " " + &rhs))
            }
            _ => unreachable!(),
        }
    }
}

macro_rules! impl_bin_ops {
    ($op_trait:ident, $op_fn:ident, $op:tt) => {
        impl<'a> $op_trait<CompilerValue<'a>> for CompilerValue<'a> {
            type Output = CompilerValue<'a>;

            fn $op_fn(self, rhs: CompilerValue) -> Self::Output {
                match (self, rhs) {
                    (CompilerValue::Int(lhs), CompilerValue::Int(rhs)) => CompilerValue::Int(lhs $op rhs),
                    (CompilerValue::Int(lhs), CompilerValue::Float(rhs)) => CompilerValue::Float(lhs as f64 $op rhs),
                    (CompilerValue::Float(lhs), CompilerValue::Int(rhs)) => CompilerValue::Float(lhs $op rhs as f64),
                    (CompilerValue::Float(lhs), CompilerValue::Float(rhs)) => CompilerValue::Float(lhs $op rhs),
                    _ => unreachable!(),
                }
            }
        }

        impl<'a> $op_trait<&CompilerValue<'a>> for CompilerValue<'a> {
            type Output = CompilerValue<'a>;

            fn $op_fn(self, rhs: &CompilerValue) -> Self::Output {
                match (self, rhs) {
                    (CompilerValue::Int(lhs), CompilerValue::Int(rhs)) => CompilerValue::Int(lhs $op *rhs),
                    (CompilerValue::Int(lhs), CompilerValue::Float(rhs)) => CompilerValue::Float(lhs as f64 $op *rhs),
                    (CompilerValue::Float(lhs), CompilerValue::Int(rhs)) => CompilerValue::Float(lhs $op *rhs as f64),
                    (CompilerValue::Float(lhs), CompilerValue::Float(rhs)) => CompilerValue::Float(lhs $op *rhs),
                    _ => unreachable!(),
                }
            }
        }

        impl<'a> $op_trait<CompilerValue<'a>> for &CompilerValue<'a> {
            type Output = CompilerValue<'a>;

            fn $op_fn(self, rhs: CompilerValue) -> Self::Output {
                match (self, rhs) {
                    (CompilerValue::Int(lhs), CompilerValue::Int(rhs)) => CompilerValue::Int(*lhs $op rhs),
                    (CompilerValue::Int(lhs), CompilerValue::Float(rhs)) => CompilerValue::Float(*lhs as f64 $op rhs),
                    (CompilerValue::Float(lhs), CompilerValue::Int(rhs)) => CompilerValue::Float(*lhs $op rhs as f64),
                    (CompilerValue::Float(lhs), CompilerValue::Float(rhs)) => CompilerValue::Float(*lhs $op rhs),
                    _ => unreachable!(),
                }
            }
        }

        impl<'a> $op_trait<&CompilerValue<'a>> for &CompilerValue<'a> {
            type Output = CompilerValue<'a>;

            fn $op_fn(self, rhs: &CompilerValue) -> Self::Output {
                match (self, rhs) {
                    (CompilerValue::Int(lhs), CompilerValue::Int(rhs)) => CompilerValue::Int(*lhs $op *rhs),
                    (CompilerValue::Int(lhs), CompilerValue::Float(rhs)) => CompilerValue::Float(*lhs as f64 $op *rhs),
                    (CompilerValue::Float(lhs), CompilerValue::Int(rhs)) => CompilerValue::Float(*lhs $op *rhs as f64),
                    (CompilerValue::Float(lhs), CompilerValue::Float(rhs)) => CompilerValue::Float(*lhs $op *rhs),
                    _ => unreachable!(),
                }
            }
        }
    };
}

impl_bin_ops!(Sub, sub, -);
impl_bin_ops!(Mul, mul, *);
impl_bin_ops!(Div, div, /);
impl_bin_ops!(Rem, rem, %);

impl<'ast> Vm<'ast> {
    pub(crate) fn new() -> Self {
        Self {
            call_stack: vec![],
            value_stack: vec![],
            globals: HashMap::new(),
        }
    }

    fn current_frame(&self) -> &CallFrame<'ast> {
        self.call_stack.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame<'ast> {
        self.call_stack.last_mut().unwrap()
    }

    fn pop_stack(&mut self) -> CompilerValue<'ast> {
        self.value_stack.pop().expect("no values to pop from stack")
    }

    fn inc_ip(&mut self, offset: usize) {
        self.current_frame_mut().ip += offset;
    }

    fn dec_ip(&mut self, offset: usize) {
        self.current_frame_mut().ip -= offset;
    }

    fn current_bytecode(&self) -> &Bytecode<'ast> {
        &self.current_frame().program.bytecode
    }

    pub(crate) fn interpret(&mut self, program: &ProgFunction<'ast>) {
        self.call_stack.push(CallFrame {
            program: program.clone(),
            stack_offset: 0,
            ip: 0,
        });
        loop {
            let instr = self.current_bytecode().instructions[self.current_frame().ip];
            match instr {
                Instruction::Add => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a + b;
                    self.value_stack.push(res);
                }
                Instruction::Constant(idx) => {
                    // TODO: bro, i need to find a way out of clones
                    let val = self.current_bytecode().constants[idx].clone();
                    self.value_stack.push(val);
                }
                Instruction::Divide => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a / b;
                    self.value_stack.push(res);
                }
                Instruction::Equal => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = CompilerValue::Bool(a == b);
                    self.value_stack.push(res);
                }
                Instruction::GetGlobal(idx) => {
                    let global_name = &self.current_bytecode().constants[idx];
                    if let CompilerValue::Str(name) = global_name {
                        if let Some(val) = self.globals.get(&(**name)) {
                            self.value_stack.push(val.clone());
                        } else {
                            todo!("return runtime time error")
                        }
                    }
                }
                Instruction::GetLocal(idx) => {
                    let stack_offset = self.current_frame().stack_offset;
                    self.value_stack
                        .push(self.value_stack[stack_offset + idx].clone());
                }
                Instruction::Greater => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = CompilerValue::Bool(a > b);
                    self.value_stack.push(res);
                }
                Instruction::Jump(offset) => {
                    self.inc_ip(offset);
                    continue;
                }
                Instruction::JumpIfFalse(offset) => {
                    if let Some(CompilerValue::Bool(val)) = self.value_stack.last() {
                        if !val {
                            self.inc_ip(offset);
                            continue;
                        }
                    } else {
                        todo!("return runtime time error")
                    }
                }
                Instruction::Less => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = CompilerValue::Bool(a < b);
                    self.value_stack.push(res);
                }
                Instruction::Loop(offset) => {
                    self.dec_ip(offset);
                    continue;
                }
                Instruction::Method(idx) => {
                    let func_name = &self.current_bytecode().constants[idx];
                    if let CompilerValue::Str(name) = func_name {
                        let func_val = self.globals.get(&(**name)).expect("can't find function");
                        if let CompilerValue::Function(func) = func_val {
                            let func_call_frame = CallFrame {
                                program: func.clone(),
                                stack_offset: self.value_stack.len(),
                                ip: 0,
                            };
                            self.call_stack.push(func_call_frame);
                            continue;
                        }
                    }
                }
                Instruction::Mod => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a % b;
                    self.value_stack.push(res);
                }
                Instruction::Multiply => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a * b;
                    self.value_stack.push(res);
                }
                Instruction::Not => {
                    if let CompilerValue::Bool(val) = self.pop_stack() {
                        self.value_stack.push(CompilerValue::Bool(!val));
                    } else {
                        todo!("return runtime time error")
                    }
                }
                Instruction::Pop => {
                    self.pop_stack();
                }
                Instruction::Print => {
                    let str_val = self.pop_stack();
                    println!("{}", str_val.to_string());
                }
                Instruction::Return => {
                    if self.call_stack.len() > 1 {
                        let result = self.pop_stack();
                        let val_stack_offset = self.current_frame().stack_offset;
                        self.call_stack.pop();
                        self.value_stack.truncate(val_stack_offset);
                        self.value_stack.push(result);
                    } else {
                        return;
                    }
                }
                Instruction::SetGlobal(idx) => {
                    let glob_name = self.current_bytecode().constants[idx].clone();
                    let val = self.pop_stack();
                    if let CompilerValue::Str(name) = glob_name {
                        match self.globals.get_mut(&(*name)) {
                            Some(entry) => {
                                *entry = val;
                            }
                            None => {
                                self.globals.insert(name.to_string(), val);
                            }
                        }
                    }
                }
                Instruction::SetLocal(idx) => {
                    let stack_offset = self.current_frame().stack_offset;
                    self.value_stack[stack_offset + idx] =
                        self.value_stack.last().expect("stack is empty").clone();
                }
                Instruction::Substract => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a - b;
                    self.value_stack.push(res);
                }
            }
            self.inc_ip(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{io, sync::Arc};

    use crate::{compiler::Compiler, lexer::Lexer, parser::Parser, vm::Vm, TokenKind};

    use color_eyre::eyre::Result;

    #[test]
    fn test_vm() -> Result<()> {
        let snapshot_string = capture_stdout(|| {
            let program = include_str!("../testdata/snapshots/test_program.rpp");
            let tokens = Lexer::tokenize_str(program).unwrap();
            let token_kinds = tokens
                .into_iter()
                .map(|tok| tok.kind)
                .collect::<Vec<TokenKind>>();
            let parser = Parser::new(&token_kinds);
            let ast = parser.parse().unwrap();
            let compiler = Compiler::new();
            let bytecode_program = compiler.compile_program(&ast);
            let mut vm = Vm::new();
            vm.interpret(&bytecode_program);
        });

        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_path("../testdata/output/");
        settings.bind(|| {
            insta::assert_snapshot!(snapshot_string.trim());
        });

        Ok(())
    }

    // https://stackoverflow.com/questions/72185130/how-to-capture-the-content-of-stdout-stderr-when-i-cannot-change-the-code-that-p
    fn capture_stdout<F>(f: F) -> String
    where
        F: FnOnce() -> (),
    {
        io::set_output_capture(Some(Default::default()));
        f();
        let captured = io::set_output_capture(None);
        let captured = Arc::try_unwrap(captured.unwrap()).unwrap();
        let captured = captured.into_inner().unwrap();
        let captured = String::from_utf8(captured).unwrap();
        captured
    }
}
