use crate::{
    compiler::{CompiledProgram, Instruction},
    interpreter::Value,
};

#[derive(Debug)]
pub(crate) struct Vm {
    stack: Vec<Value>,
    ip: usize,
}

impl Vm {
    pub(crate) fn new() -> Self {
        Self {
            stack: vec![],
            ip: 0,
        }
    }

    pub(crate) fn interpret(&mut self, program: &CompiledProgram) {
        for instr in &program.instructions {
            match instr {
                Instruction::Add => {
                    let b = self.stack.pop().expect("no values to pop from stack");
                    let a = self.stack.pop().expect("no values to pop from stack");
                    let res = a + b;
                    self.stack.push(res);
                }
                Instruction::Call(_) => todo!(),
                Instruction::Closure(_) => todo!(),
                Instruction::Constant(idx) => {
                    // TODO: bro, i need to find a way out of clones
                    let val = program.constants[*idx].clone();
                    self.stack.push(val);
                }
                Instruction::Divide => todo!(),
                Instruction::Equal => todo!(),
                Instruction::False => todo!(),
                Instruction::GetLocal(_) => todo!(),
                Instruction::Greater => todo!(),
                Instruction::Invoke(_) => todo!(),
                Instruction::Jump(_) => todo!(),
                Instruction::JumpIfFalse(_) => todo!(),
                Instruction::Less => todo!(),
                Instruction::Loop(_) => todo!(),
                Instruction::Method(_) => todo!(),
                Instruction::Multiply => todo!(),
                Instruction::Negate => todo!(),
                Instruction::Not => todo!(),
                Instruction::Pop => todo!(),
                Instruction::Print => todo!(),
                Instruction::Return => todo!(),
                Instruction::SetGlobal(_) => todo!(),
                Instruction::SetLocal(_) => todo!(),
                Instruction::Substract => todo!(),
                Instruction::True => todo!(),
            }
        }
    }
}
