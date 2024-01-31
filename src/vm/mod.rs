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

    fn pop_stack(&mut self) -> Value {
        self.stack.pop().expect("no values to pop from stack")
    }

    pub(crate) fn interpret(&mut self, program: &CompiledProgram) {
        for instr in &program.instructions {
            match instr {
                Instruction::Add => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a + b;
                    self.stack.push(res);
                }
                Instruction::Constant(idx) => {
                    // TODO: bro, i need to find a way out of clones
                    let val = program.constants[*idx].clone();
                    self.stack.push(val);
                }
                Instruction::Divide => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a / b;
                    self.stack.push(res);
                }
                Instruction::Equal => todo!(),
                Instruction::False => todo!(),
                Instruction::GetLocal(idx) => self.stack.push(self.stack[*idx].clone()),
                Instruction::Greater => todo!(),
                Instruction::Jump(_) => todo!(),
                Instruction::JumpIfFalse(_) => todo!(),
                Instruction::Less => todo!(),
                Instruction::Loop(_) => todo!(),
                Instruction::Method(_) => todo!(),
                Instruction::Mod => todo!(),
                Instruction::Multiply => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a * b;
                    self.stack.push(res);
                }
                Instruction::Not => todo!(),
                Instruction::Pop => {
                    self.pop_stack();
                }
                Instruction::Print => {
                    if let Value::Str(str_val) = self.pop_stack() {
                        println!("{}", str_val);
                    } else {
                        //
                        todo!("return compile time error")
                    }
                }
                Instruction::Return => todo!(),
                Instruction::SetLocal(idx) => {
                    self.stack[*idx] = self.stack.last().expect("stack is empty").clone();
                }
                Instruction::Substract => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a - b;
                    self.stack.push(res);
                }
                Instruction::True => todo!(),
            }
        }
    }
}
