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
        loop {
            let instr = &program.instructions[self.ip];
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
                Instruction::GetLocal(idx) => self.stack.push(self.stack[*idx].clone()),
                Instruction::Greater => todo!(),
                Instruction::Jump(offset) => {
                    self.ip += offset;
                    continue;
                }
                Instruction::JumpIfFalse(offset) => {
                    if let Some(Value::Bool(val)) = self.stack.last() {
                        if !val {
                            self.ip += offset;
                            continue;
                        }
                    } else {
                        todo!("return runtime time error")
                    }
                }
                Instruction::Less => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = Value::Bool(a < b);
                    self.stack.push(res);
                }
                Instruction::Loop(offset) => {
                    self.ip -= offset;
                    continue;
                }
                Instruction::Method(_) => todo!(),
                Instruction::Mod => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a % b;
                    self.stack.push(res);
                }
                Instruction::Multiply => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a * b;
                    self.stack.push(res);
                }
                Instruction::Not => {
                    if let Value::Bool(val) = self.pop_stack() {
                        self.stack.push(Value::Bool(!val));
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
                    return;
                }
                Instruction::SetLocal(idx) => {
                    self.stack[*idx] = self.stack.last().expect("stack is empty").clone();
                }
                Instruction::Substract => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a - b;
                    self.stack.push(res);
                }
            }
            self.ip += 1;
        }
    }
}
