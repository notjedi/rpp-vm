use std::collections::HashMap;

use crate::{
    compiler::{Instruction, ProgFunction},
    interpreter::Value,
};

#[derive(Debug)]
pub(crate) struct CallFrame {
    program: ProgFunction,
    stack_offset: usize,
    ip: usize,
}

#[derive(Debug)]
pub(crate) struct Vm {
    value_stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    globals: HashMap<String, Value>,
}

impl Vm {
    pub(crate) fn new() -> Self {
        Self {
            call_stack: vec![],
            value_stack: vec![],
            globals: HashMap::new(),
        }
    }

    fn current_frame(&self) -> &CallFrame {
        self.call_stack.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.call_stack.last_mut().unwrap()
    }

    fn pop_stack(&mut self) -> Value {
        self.value_stack.pop().expect("no values to pop from stack")
    }

    fn inc_ip(&mut self, offset: usize) {
        self.current_frame_mut().ip += offset;
    }

    fn dec_ip(&mut self, offset: usize) {
        self.current_frame_mut().ip -= offset;
    }

    pub(crate) fn interpret(&mut self, program: &ProgFunction) {
        self.call_stack.push(CallFrame {
            program: program.clone(),
            stack_offset: 0,
            ip: 0,
        });
        loop {
            let instr = self.current_frame().program.bytecode.instructions[self.current_frame().ip];
            match instr {
                Instruction::Add => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a + b;
                    self.value_stack.push(res);
                }
                Instruction::Constant(idx) => {
                    // TODO: bro, i need to find a way out of clones
                    let val = self.current_frame().program.bytecode.constants[idx].clone();
                    self.value_stack.push(val);
                }
                Instruction::Divide => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = a / b;
                    self.value_stack.push(res);
                }
                Instruction::Equal => todo!(),
                Instruction::GetGlobal(idx) => todo!(),
                Instruction::GetLocal(idx) => {
                    let stack_offset = self.current_frame().stack_offset;
                    self.value_stack
                        .push(self.value_stack[stack_offset + idx].clone());
                }
                Instruction::Greater => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    let res = Value::Bool(a > b);
                    self.value_stack.push(res);
                }
                Instruction::Jump(offset) => {
                    self.inc_ip(offset);
                    continue;
                }
                Instruction::JumpIfFalse(offset) => {
                    if let Some(Value::Bool(val)) = self.value_stack.last() {
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
                    let res = Value::Bool(a < b);
                    self.value_stack.push(res);
                }
                Instruction::Loop(offset) => {
                    self.dec_ip(offset);
                    continue;
                }
                Instruction::Method(_) => todo!(),
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
                    if let Value::Bool(val) = self.pop_stack() {
                        self.value_stack.push(Value::Bool(!val));
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
                Instruction::SetGlobal(idx) => todo!(),
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
