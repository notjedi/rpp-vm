use std::collections::HashMap;

use crate::{
    compiler::{Bytecode, Instruction, ProgFunction},
    interpreter::Value,
};

// TODO: how does python go about this?
// there is a local variable with same name as global function
// 1. user tries to mutate the local variable using the same identifier
// 2. user tries to call the global function using the same identifier
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

    fn current_bytecode(&self) -> &Bytecode {
        &self.current_frame().program.bytecode
    }

    pub(crate) fn interpret(&mut self, program: &ProgFunction) {
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
                Instruction::Equal => todo!(),
                Instruction::GetGlobal(idx) => {
                    let global_name = self.current_bytecode().constants[idx].clone();
                    if let Value::Str(name) = global_name {
                        if let Some(val) = self.globals.get(&(*name)) {
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
                Instruction::Method(idx) => {
                    let func_name = &self.current_bytecode().constants[idx];
                    if let Value::Str(name) = func_name {
                        let func_val = self
                            .globals
                            .get(&(**name))
                            .expect("can't find function")
                            .clone();
                        if let Value::ProgFunction(func) = func_val {
                            let func_call_frame = CallFrame {
                                program: func,
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
                    if let Value::Str(name) = glob_name {
                        match self.globals.get_mut(&(**name)) {
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
