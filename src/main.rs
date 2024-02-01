#![feature(let_chains)]
#![feature(inline_const_pat)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]
#![allow(clippy::enum_variant_names)]
#![allow(illegal_floating_point_literal_pattern)]

mod compiler;
mod interpreter;
mod lexer;
mod parser;
mod vm;

use crate::parser::Parser;
use crate::{lexer::Lexer, vm::Vm};

use color_eyre::eyre::Result;
use compiler::Compiler;
use interpreter::{Interpreter, Visitable};

const USE_COMPILER: bool = true;

fn main() -> Result<()> {
    color_eyre::install()?;
    let program = r#"
        EN VAZHI THANI VAZHI myfunc_one
            25 + 15;
        MARAKKADHINGA

        LAKSHMI START
            AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 0;
            EN PEAR MANICKAM ix >= 5 {
                AANDAVAN SOLLRAN hii ARUNACHALAM SEIYARAN 0;
                DOT "ix is not less than 5";
            } ENAKKU INNURU PEAR IRUKKU {
                AANDAVAN SOLLRAN hii ARUNACHALAM SEIYARAN 0;
                DOT "ix is less than 5";
            } KATHAM KATHAM;
            DOT "out of the if statement, ix is now =" ix;

            !! TODO: support for statements
            !! TODO: support break statements

            AANDAVAN SOLLRAN temp ARUNACHALAM SEIYARAN 0;
            BABA COUNTING STARTS temp < 2 {
                AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 0;
                BABA COUNTING STARTS ix < 5 {
                    DOT ix;
                    ix BHAJJI SAAPDU ix + 1;
                } KATHAM KATHAM;
                temp BHAJJI SAAPDU temp + 1;
                DOT "done with outer loop";
            } KATHAM KATHAM;

            AANDAVAN SOLLRAN hi ARUNACHALAM SEIYARAN 100;
            hi BHAJJI SAAPDU hi + 1;
            DOT hi;
        MAGIZHCHI
    "#;

    let tokens = Lexer::tokenize_str(program)?;
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;
    if USE_COMPILER {
        // Bytecode interpreter
        let compiler = Compiler::new();
        let bytecode_program = compiler.compile_program(&ast);
        dbg!(&bytecode_program);
        let mut vm = Vm::new();
        vm.interpret(&bytecode_program);
        dbg!(vm);
    } else {
        // Tree-walk interpreter
        let mut interpreter = Interpreter::new();
        ast.visit(&mut interpreter)?;
    }
    Ok(())
}
