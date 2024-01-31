#![feature(let_chains)]
#![feature(inline_const_pat)]
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
            AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 100;
            ix BHAJJI SAAPDU ix + 1;
            DOT "returning ix =" ix "to main";
            EN PEAR MANICKAM ix >= 5 {
                DOT "ix is not less than 5";
            } ENAKKU INNURU PEAR IRUKKU { 
                DOT "ix is less than 5";
            } KATHAM KATHAM;
            DOT "out of the if statement";
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
