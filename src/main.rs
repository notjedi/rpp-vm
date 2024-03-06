#![feature(let_chains)]
#![feature(variant_count)]
#![feature(inline_const_pat)]
#![feature(internal_output_capture)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]
#![allow(clippy::enum_variant_names)]

mod asm_codegen;
mod compiler;
mod interpreter;
mod lexer;
mod parser;
mod vm;

use crate::{
    compiler::Compiler,
    interpreter::{Interpreter, Visitable},
    lexer::{Lexer, TokenKind},
    parser::Parser,
    vm::Vm,
};

use color_eyre::eyre::Result;

const USE_COMPILER: bool = true;

fn main() -> Result<()> {
    color_eyre::install()?;
    let program = r#"
        EN VAZHI THANI VAZHI myfunc_one
            DOT "hi from myfunc_one func";
        MARAKKADHINGA

        EN VAZHI THANI VAZHI returning_one
            AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 100;
            IDHU EPDI IRUKKU ix;
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

            DOT;

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

            DOT;

            AANDAVAN SOLLRAN range ARUNACHALAM SEIYARAN 5;
            NAA 1 THADAVA SONNA range THADAVA SONNA MADHRI {
                DOT "Hi from for stmt";
            } KATHAM KATHAM;

            DOT;

            AANDAVAN SOLLRAN chumma ARUNACHALAM SEIYARAN 0;
            BABA COUNTING STARTS True {
                DOT chumma;
                chumma BHAJJI SAAPDU chumma + 1;
                AANDAVAN SOLLRAN hi_test ARUNACHALAM SEIYARAN 0;
                EN PEAR MANICKAM chumma >= 5 {
                    DOT "breaking out of loop...";
                    BLACK SHEEP;
                } KATHAM KATHAM;
            } KATHAM KATHAM;

            DOT;

            AANDAVAN SOLLRAN start ARUNACHALAM SEIYARAN 0;
            AANDAVAN SOLLRAN next_range ARUNACHALAM SEIYARAN 5;
            NAA start THADAVA SONNA next_range THADAVA SONNA MADHRI {
                DOT start;
                EN PEAR MANICKAM start >= 2 {
                    DOT "breaking out of loop...";
                    BLACK SHEEP;
                } KATHAM KATHAM;
            } KATHAM KATHAM;

            CHUMMA ADHURUDHULA myfunc_one;
            y CHUMMA ADHURUDHULA returning_one;
            DOT y;
        MAGIZHCHI
    "#;

    let tokens = Lexer::tokenize_str(program)?;
    let token_kinds = tokens
        .into_iter()
        .map(|tok| tok.kind)
        .collect::<Vec<TokenKind>>();
    let parser = Parser::new(&token_kinds);
    let ast = parser.parse().unwrap();
    if USE_COMPILER {
        // Bytecode interpreter
        let compiler = Compiler::new();
        let bytecode_program = compiler.compile_program(&ast);
        let mut vm = Vm::new();
        vm.interpret(&bytecode_program);
    } else {
        // Tree-walk interpreter
        let mut interpreter = Interpreter::new();
        ast.visit(&mut interpreter).unwrap();
    }
    Ok(())
}
