#![feature(let_chains)]
#![feature(inline_const_pat)]
#![allow(incomplete_features)]
#![allow(clippy::enum_variant_names)]

mod interpreter;
mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::Parser;

use color_eyre::eyre::Result;
use interpreter::{Interpreter, Visitable};

fn main() -> Result<()> {
    color_eyre::install()?;
    let program = r#"
        LAKSHMI START
            DOT "Hello, world!";
        MAGIZHCHI
    "#;

    let tokens = Lexer::tokenize_str(program)?;
    let mut parser = Parser::new(tokens);
    let mut ast = parser.parse()?;
    let mut interpreter = Interpreter::new();
    ast.visit(&mut interpreter);
    Ok(())
}
