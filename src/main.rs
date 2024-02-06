#![feature(let_chains)]
#![feature(inline_const_pat)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]
#![allow(clippy::enum_variant_names)]
#![allow(illegal_floating_point_literal_pattern)]

// mod compiler;
mod interpreter;
mod lexer;
mod parser;
// mod vm;

use crate::{
    lexer::{Lexer, TokenKind},
    parser::Parser,
};

use color_eyre::eyre::Result;

const USE_COMPILER: bool = true;

fn main() -> Result<()> {
    color_eyre::install()?;
    let program = r#"
        LAKSHMI START
            DOT "hi from myfunc_one func";
        MAGIZHCHI
    "#;

    let tokens = Lexer::tokenize_str(program)?;
    let token_kinds = tokens
        .into_iter()
        .map(|tok| tok.kind)
        .collect::<Vec<TokenKind>>();
    let parser = Parser::new(&token_kinds);
    let ast = parser.parse().unwrap();
    dbg!(ast);
    Ok(())
}
