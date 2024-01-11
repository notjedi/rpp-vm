#![feature(let_chains)]
#![feature(inline_const_pat)]
#![allow(incomplete_features)]
#![allow(clippy::enum_variant_names)]
#![allow(illegal_floating_point_literal_pattern)]

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
            AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 100;
            DOT "printing ix =" ix "that was declared before";
            ix BHAJJI SAAPDU ix+1;
            DOT "printing ix =" ix "after it was updated";
            DOT "is ix > 10 = " ix > 10;
            DOT "is ix < 10 = " ix < 10;
            DOT "is ix <= 10 = " ix <= 10;
            DOT "is ix >= 10 = " ix >= 10;
            DOT "is ix == 10 = " ix == 10;
            DOT "is ix != 10 = " ix != 10;
            DOT "\nis ix == True = " ix == True;
            DOT "is ix == 101.0 = " ix == 101.0 "\n";

            !! DOT "-ix is" -ix; = parses as a sub op b/w string and int
            DOT -ix;
            DOT +ix;

            DOT "printing ix =" ix;
        MAGIZHCHI
    "#;

    let tokens = Lexer::tokenize_str(program)?;
    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;
    let mut interpreter = Interpreter::new();
    ast.visit(&mut interpreter)?;
    Ok(())
}
