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

fn main() -> Result<()> {
    color_eyre::install()?;
    let program = r#"
            EN VAZHI THANI VAZHI myfunc_one
                AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 100;
                DOT "returning ix =" ix "to main";
                IDHU EPDI IRUKKU ix;
                1 + 2 - 3 * 4 + 5 / 6 - 8 * 9;

                BABA COUNTING STARTS True{
                    DOT ix;
                    ix BHAJJI SAAPDU ix + 1;
                    !! EN PEAR MANICKAM ix >= 5{
                    EN PEAR MANICKAM ix % 3 == ix % 6 {
                        BLACK SHEEP;
                    }KATHAM KATHAM;
                }KATHAM KATHAM;
            MARAKKADHINGA

            LAKSHMI START
            MAGIZHCHI
        "#;

    let tokens = Lexer::tokenize_str(program).unwrap();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    dbg!(ast);
    Ok(())
}
