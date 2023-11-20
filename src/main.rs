#![feature(let_chains)]
#![feature(inline_const_pat)]
#![allow(incomplete_features)]
#![allow(clippy::enum_variant_names)]

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
                51 % 5 * 10 / 2;

                BABA COUNTING STARTS True{
                    DOT ix;
                    ix BHAJJI SAAPDU ix + 1;
                    EN PEAR MANICKAM ix >= 5{
                        BLACK SHEEP;
                    }KATHAM KATHAM;
                }KATHAM KATHAM;
            MARAKKADHINGA
        "#;

    let tokens = Lexer::tokenize_str(program).unwrap();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    dbg!(ast);
    Ok(())
}
