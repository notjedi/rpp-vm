#![feature(let_chains)]
#![feature(inline_const_pat)]
#![allow(incomplete_features)]

mod lexer;
mod parser;
use color_eyre::eyre::Result;
use lexer::Lexer;

fn main() -> Result<()> {
    color_eyre::install()?;
    let program = r#"
            BABA COUNTING STARTS True{
                DOT ix;
                ix BHAJJI SAAPDU ix + 1;
                EN PEAR MANICKAM ix >= 5{
                    DOT "breaking out of loop...";
                    BLACK SHEEP;
                }KATHAM KATHAM;
            }KATHAM KATHAM;
        "#;

    let tokens = Lexer::tokenize_str(program).unwrap();
    println!("{tokens:?}");
    Ok(())
}
