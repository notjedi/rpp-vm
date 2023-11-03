#![allow(incomplete_features)]
#![feature(inline_const_pat)]

mod lexer;
use lexer::Lexer;

fn main() {
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
}
