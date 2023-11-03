#![allow(incomplete_features)]
#![feature(inline_const_pat)]

mod lexer;
use lexer::Lexer;

fn main() {
    let program = r#"
            EN VAZHI THANI VAZHI myfunc_one
                DOT "Hello from myfunc_one!";
                AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 100;
                DOT "returning ix =" ix "to main";
                IDHU EPDI IRUKKU ix;
            MARAKKADHINGA

            LAKSHMI START
            DOT "Hi from main!";
            y CHUMMA ADHURUDHULA myfunc_one;
            DOT "Value returned from myfunc_one:" y;
            MAGIZHCHI
        "#;

    let tokens = Lexer::tokenize(program).unwrap();
    println!("{tokens:?}");
}
