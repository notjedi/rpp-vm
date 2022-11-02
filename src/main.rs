mod lexer;

use lexer::tokens::TokenKind;

fn main() {
    println!("{}", TokenKind::ProgramStart);
    println!("{}", TokenKind::Number(42));
    println!("{}", TokenKind::Ident("foo".to_string()));
    println!(
        "{}",
        TokenKind::Literal("Meaning of life is 42?".to_string())
    );
    println!("{:?}", TokenKind::ProgramStart);
}
