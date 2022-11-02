mod lexer;

fn main() {
    println!("{}", lexer::TokenKind::ProgramStart);
    println!("{}", lexer::TokenKind::Number(42));
    println!("{}", lexer::TokenKind::Ident("foo".to_string()));
    println!(
        "{}",
        lexer::TokenKind::Literal("Meaning of life is 42?".to_string())
    );
    println!("{:?}", lexer::TokenKind::ProgramStart);
}
