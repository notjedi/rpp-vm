mod lexer;

use lexer::tokens::Token;

fn main() {
    println!("{}", Token::ProgramStart);
    println!("{}", Token::Number(42));
    println!("{}", Token::Ident("foo".to_string()));
    println!("{}", Token::Literal("Meaning of life is 42?".to_string()));
    println!("{:?}", Token::ProgramStart);
}
