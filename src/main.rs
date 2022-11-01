mod lexer;

fn main() {
    println!("{}", lexer::TokenKind::ProgramStart);
    println!("{:?}", lexer::TokenKind::ProgramStart);
}
