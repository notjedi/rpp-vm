pub mod tokens;

use tokens::TokenKind;

pub struct Lexer {
    input: Vec<char>, // Source code
    pub pos: usize,   // Reading position
}

#[cfg(test)]
mod tests {}
