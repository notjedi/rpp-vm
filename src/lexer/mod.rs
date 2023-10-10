pub mod tokens;

use tokens::Token;

pub struct Lexer {
    input: Vec<char>, // Source code
    pos: usize,       // Reading position
}

#[cfg(test)]
mod tests {}
