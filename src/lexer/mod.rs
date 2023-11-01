mod tokens;
use std::str::Chars;

pub(crate) use tokens::Token;

pub(crate) struct Lexer<'a> {
    input: Chars<'a>,
}
