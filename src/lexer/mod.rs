mod tokens;
pub use tokens::Token;

use anyhow::Result;

pub struct Lexer {
    input: Vec<char>,
    read_pos: usize, // moving reading position
    pos: usize,      // reading position
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input: input.chars().collect(),
            read_pos: 0,
            pos: 0,
            ch: ' ',
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_pos < self.input.len() {
            self.ch = self.input[self.read_pos];
            self.pos = self.read_pos;
            self.read_pos += 1;
        }
    }

    fn skip_whitespace(&mut self) {
        while (self.ch == ' ' || self.ch == '\n') && self.read_pos < self.input.len() {
            self.read_char();
        }
    }

    fn read_ident(&mut self) -> String {
        let pos = self.pos;
        while self.ch.is_ascii_alphabetic() || self.ch == '_' {
            self.read_char();
        }

        String::from_utf8_lossy(
            &self.input[pos..self.pos]
                .iter()
                .map(|c| *c as u8)
                .collect::<Vec<u8>>(),
        )
        .to_string()
    }

    fn read_int(&mut self) -> String {
        let pos = self.pos;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        String::from_utf8_lossy(
            &self.input[pos..self.pos]
                .iter()
                .map(|c| *c as u8)
                .collect::<Vec<u8>>(),
        )
        .to_string()
    }

    fn read_line(&mut self) -> String {
        let pos = self.pos;
        while self.ch != '\n' {
            self.read_char();
        }

        String::from_utf8_lossy(
            &self.input[pos..self.pos]
                .iter()
                .map(|c| *c as u8)
                .collect::<Vec<u8>>(),
        )
        .to_string()
    }

    fn read_literal(&mut self) -> String {
        let pos = self.pos;
        self.read_char();
        while self.ch != '"' {
            self.read_char();
        }
        self.read_char();

        String::from_utf8_lossy(
            &self.input[pos..self.pos]
                .iter()
                .map(|c| *c as u8)
                .collect::<Vec<u8>>(),
        )
        .to_string()
    }

    pub fn reset(&mut self) {
        self.ch = ' ';
        self.pos = 0;
        self.read_pos = 0;
    }

    pub fn peek(&mut self) -> char {
        if self.read_pos >= self.input.len() {
            return 0 as char;
        } else {
            return self.input[self.read_pos];
        }
    }

    pub fn next_token(&mut self) -> Result<Token, &'static str> {
        if self.read_pos >= self.input.len() {
            println!("breaking");
            return Err("end of input");
        }
        self.skip_whitespace();

        // println!(
        //     "{} {} {} {} {} {} {} {}",
        //     self.pos,
        //     self.ch,
        //     self.read_pos,
        //     self.input.len(),
        //     self.input[self.pos],
        //     self.input[self.pos] as u8,
        //     self.input[self.read_pos],
        //     self.input[self.read_pos] as u8,
        // );

        let token = match self.ch {
            '+' => Token::Sum,
            '-' => Token::Sub,
            '*' => Token::Mul,
            '/' => Token::Div,
            '%' => Token::Mod,

            ';' => Token::SemiColon,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,

            '<' => {
                if self.peek() == '=' {
                    self.read_char();
                    Token::LessThanEqual
                } else {
                    Token::LessThan
                }
            }

            '>' => {
                if self.peek() == '=' {
                    self.read_char();
                    Token::GreaterThanEqual
                } else {
                    Token::GreaterThan
                }
            }

            '!' => {
                let next_char = self.peek();
                if next_char == '!' {
                    return Ok(Token::Comment(self.read_line()));
                } else if next_char == '=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    unreachable!()
                }
            }

            '=' => {
                if self.peek() == '=' {
                    self.read_char();
                    Token::Equal
                } else {
                    unreachable!()
                }
            }

            ':' => {
                if self.peek() == '=' {
                    self.read_char();
                    Token::DeclareAlt
                } else {
                    unreachable!()
                }
            }

            '"' => return Ok(Token::Literal(self.read_literal())),

            'a'..='z' | 'A'..='Z' | '_' => {
                let tok = match self.read_ident().as_str() {
                    "LAKSHMI START" => Token::ProgramStart,
                    "MAGIZHCHI" => Token::ProgramEnd,
                    "DOT" => Token::Print,

                    "true" => Token::BoolTrue,
                    "false" => Token::BoolFalse,

                    "AANDAVAN SOLLRAN" => Token::StartDeclare,
                    "ARUNACHALAM SEIYARAN" => Token::Declare,
                    "BHAJJI SAAPDU" => Token::Assign,

                    "EN PEAR MANICKAM" => Token::IfCond,
                    "ENAKKU INNURU PEAR IRUKKU" => Token::ElseCond,
                    "BABA COUNTING STARTS" => Token::WhileLoop,
                    "NAA" => Token::ForStart,
                    "THADAVA SONNA" => Token::ForRangeStart,
                    "THADAVA SONNA MADHRI" => Token::ForRangeEnd,
                    "KATHAM KATHAM" => Token::EndBlock,
                    "BLACK SHEEP" => Token::BreakLoop,
                    "EN VAZHI THANI VAZHI" => Token::FuncDeclare,
                    "MARAKKADHINGA" => Token::EndFunc,
                    "IDHU EPDI IRUKKU" => Token::FuncReturn,
                    "CHUMMA ADHURUDHULA" => Token::FuncCall,

                    ident => Token::Ident(ident.to_string()),
                };
                return Ok(tok);
            }

            '0'..='9' => return Ok(Token::Number(self.read_int())),
            _ => unreachable!(),
        };

        self.read_char();
        Ok(token)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Token;

    use super::Lexer;
    use anyhow::Result;

    #[test]
    fn test_fizz_buzz() -> Result<()> {
        let program = r#"
            LAKSHMI START

            !! declare variables
            AANDAVAN SOLLRAN ix ARUNACHALAM SEIYARAN 1;
            AANDAVAN SOLLRAN range ARUNACHALAM SEIYARAN 16;

            NAA 1 THADAVA SONNA range THADAVA SONNA MADHRI{
                EN PEAR MANICKAM ix%15==0{
                    DOT "FizzBuzz";
                } ENAKKU INNURU PEAR IRUKKU{
                    EN PEAR MANICKAM ix%3==0{
                        DOT "Fizz";
                    } ENAKKU INNURU PEAR IRUKKU{
                        EN PEAR MANICKAM ix%5==0{
                            DOT "Buzz";
                        } ENAKKU INNURU PEAR IRUKKU{
                            DOT ix;
                        }KATHAM KATHAM;
                    }KATHAM KATHAM;
                }KATHAM KATHAM;
                ix BHAJJI SAAPDU ix + 1;
            !! End Loop
            }KATHAM KATHAM;
            MAGIZHCHI
        "#;

        let mut lexer = Lexer::new(program.to_string());
        loop {
            match lexer.next_token() {
                Ok(token) => println!("{:?}", token),
                Err(_) => break,
            }
        }

        Ok(())
    }
}
