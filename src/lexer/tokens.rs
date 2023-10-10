use std::fmt;

#[derive(Debug)]
#[allow(dead_code)] // TODO:  remove this
pub enum Token {
    ProgramStart,
    ProgramEnd,

    Print,

    // Symbols
    SemiColon,
    LeftBrace,
    RightBrace,

    // Vars
    Number(u64),
    Ident(String),
    Literal(String),
    BoolTrue,
    BoolFalse,

    // Math ops
    Sum,
    Sub,
    Mul,
    Div,
    Mod,

    // Logical ops
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    Equal,
    NotEqual,

    // rajini++ commands
    StartDeclare,
    Declare,
    Assign,
    DeclareAlt,

    // Flow Control
    IfCond,
    ElseCond,
    WhileLoop,
    ForStart,
    ForRangeStart,
    ForRangeEnd,
    EndBlock,
    BreakLoop,
    FuncDeclare,
    EndFunc,
    FuncReturn,
    FuncCall,

    Illegal,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.get_name())
    }
}

impl Token {
    fn get_name(&self) -> String {
        match self {
            Token::ProgramStart => String::from("LAKSHMI START"),
            Token::ProgramEnd => "MAGIZHCHI".to_string(),

            Token::Print => String::from("DOT"),

            Token::SemiColon => String::from(";"),
            Token::LeftBrace => String::from("{"),
            Token::RightBrace => String::from("}"),

            Token::Number(num) => num.to_string(),
            Token::Ident(ident) => ident.to_string(),
            Token::Literal(literal) => literal.to_string(),
            Token::BoolTrue => String::from("true"),
            Token::BoolFalse => String::from("false"),

            Token::Sum => String::from("+"),
            Token::Sub => String::from("-"),
            Token::Mul => String::from("*"),
            Token::Div => String::from("/"),
            Token::Mod => String::from("%"),
            Token::GreaterThan => String::from(">"),
            Token::LessThan => String::from("<"),
            Token::GreaterThanEqual => String::from(">="),
            Token::LessThanEqual => String::from("<="),
            Token::Equal => String::from("=="),
            Token::NotEqual => String::from("!="),

            Token::StartDeclare => String::from("AANDAVAN SOLLRAN"),
            Token::Declare => String::from("ARUNACHALAM SEIYARAN"),
            Token::Assign => String::from("BHAJJI SAAPDU"),
            Token::DeclareAlt => String::from(":="),

            Token::IfCond => String::from("EN PEAR MANICKAM"),
            Token::ElseCond => String::from("ENAKKU INNURU PEAR IRUKKU"),
            Token::WhileLoop => String::from("BABA COUNTING STARTS"),
            Token::ForStart => String::from("NAA"),
            Token::ForRangeStart => String::from("THADAVA SONNA"),
            Token::ForRangeEnd => String::from("THADAVA SONNA MADHRI"),
            Token::EndBlock => String::from("KATHAM KATHAM"),
            Token::BreakLoop => String::from("BLACK SHEEP"),
            Token::FuncDeclare => String::from("EN VAZHI THANI VAZHI"),
            Token::EndFunc => String::from("MARAKKADHINGA"),
            Token::FuncReturn => String::from("IDHU EPDI IRUKKU"),
            Token::FuncCall => String::from("CHUMMA ADHURUDHULA"),

            Token::Illegal => String::from("Illegal"),
        }
    }
}

impl From<&str> for Token {
    fn from(value: &str) -> Self {
        match value {
            "LAKSHMI START" => Token::ProgramStart,
            "MAGIZHCHI" => Token::ProgramEnd,

            "DOT" => Token::Print,

            ";" => Token::SemiColon,
            "{" => Token::LeftBrace,
            "}" => Token::RightBrace,

            // num.to_string() => TokenKind::Number(num),
            // ident.to_string() => TokenKind::Ident(ident),
            // literal.to_string() => TokenKind::Literal(literal),
            "true" => Token::BoolTrue,
            "false" => Token::BoolFalse,

            "+" => Token::Sum,
            "-" => Token::Sub,
            "*" => Token::Mul,
            "/" => Token::Div,
            "%" => Token::Mod,
            ">" => Token::GreaterThan,
            "<" => Token::LessThan,
            ">=" => Token::GreaterThanEqual,
            "<=" => Token::LessThanEqual,
            "==" => Token::Equal,
            "!=" => Token::NotEqual,

            "AANDAVAN SOLLRAN" => Token::StartDeclare,
            "ARUNACHALAM SEIYARAN" => Token::Declare,
            "BHAJJI SAAPDU" => Token::Assign,
            ":=" => Token::DeclareAlt,

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
            _ => Token::Illegal,
        }
    }
}
