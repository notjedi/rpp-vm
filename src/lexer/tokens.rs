use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<'a> {
    ProgramStart,
    ProgramEnd,
    Comment(&'a str),

    Print,

    SemiColon,
    LeftBrace,
    RightBrace,

    Number(i64),
    Float(f64),
    Ident(&'a str),
    Literal(&'a str),

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
}

impl<'a> Token<'a> {
    pub fn is_mathop(token: &Token) -> bool {
        match token {
            Token::Sum | Token::Sub | Token::Mul | Token::Div | Token::Mod => true,
            _ => false,
        }
    }

    pub fn is_logicalop(token: &Token) -> bool {
        match token {
            Token::GreaterThan
            | Token::GreaterThanEqual
            | Token::LessThan
            | Token::LessThanEqual
            | Token::Equal
            | Token::NotEqual => true,
            _ => false,
        }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.get_name())
    }
}

impl<'a> Token<'a> {
    fn get_name(&self) -> String {
        match self {
            Token::ProgramStart => String::from("LAKSHMI START"),
            Token::ProgramEnd => "MAGIZHCHI".to_string(),

            Token::Number(num) => num.to_string(),
            Token::Float(num) => num.to_string(),
            Token::Ident(ident) => ident.to_string(),
            Token::Literal(literal) => literal.to_string(),

            Token::BoolTrue => String::from("true"),
            Token::BoolFalse => String::from("false"),

            Token::SemiColon => String::from(";"),
            Token::LeftBrace => String::from("{"),
            Token::RightBrace => String::from("}"),
            Token::Sum => String::from("+"),
            Token::Sub => String::from("-"),
            Token::Mul => String::from("*"),
            Token::Div => String::from("/"),
            Token::Mod => String::from("%"),
            Token::GreaterThan => String::from(">"),
            Token::GreaterThanEqual => String::from(">="),
            Token::LessThan => String::from("<"),
            Token::LessThanEqual => String::from("<="),
            Token::Equal => String::from("=="),
            Token::NotEqual => String::from("!="),

            Token::Print => String::from("DOT"),
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
            Token::Comment(_) => String::from("Comment"),
        }
    }
}

impl<'a> From<&'a str> for Token<'a> {
    fn from(value: &'a str) -> Token<'a> {
        match value {
            "LAKSHMI START" => Token::ProgramStart,
            "MAGIZHCHI" => Token::ProgramEnd,
            "DOT" => Token::Print,

            ";" => Token::SemiColon,
            "{" => Token::LeftBrace,
            "}" => Token::RightBrace,

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

            _ => Token::Literal(value),
        }
    }
}
