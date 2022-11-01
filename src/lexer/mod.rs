use std::fmt;
// use std::string::String;

#[derive(Debug)]
#[allow(dead_code)] // TODO:  remove this
pub enum TokenKind {
    ProgramStart, // LAKSHMI START
    ProgramEnd,   // MAGIZHCHI

    Print, // DOT

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
    StartDeclare, // AANDAVAN SOLLRAN
    Declare,      // ARUNACHALAM SEIYARAN
    Assign,       // BHAJJI SAAPDU
    DeclareAlt,   // :=

    // Flow Control
    IfCond,        // EN PEAR MANICKAM
    ElseCond,      // ENAKKU INNURU PEAR IRUKKU
    WhileLoop,     // BABA COUNTING STARTS
    ForStart,      // NAA
    ForRangeStart, // THADAVA SONNA
    ForRangeEnd,   // THADAVA SONNA MADHRI
    EndBlock,      // KATHAM KATHAM
    BreakLoop,     // BLACK SHEEP
    FuncDeclare,   // EN VAZHI THANI VAZHI
    EndFunc,       // MARAKKADHINGA
    FuncReturn,    // IDHU EPDI IRUKKU
    FuncCall,      // CHUMMA ADHURUDHULA
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.get_name())
    }
}

impl TokenKind {
    fn get_name(&self) -> String {
        match self {
            TokenKind::ProgramStart => String::from("LAKSHMI START"),
            TokenKind::ProgramEnd => "MAGIZHCHI".to_string(),

            TokenKind::Print => String::from("DOT"),

            TokenKind::SemiColon => String::from(";"),
            TokenKind::LeftBrace => String::from("{"),
            TokenKind::RightBrace => String::from("}"),

            TokenKind::Number(num) => num.to_string(),
            TokenKind::Ident(ident) => ident.to_string(),
            TokenKind::Literal(literal) => literal.to_string(),
            TokenKind::BoolTrue => String::from("true"),
            TokenKind::BoolFalse => String::from("false"),

            TokenKind::Sum => String::from("+"),
            TokenKind::Sub => String::from("-"),
            TokenKind::Mul => String::from("*"),
            TokenKind::Div => String::from("/"),
            TokenKind::Mod => String::from("%"),
            TokenKind::GreaterThan => String::from(">"),
            TokenKind::LessThan => String::from("<"),
            TokenKind::GreaterThanEqual => String::from(">="),
            TokenKind::LessThanEqual => String::from("<="),
            TokenKind::Equal => String::from("=="),
            TokenKind::NotEqual => String::from("!="),

            TokenKind::StartDeclare => String::from("AANDAVAN SOLLRAN"),
            TokenKind::Declare => String::from("ARUNACHALAM SEIYARAN"),
            TokenKind::Assign => String::from("BHAJJI SAAPDU"),
            TokenKind::DeclareAlt => String::from(":="),

            TokenKind::IfCond => String::from("EN PEAR MANICKAM"),
            TokenKind::ElseCond => String::from("ENAKKU INNURU PEAR IRUKKU"),
            TokenKind::WhileLoop => String::from("BABA COUNTING STARTS"),
            TokenKind::ForStart => String::from("NAA"),
            TokenKind::ForRangeStart => String::from("THADAVA SONNA"),
            TokenKind::ForRangeEnd => String::from("THADAVA SONNA MADHRI"),
            TokenKind::EndBlock => String::from("KATHAM KATHAM"),
            TokenKind::BreakLoop => String::from("BLACK SHEEP"),
            TokenKind::FuncDeclare => String::from("EN VAZHI THANI VAZHI"),
            TokenKind::EndFunc => String::from("MARAKKADHINGA"),
            TokenKind::FuncReturn => String::from("IDHU EPDI IRUKKU"),
            TokenKind::FuncCall => String::from("CHUMMA ADHURUDHULA"),
        }
    }
}

#[cfg(test)]
mod tests {}
