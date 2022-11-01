use std::fmt;

#[derive(Debug)]
#[allow(dead_code)]
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
    Word(String),
    String(String),
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
    ForRangeStart, // THADAVA SONNA(?! MADHRI)
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
        write!(f, "{:?}", self)
    }
}

#[cfg(test)]
mod tests {}
