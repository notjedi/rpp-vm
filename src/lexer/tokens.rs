#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub(crate) enum Literal {
    BoolTrue,
    BoolFalse,
    Int(i64),
    Float(f64),
    Char(char),
    Str(String),
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub(crate) enum KeyWord {
    ProgramStart,
    ProgramEnd,
    Print,

    StartDeclare,
    Declare,
    Assign,
    DeclareAlt,

    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    Equal,
    NotEqual,

    Sum,
    Sub,
    Mul,
    Div,
    Mod,

    SemiColon,
    LeftBrace,
    RightBrace,

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

impl KeyWord {
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            KeyWord::ProgramStart => "LAKSHMI START",
            KeyWord::ProgramEnd => "MAGIZHCHI",
            KeyWord::Print => "DOT",

            KeyWord::StartDeclare => "AANDAVAN SOLLRAN",
            KeyWord::Declare => "ARUNACHALAM SEIYARAN",
            KeyWord::Assign => "BHAJJI SAAPDU",
            KeyWord::DeclareAlt => ":=",

            KeyWord::GreaterThan => ">",
            KeyWord::LessThan => "<",
            KeyWord::GreaterThanEqual => ">=",
            KeyWord::LessThanEqual => "<=",
            KeyWord::Equal => "==",
            KeyWord::NotEqual => "!=",

            KeyWord::Sum => "+",
            KeyWord::Sub => "-",
            KeyWord::Mul => "*",
            KeyWord::Div => "/",
            KeyWord::Mod => "%",

            KeyWord::SemiColon => ";",
            KeyWord::LeftBrace => "{",
            KeyWord::RightBrace => "}",

            KeyWord::IfCond => "EN PEAR MANICKAM",
            KeyWord::ElseCond => "ENAKKU INNURU PEAR IRUKKU",
            KeyWord::WhileLoop => "BABA COUNTING STARTS",
            KeyWord::ForStart => "NAA",
            KeyWord::ForRangeStart => "THADAVA SONNA(?! MADHRI)",
            KeyWord::ForRangeEnd => "THADAVA SONNA MADHRI",
            KeyWord::EndBlock => "kaTHAM KATHAM",
            KeyWord::BreakLoop => "bLACK SHEEP",
            KeyWord::FuncDeclare => "EN VAZHI THANI VAZHI",
            KeyWord::EndFunc => "marAKKADHINGA",
            KeyWord::FuncReturn => "IDHU EPDI IRUKKU",
            KeyWord::FuncCall => "chUMMA ADHURUDHULA",
        }
    }
}

impl KeyWord {
    pub(crate) fn is_binop(&self) -> bool {
        matches!(
            self,
            KeyWord::Sum | KeyWord::Sub | KeyWord::Mul | KeyWord::Div | KeyWord::Mod
        )
    }

    pub(crate) fn is_logicalop(&self) -> bool {
        matches!(
            self,
            KeyWord::GreaterThan
                | KeyWord::GreaterThanEqual
                | KeyWord::LessThan
                | KeyWord::LessThanEqual
                | KeyWord::Equal
                | KeyWord::NotEqual
        )
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub(crate) enum Token {
    Eof,
    Ident(String),
    Comment(String),
    KeyWord(KeyWord),
    Literal(Literal),
}

impl Token {
    pub(crate) fn is_binop(&self) -> bool {
        match self {
            Token::KeyWord(kw) => kw.is_binop(),
            _ => false,
        }
    }

    pub(crate) fn is_logicalop(&self) -> bool {
        match self {
            Token::KeyWord(kw) => kw.is_logicalop(),
            _ => false,
        }
    }
}
