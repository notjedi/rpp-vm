#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Literal {
    BoolTrue,
    BoolFalse,
    Int(i64),
    Float(f64),
    Char(char),
    Str(String),
}

#[derive(Clone, Debug, PartialEq)]
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
    pub(crate) const fn as_str(&self) -> &'static str {
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
            KeyWord::ForRangeStart => "THADAVA SONNA",
            KeyWord::ForRangeEnd => "THADAVA SONNA MADHRI",
            KeyWord::EndBlock => "KATHAM KATHAM",
            KeyWord::BreakLoop => "BLACK SHEEP",
            KeyWord::FuncDeclare => "EN VAZHI THANI VAZHI",
            KeyWord::EndFunc => "MARAKKADHINGA",
            KeyWord::FuncReturn => "IDHU EPDI IRUKKU",
            KeyWord::FuncCall => "CHUMMA ADHURUDHULA",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum TokenKind {
    Eof,
    Ident(String),
    Comment(String),
    KeyWord(KeyWord),
    Literal(Literal),
}

impl Default for TokenKind {
    fn default() -> Self {
        Self::Eof
    }
}

impl Default for &TokenKind {
    fn default() -> &'static TokenKind {
        &TokenKind::Eof
    }
}

impl From<KeyWord> for TokenKind {
    fn from(val: KeyWord) -> Self {
        TokenKind::KeyWord(val)
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct Span {
    pub(crate) row: usize,
    pub(crate) col: usize,
    pub(crate) length: usize,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) span: Span,
}
