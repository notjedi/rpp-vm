#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Literal<'a> {
    BoolTrue,
    BoolFalse,
    Int(i64),
    Float(f64),
    Char(char),
    Str(&'a str),
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
pub(crate) enum TokenKind<'a> {
    Eof,
    Ident(&'a str),
    Comment(&'a str),
    KeyWord(KeyWord),
    Literal(Literal<'a>),
}

impl<'a> Default for TokenKind<'a> {
    fn default() -> Self {
        Self::Eof
    }
}

impl<'a> Default for &TokenKind<'_> {
    fn default() -> &'static TokenKind<'static> {
        &TokenKind::Eof
    }
}

impl<'a> From<KeyWord> for TokenKind<'a> {
    fn from(val: KeyWord) -> TokenKind<'a> {
        TokenKind::KeyWord(val)
    }
}

// impl<'a> From<KeyWord> for &TokenKind<'_> {
//     fn from(val: KeyWord) -> &'static TokenKind<'static> {
//         let static_val: &'static TokenKind = &TokenKind::KeyWord(val);
//         static_val
// let static_str: &'static str = "This is a static string";
//     }
// }

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct Span {
    pub(crate) row: usize,
    pub(crate) col: usize,
    pub(crate) length: usize,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct Token<'a> {
    pub(crate) kind: TokenKind<'a>,
    pub(crate) span: Span,
}
