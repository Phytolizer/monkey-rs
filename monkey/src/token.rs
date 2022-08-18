#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Kind {
    Illegal,
    Eof,

    Ident,
    Int,

    Assign,
    Plus,

    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
}

impl Kind {
    pub(crate) fn to_string(self) -> &'static str {
        match self {
            Kind::Illegal => "ILLEGAL",
            Kind::Eof => "EOF",
            Kind::Ident => "IDENT",
            Kind::Int => "INT",
            Kind::Assign => "=",
            Kind::Plus => "+",
            Kind::Comma => ",",
            Kind::Semicolon => ";",
            Kind::LParen => "(",
            Kind::RParen => ")",
            Kind::LBrace => "{",
            Kind::RBrace => "}",
            Kind::Function => "fn",
            Kind::Let => "let",
        }
    }
}

pub(crate) struct Token {
    pub(crate) kind: Kind,
    pub(crate) literal: String,
}

impl Token {
    pub(crate) fn new(kind: Kind, literal: &str) -> Self {
        Self {
            kind,
            literal: literal.to_string(),
        }
    }

    pub(crate) fn take(kind: Kind, literal: String) -> Self {
        Self { kind, literal }
    }

    pub(crate) fn char_lit(kind: Kind, literal: char) -> Self {
        Self {
            kind,
            literal: std::iter::once(literal).collect(),
        }
    }
}
