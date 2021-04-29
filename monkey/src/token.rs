use std::collections::HashMap;
use std::fmt::Display;

use once_cell::sync::Lazy;
use strum_macros::EnumIter;

#[allow(clippy::upper_case_acronyms)]
#[derive(EnumIter, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenKind {
    ILLEGAL,
    EOF,

    IDENT,
    INT,

    ASSIGN,
    PLUS,

    COMMA,
    SEMICOLON,
    BANG,
    MINUS,
    SLASH,
    STAR,

    LESS,
    GREATER,
    EQ,
    NOT_EQ,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
    IF,
    ELSE,
    RETURN,
    TRUE,
    FALSE,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub(crate) literal: String,
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::ILLEGAL,
            literal: String::default(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token {{ kind: {:?}, literal: '{}' }}",
            self.kind, self.literal
        )
    }
}

static KEYWORDS: Lazy<HashMap<&'static str, TokenKind>> = Lazy::new(|| {
    maplit::hashmap! {
        "let" => TokenKind::LET,
        "fn" => TokenKind::FUNCTION,
        "if" => TokenKind::IF,
        "else" => TokenKind::ELSE,
        "return" => TokenKind::RETURN,
        "true" => TokenKind::TRUE,
        "false" => TokenKind::FALSE,
    }
});

pub(crate) fn LookupIdent(ident: &str) -> TokenKind {
    KEYWORDS.get(&ident).cloned().unwrap_or(TokenKind::IDENT)
}
