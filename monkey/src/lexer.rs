use std::iter::Peekable;
use std::str::Chars;

use crate::token;
use crate::token::Token;

pub(crate) struct Lexer<'i> {
    input: Peekable<Chars<'i>>,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

fn is_letter(ch: char) -> bool {
    ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch) || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ('0'..='9').contains(&ch)
}

const KEYWORDS: phf::Map<&'static str, token::Kind> = phf::phf_map! {
    "fn" => token::Kind::Function,
    "let" => token::Kind::Let,
};

impl<'i> Lexer<'i> {
    pub(crate) fn new(input: &'i str) -> Self {
        let mut l = Lexer {
            input: input.chars().peekable(),
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        l
    }

    fn peek_char(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    fn read_char(&mut self) {
        self.ch = self.peek_char();
        self.input.next();
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                Some(ch) if ch.is_whitespace() => self.read_char(),
                _ => break,
            }
        }
    }

    pub(crate) fn next_token(&mut self) -> Token {
        use token::Kind as tk;
        self.skip_whitespace();

        let tok = match self.ch {
            Some('=') => Token::new(tk::Assign, "="),
            Some(';') => Token::new(tk::Semicolon, ";"),
            Some('(') => Token::new(tk::LParen, "("),
            Some(')') => Token::new(tk::RParen, ")"),
            Some('{') => Token::new(tk::LBrace, "{"),
            Some('}') => Token::new(tk::RBrace, "}"),
            Some('+') => Token::new(tk::Plus, "+"),
            Some(',') => Token::new(tk::Comma, ","),
            None => Token::new(tk::Eof, ""),
            Some(c) if is_letter(c) => {
                let literal = self.read_identifier();
                let kind = KEYWORDS.get(&literal).copied().unwrap_or(tk::Ident);
                return Token::take(kind, literal);
            }
            Some(c) if is_digit(c) => {
                let literal = self.read_number();
                return Token::take(tk::Int, literal);
            }
            Some(c) => Token::char_lit(token::Kind::Illegal, c),
        };

        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let mut result = String::new();
        loop {
            match self.ch {
                Some(c) if is_letter(c) => {
                    result.push(c);
                    self.read_char();
                }
                _ => break,
            }
        }
        result
    }

    fn read_number(&mut self) -> String {
        let mut result = String::new();
        loop {
            match self.ch {
                Some(c) if is_digit(c) => {
                    result.push(c);
                    self.read_char();
                }
                _ => break,
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token;

    #[test]
    fn next_token() {
        const INPUT: &str = include_str!("lexer/next_token_input.txt");

        struct Test {
            expected_kind: token::Kind,
            expected_literal: &'static str,
        }
        impl Test {
            const fn new(expected_kind: token::Kind, expected_literal: &'static str) -> Self {
                Self {
                    expected_kind,
                    expected_literal,
                }
            }
        }
        use token::Kind as tk;
        const TESTS: &[Test] = &[
            Test::new(tk::Let, "let"),
            Test::new(tk::Ident, "five"),
            Test::new(tk::Assign, "="),
            Test::new(tk::Int, "5"),
            Test::new(tk::Semicolon, ";"),
            Test::new(tk::Let, "let"),
            Test::new(tk::Ident, "ten"),
            Test::new(tk::Assign, "="),
            Test::new(tk::Int, "10"),
            Test::new(tk::Semicolon, ";"),
            Test::new(tk::Let, "let"),
            Test::new(tk::Ident, "add"),
            Test::new(tk::Assign, "="),
            Test::new(tk::Function, "fn"),
            Test::new(tk::LParen, "("),
            Test::new(tk::Ident, "x"),
            Test::new(tk::Comma, ","),
            Test::new(tk::Ident, "y"),
            Test::new(tk::RParen, ")"),
            Test::new(tk::LBrace, "{"),
            Test::new(tk::Ident, "x"),
            Test::new(tk::Plus, "+"),
            Test::new(tk::Ident, "y"),
            Test::new(tk::Semicolon, ";"),
            Test::new(tk::RBrace, "}"),
            Test::new(tk::Semicolon, ";"),
            Test::new(tk::Let, "let"),
            Test::new(tk::Ident, "result"),
            Test::new(tk::Assign, "="),
            Test::new(tk::Ident, "add"),
            Test::new(tk::LParen, "("),
            Test::new(tk::Ident, "five"),
            Test::new(tk::Comma, ","),
            Test::new(tk::Ident, "ten"),
            Test::new(tk::RParen, ")"),
            Test::new(tk::Semicolon, ";"),
            Test::new(tk::Eof, ""),
        ];

        let mut l = Lexer::new(INPUT);
        for tt in TESTS {
            let tok = l.next_token();
            assert_eq!(tok.kind, tt.expected_kind);
            assert_eq!(tok.literal, tt.expected_literal);
        }
    }
}
