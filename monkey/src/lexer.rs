use std::iter::Peekable;
use std::str::CharIndices;

use crate::token::LookupIdent;
use crate::token::Token;
use crate::token::TokenKind;

pub struct Lexer<'src> {
    input: &'src str,
    input_chars: Peekable<CharIndices<'src>>,
    ch: char,
    pos: usize,
}

impl<'src> Lexer<'src> {
    pub fn New(input: &'src str) -> Self {
        let mut lexer = Self {
            input_chars: input.char_indices().peekable(),
            input,
            ch: '\0',
            pos: 0,
        };
        lexer.readChar();
        lexer
    }

    fn readChar(&mut self) {
        let (pos, ch) = self.input_chars.next().unwrap_or((self.input.len(), '\0'));
        self.pos = pos;
        self.ch = ch;
    }

    fn peekChar(&mut self) -> char {
        let (_, ch) = self
            .input_chars
            .peek()
            .cloned()
            .unwrap_or((self.input.len(), '\0'));
        ch
    }

    fn skipWhitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.readChar();
        }
    }

    pub fn NextToken(&mut self) -> Token<'src> {
        self.skipWhitespace();
        let tok = match self.ch {
            ';' => self.singleCharToken(TokenKind::SEMICOLON),
            '(' => self.singleCharToken(TokenKind::LPAREN),
            ')' => self.singleCharToken(TokenKind::RPAREN),
            '{' => self.singleCharToken(TokenKind::LBRACE),
            '}' => self.singleCharToken(TokenKind::RBRACE),
            ',' => self.singleCharToken(TokenKind::COMMA),
            '+' => self.singleCharToken(TokenKind::PLUS),
            '-' => self.singleCharToken(TokenKind::MINUS),
            '*' => self.singleCharToken(TokenKind::STAR),
            '/' => self.singleCharToken(TokenKind::SLASH),
            '<' => self.singleCharToken(TokenKind::LESS),
            '>' => self.singleCharToken(TokenKind::GREATER),
            '!' => {
                if self.peekChar() == '=' {
                    self.readChar();
                    self.twoCharToken(TokenKind::NOT_EQ)
                } else {
                    self.singleCharToken(TokenKind::BANG)
                }
            }
            '=' => {
                if self.peekChar() == '=' {
                    self.readChar();
                    self.twoCharToken(TokenKind::EQ)
                } else {
                    self.singleCharToken(TokenKind::ASSIGN)
                }
            }
            '\0' => Token {
                kind: TokenKind::EOF,
                literal: "",
            },
            ch if ch.is_alphabetic() => {
                let literal = self.readIdentifier();
                let kind = LookupIdent(literal);
                return Token { kind, literal };
            }
            ch if ch.is_digit(10) => {
                let literal = self.readNumber();
                let kind = TokenKind::INT;
                return Token { kind, literal };
            }
            _ => self.singleCharToken(TokenKind::ILLEGAL),
        };

        self.readChar();
        tok
    }

    fn singleCharToken(&self, kind: TokenKind) -> Token<'src> {
        if self.pos < self.input.len() {
            Token {
                kind,
                literal: &self.input[self.pos..self.pos + 1],
            }
        } else {
            unreachable!()
        }
    }

    fn twoCharToken(&self, kind: TokenKind) -> Token<'src> {
        if self.pos < self.input.len() {
            Token {
                kind,
                literal: &self.input[self.pos - 1..self.pos + 1],
            }
        } else {
            unreachable!()
        }
    }

    fn readIdentifier(&mut self) -> &'src str {
        let pos = self.pos;
        while self.ch.is_alphanumeric() {
            self.readChar();
        }
        &self.input[pos..self.pos]
    }

    fn readNumber(&mut self) -> &'src str {
        let pos = self.pos;
        while self.ch.is_digit(10) {
            self.readChar();
        }
        &self.input[pos..self.pos]
    }
}

#[cfg(test)]
mod tests;
