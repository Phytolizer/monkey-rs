use crate::token::TokenKind;
use rstest::fixture;
use rstest::rstest;

use super::Lexer;

struct NextTokenTests {
    lexer: Lexer<'static>,
    expectedKinds: Vec<TokenKind>,
    expectedLiterals: Vec<&'static str>,
}

#[fixture]
fn nextTokenInput() -> NextTokenTests {
    let input = "
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        ";

    NextTokenTests {
        lexer: Lexer::New(input),
        expectedLiterals: vec![
            "let", "five", "=", "5", ";", "let", "ten", "=", "10", ";", "let", "add", "=", "fn",
            "(", "x", ",", "y", ")", "{", "x", "+", "y", ";", "}", ";", "let", "result", "=",
            "add", "(", "five", ",", "ten", ")", ";", "!", "-", "/", "*", "5", ";", "5", "<", "10",
            ">", "5", ";", "if", "(", "5", "<", "10", ")", "{", "return", "true", ";", "}", "else",
            "{", "return", "false", ";", "}", "10", "==", "10", ";", "10", "!=", "9", ";",
        ],
        expectedKinds: vec![
            TokenKind::LET,
            TokenKind::IDENT,
            TokenKind::ASSIGN,
            TokenKind::INT,
            TokenKind::SEMICOLON,
            TokenKind::LET,
            TokenKind::IDENT,
            TokenKind::ASSIGN,
            TokenKind::INT,
            TokenKind::SEMICOLON,
            TokenKind::LET,
            TokenKind::IDENT,
            TokenKind::ASSIGN,
            TokenKind::FUNCTION,
            TokenKind::LPAREN,
            TokenKind::IDENT,
            TokenKind::COMMA,
            TokenKind::IDENT,
            TokenKind::RPAREN,
            TokenKind::LBRACE,
            TokenKind::IDENT,
            TokenKind::PLUS,
            TokenKind::IDENT,
            TokenKind::SEMICOLON,
            TokenKind::RBRACE,
            TokenKind::SEMICOLON,
            TokenKind::LET,
            TokenKind::IDENT,
            TokenKind::ASSIGN,
            TokenKind::IDENT,
            TokenKind::LPAREN,
            TokenKind::IDENT,
            TokenKind::COMMA,
            TokenKind::IDENT,
            TokenKind::RPAREN,
            TokenKind::SEMICOLON,
            TokenKind::BANG,
            TokenKind::MINUS,
            TokenKind::SLASH,
            TokenKind::STAR,
            TokenKind::INT,
            TokenKind::SEMICOLON,
            TokenKind::INT,
            TokenKind::LESS,
            TokenKind::INT,
            TokenKind::GREATER,
            TokenKind::INT,
            TokenKind::SEMICOLON,
            TokenKind::IF,
            TokenKind::LPAREN,
            TokenKind::INT,
            TokenKind::LESS,
            TokenKind::INT,
            TokenKind::RPAREN,
            TokenKind::LBRACE,
            TokenKind::RETURN,
            TokenKind::TRUE,
            TokenKind::SEMICOLON,
            TokenKind::RBRACE,
            TokenKind::ELSE,
            TokenKind::LBRACE,
            TokenKind::RETURN,
            TokenKind::FALSE,
            TokenKind::SEMICOLON,
            TokenKind::RBRACE,
            TokenKind::INT,
            TokenKind::EQ,
            TokenKind::INT,
            TokenKind::SEMICOLON,
            TokenKind::INT,
            TokenKind::NOT_EQ,
            TokenKind::INT,
            TokenKind::SEMICOLON,
        ],
    }
}

#[rstest]
fn next_token(mut nextTokenInput: NextTokenTests) {
    for (expectedKind, expectedLiteral) in nextTokenInput
        .expectedKinds
        .iter()
        .zip(nextTokenInput.expectedLiterals.iter())
    {
        let tok = nextTokenInput.lexer.NextToken();
        assert_eq!(&tok.kind, expectedKind);
        assert_eq!(&tok.literal, expectedLiteral);
    }

    let tok = nextTokenInput.lexer.NextToken();
    assert_eq!(tok.kind, TokenKind::EOF);
    assert_eq!(tok.literal, "");
}
