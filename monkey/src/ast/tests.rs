use super::*;
use crate::token::TokenKind;

#[test]
fn String() {
    let program = Program {
        statements: vec![LetStatement {
            token: Token {
                kind: TokenKind::LET,
                literal: "let",
            },
            name: Identifier {
                token: Token {
                    kind: TokenKind::IDENT,
                    literal: "myVar",
                },
                value: "myVar",
            },
            value: Identifier {
                token: Token {
                    kind: TokenKind::IDENT,
                    literal: "anotherVar",
                },
                value: "anotherVar",
            }
            .into(),
        }
        .into()],
    };

    assert_eq!(program.String(), "let myVar = anotherVar;")
}
