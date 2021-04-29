use super::*;
use crate::token::TokenKind;

#[test]
fn String() {
    let program = Program {
        statements: vec![LetStatement {
            token: Token {
                kind: TokenKind::LET,
                literal: "let".into(),
            },
            name: Identifier {
                token: Token {
                    kind: TokenKind::IDENT,
                    literal: "myVar".into(),
                },
                value: "myVar".into(),
            },
            value: Identifier {
                token: Token {
                    kind: TokenKind::IDENT,
                    literal: "anotherVar".into(),
                },
                value: "anotherVar".into(),
            }
            .into(),
        }
        .into()],
    };

    assert_eq!(program.String(), "let myVar = anotherVar;")
}
