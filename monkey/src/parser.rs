use crate::ast::Program;
use crate::lexer::Lexer;
use crate::token::Token;

pub(crate) struct Parser<'src> {
    lexer: Lexer<'src>,

    curToken: Token<'src>,
    peekToken: Token<'src>,
}

impl<'src> Parser<'src> {
    pub(crate) fn New(lexer: Lexer<'src>) -> Self {
        let mut p = Self {
            lexer,
            curToken: Token::default(),
            peekToken: Token::default(),
        };

        p.nextToken();
        p.nextToken();

        p
    }

    pub(crate) fn ParseProgram(&mut self) -> Option<Program<'src>> {
        None
    }

    fn nextToken(&mut self) {
        self.curToken = self.peekToken.clone();
        self.peekToken = self.lexer.NextToken();
    }
}

#[cfg(test)]
mod tests {
    use matches::assert_matches;

    use super::*;

    use crate::ast::ExpressionEnum;
    use crate::ast::Identifier;
    use crate::ast::LetStatement;
    use crate::ast::Node;
    use crate::ast::StatementEnum;
    use crate::token::Token;
    use crate::token::TokenKind;

    #[test]
    fn LetStatements() {
        let input = "
        let five = 5;
        let ten = 10;
        let foobar = 838383;
        ";

        let l = Lexer::New(input);
        let mut p = Parser::New(l);

        let program = p.ParseProgram();
        dbg!(program.as_ref().map(|p| p.String()));
        assert!(program.is_some());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 3);
        assert_matches!(program.statements[0], StatementEnum::LetStatement { .. });
    }
}
