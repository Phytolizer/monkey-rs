use crate::ast::ExpressionEnum;
use crate::ast::ExpressionStatement;
use crate::ast::Identifier;
use crate::ast::InfixExpression;
use crate::ast::IntegerLiteral;
use crate::ast::LetStatement;
use crate::ast::PrefixExpression;
use crate::ast::Program;
use crate::ast::ReturnStatement;
use crate::ast::StatementEnum;
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::upper_case_acronyms)]
enum Precedence {
    _ZERO,
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::upper_case_acronyms)]
enum PrefixDispatcher {
    PARSE_IDENTIFIER,
    PARSE_INTEGER_LITERAL,
    PARSE_PREFIX_EXPRESSION,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::upper_case_acronyms)]
enum InfixDispatcher {
    PARSE_INFIX_EXPRESSION,
}

trait TokenTypeExt {
    fn prefix_dispatcher(self) -> Option<PrefixDispatcher>;
    fn infix_dispatcher(self) -> Option<InfixDispatcher>;
    fn binary_precedence(self) -> Precedence;
}

impl TokenTypeExt for TokenKind {
    fn prefix_dispatcher(self) -> Option<PrefixDispatcher> {
        match self {
            Self::IDENT => Some(PrefixDispatcher::PARSE_IDENTIFIER),
            Self::INT => Some(PrefixDispatcher::PARSE_INTEGER_LITERAL),
            Self::BANG | Self::MINUS => Some(PrefixDispatcher::PARSE_PREFIX_EXPRESSION),
            _ => None,
        }
    }

    fn infix_dispatcher(self) -> Option<InfixDispatcher> {
        match self {
            Self::PLUS
            | Self::MINUS
            | Self::STAR
            | Self::SLASH
            | Self::LESS
            | Self::GREATER
            | Self::EQ
            | Self::NOT_EQ => Some(InfixDispatcher::PARSE_INFIX_EXPRESSION),
            _ => None,
        }
    }

    fn binary_precedence(self) -> Precedence {
        match self {
            Self::EQ => Precedence::EQUALS,
            Self::NOT_EQ => Precedence::EQUALS,
            Self::LESS => Precedence::LESSGREATER,
            Self::GREATER => Precedence::LESSGREATER,
            Self::PLUS => Precedence::SUM,
            Self::MINUS => Precedence::SUM,
            Self::STAR => Precedence::PRODUCT,
            Self::SLASH => Precedence::PRODUCT,
            _ => Precedence::LOWEST,
        }
    }
}

pub(crate) struct Parser<'src> {
    lexer: Lexer<'src>,

    curToken: Token<'src>,
    peekToken: Token<'src>,

    pub(crate) errors: Vec<String>,
}

impl<'src> Parser<'src> {
    pub(crate) fn New(lexer: Lexer<'src>) -> Self {
        let mut p = Self {
            lexer,
            curToken: Token::default(),
            peekToken: Token::default(),
            errors: Vec::default(),
        };

        p.nextToken();
        p.nextToken();

        p
    }

    pub(crate) fn ParseProgram(&mut self) -> Program<'src> {
        let mut statements = vec![];

        while self.curToken.kind != TokenKind::EOF {
            let stmt = self.parseStatement();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }
            self.nextToken();
        }

        Program { statements }
    }

    fn peekPrecedence(&self) -> Precedence {
        self.peekToken.kind.binary_precedence()
    }

    fn curPrecedence(&self) -> Precedence {
        self.curToken.kind.binary_precedence()
    }

    fn parseStatement(&mut self) -> Option<StatementEnum<'src>> {
        match self.curToken.kind {
            TokenKind::LET => self.parseLetStatement(),
            TokenKind::RETURN => self.parseReturnStatement().map(Into::into),
            _ => Some(self.parseExpressionStatement().into()),
        }
    }

    fn parseLetStatement(&mut self) -> Option<StatementEnum<'src>> {
        let token = self.curToken.clone();

        if !self.expectPeek(TokenKind::IDENT) {
            return None;
        }

        let name = Identifier {
            token: self.curToken.clone(),
            value: self.curToken.literal,
        };

        if !self.expectPeek(TokenKind::ASSIGN) {
            return None;
        }

        self.nextToken();

        let value = self.parseExpression(Precedence::LOWEST)?;

        if self.peekTokenIs(TokenKind::SEMICOLON) {
            self.nextToken();
        }

        Some(LetStatement { token, name, value }.into())
    }

    fn parseReturnStatement(&mut self) -> Option<ReturnStatement<'src>> {
        let token = self.curToken.clone();
        self.nextToken();

        let returnValue = self.parseExpression(Precedence::LOWEST)?;

        if self.peekTokenIs(TokenKind::SEMICOLON) {
            self.nextToken();
        }

        Some(ReturnStatement { token, returnValue })
    }

    fn parseExpressionStatement(&mut self) -> ExpressionStatement<'src> {
        let token = self.curToken.clone();

        let expression = self.parseExpression(Precedence::LOWEST);

        if self.peekTokenIs(TokenKind::SEMICOLON) {
            self.nextToken();
        }

        ExpressionStatement { token, expression }
    }

    fn dispatchPrefix(&mut self, dispatcher: PrefixDispatcher) -> Option<ExpressionEnum<'src>> {
        match dispatcher {
            PrefixDispatcher::PARSE_IDENTIFIER => Some(self.parseIdentifier().into()),
            PrefixDispatcher::PARSE_INTEGER_LITERAL => self.parseIntegerLiteral().map(Into::into),
            PrefixDispatcher::PARSE_PREFIX_EXPRESSION => {
                self.parsePrefixExpression().map(Into::into)
            }
        }
    }

    fn dispatchInfix(
        &mut self,
        left: ExpressionEnum<'src>,
        dispatcher: InfixDispatcher,
    ) -> Option<ExpressionEnum<'src>> {
        match dispatcher {
            InfixDispatcher::PARSE_INFIX_EXPRESSION => self.parseInfixExpression(left),
        }
    }

    fn parsePrefixExpression(&mut self) -> Option<PrefixExpression<'src>> {
        let token = self.curToken.clone();
        let operator = self.curToken.literal;
        self.nextToken();
        let right = self.parseExpression(Precedence::PREFIX)?;
        Some(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parseIdentifier(&mut self) -> Identifier<'src> {
        Identifier {
            token: self.curToken.clone(),
            value: self.curToken.literal,
        }
    }

    fn parseIntegerLiteral(&mut self) -> Option<IntegerLiteral<'src>> {
        let token = self.curToken.clone();

        let value: i64 = self.curToken.literal.parse().ok().or_else(|| {
            self.errors.push(format!(
                r#"could not parse "{}" as integer"#,
                self.curToken.literal
            ));
            None
        })?;

        Some(IntegerLiteral { token, value })
    }

    fn parseExpression(&mut self, precedence: Precedence) -> Option<ExpressionEnum<'src>> {
        let prefix_dispatcher = self.curToken.kind.prefix_dispatcher().or_else(|| {
            self.noPrefixParseFnError(self.curToken.kind);
            None
        })?;
        let mut left_exp = self.dispatchPrefix(prefix_dispatcher)?;
        while !self.peekTokenIs(TokenKind::SEMICOLON) && precedence < self.peekPrecedence() {
            let infix_dispatcher = match self.peekToken.kind.infix_dispatcher() {
                Some(dispatcher) => dispatcher,
                None => return Some(left_exp),
            };
            self.nextToken();
            left_exp = self.dispatchInfix(left_exp, infix_dispatcher)?;
        }
        Some(left_exp)
    }

    fn curTokenIs(&self, kind: TokenKind) -> bool {
        self.curToken.kind == kind
    }

    fn peekTokenIs(&self, kind: TokenKind) -> bool {
        self.peekToken.kind == kind
    }

    fn expectPeek(&mut self, kind: TokenKind) -> bool {
        if self.peekTokenIs(kind) {
            self.nextToken();
            true
        } else {
            self.peekError(kind);
            false
        }
    }

    fn peekError(&mut self, kind: TokenKind) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            kind, self.peekToken.kind
        );
        self.errors.push(msg);
    }

    fn noPrefixParseFnError(&mut self, kind: TokenKind) {
        self.errors
            .push(format!("no prefix parse function for {:?} found", kind));
    }

    fn nextToken(&mut self) {
        self.curToken = self.peekToken.clone();
        self.peekToken = self.lexer.NextToken();
    }

    fn parseInfixExpression(&mut self, left: ExpressionEnum<'src>) -> Option<ExpressionEnum<'src>> {
        let token = self.curToken.clone();
        let operator = self.curToken.literal;
        let precedence = self.curPrecedence();
        self.nextToken();
        let right = self.parseExpression(precedence)?;
        Some(
            InfixExpression {
                token,
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
            .into(),
        )
    }
}

#[cfg(test)]
mod tests {
    use matches::assert_matches;

    use super::*;

    use crate::ast::ExpressionStatement;
    use crate::ast::InfixExpression;
    use crate::ast::IntegerLiteral;
    use crate::ast::LetStatement;
    use crate::ast::Node;
    use crate::ast::PrefixExpression;
    use crate::ast::ReturnStatement;
    use crate::ast::StatementEnum;
    use std::convert::TryInto;

    fn testLetStatement(stmt: StatementEnum, expectedName: &str) {
        assert_eq!(stmt.TokenLiteral(), "let");
        let let_stmt: LetStatement = stmt.try_into().unwrap();
        assert_eq!(let_stmt.name.value, expectedName);
        assert_eq!(let_stmt.name.TokenLiteral(), expectedName);
    }

    fn checkParserErrors(parser: &Parser) {
        for error in parser.errors.iter() {
            eprintln!("parser error encountered: {}", error);
        }

        assert_eq!(parser.errors.len(), 0);
    }

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
        checkParserErrors(&p);
        assert_eq!(program.statements.len(), 3);
        assert_matches!(program.statements[0], StatementEnum::Let { .. });

        let expectedNames = vec!["five", "ten", "foobar"];

        for (stmt, expectedName) in program
            .statements
            .into_iter()
            .zip(expectedNames.into_iter())
        {
            testLetStatement(stmt, expectedName)
        }
    }

    #[test]
    fn ReturnStatements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";

        let l = Lexer::New(input);
        let mut p = Parser::New(l);
        let program = p.ParseProgram();
        checkParserErrors(&p);
        assert_eq!(program.statements.len(), 3);
        for stmt in program.statements.into_iter() {
            let return_stmt: ReturnStatement = stmt.try_into().unwrap();
            assert_eq!(return_stmt.TokenLiteral(), "return");
        }
    }

    #[test]
    fn IdentifierExpression() {
        let input = "foobar;";
        let l = Lexer::New(input);
        let mut p = Parser::New(l);
        let program = p.ParseProgram();
        checkParserErrors(&p);
        assert_eq!(program.statements.len(), 1);
        let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
        let ident: Identifier = stmt.expression.unwrap().try_into().unwrap();
        assert_eq!(ident.value, "foobar");
        assert_eq!(ident.TokenLiteral(), "foobar");
    }

    #[test]
    fn IntegerLiteralExpression() {
        let input = "5;";
        let l = Lexer::New(input);
        let mut p = Parser::New(l);
        let program = p.ParseProgram();
        checkParserErrors(&p);
        assert_eq!(program.statements.len(), 1);
        let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
        let literal: IntegerLiteral = stmt.expression.unwrap().try_into().unwrap();
        assert_eq!(literal.value, 5);
        assert_eq!(literal.TokenLiteral(), "5");
    }

    fn testIntegerLiteral(exp: ExpressionEnum, value: i64) {
        let il: IntegerLiteral = exp.try_into().unwrap();
        assert_eq!(il.value, value);
        assert_eq!(il.TokenLiteral(), format!("{}", value));
    }

    #[test]
    fn PrefixExpressions() {
        let prefix_tests = vec![("!5;", "!", 5i64), ("-15;", "-", 15)];

        for (input, operator, integerValue) in prefix_tests.into_iter() {
            let l = Lexer::New(input);
            let mut p = Parser::New(l);
            let program = p.ParseProgram();
            checkParserErrors(&p);
            assert_eq!(program.statements.len(), 1);
            let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
            let exp: PrefixExpression = stmt.expression.unwrap().try_into().unwrap();
            assert_eq!(exp.operator, operator);
            testIntegerLiteral(*exp.right, integerValue);
        }
    }

    #[test]
    fn InfixExpressions() {
        let infix_tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, leftValue, operator, rightValue) in infix_tests {
            let l = Lexer::New(input);
            let mut p = Parser::New(l);
            let program = p.ParseProgram();
            checkParserErrors(&p);
            assert_eq!(program.statements.len(), 1);
            let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
            let exp: InfixExpression = stmt.expression.unwrap().try_into().unwrap();
            testIntegerLiteral(*exp.left, leftValue);
            assert_eq!(exp.operator, operator);
            testIntegerLiteral(*exp.right, rightValue);
        }
    }

    #[test]
    fn OperatorPrecedence() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, expected) in tests {
            let l = Lexer::New(input);
            let mut p = Parser::New(l);
            let program = p.ParseProgram();
            checkParserErrors(&p);
            let actual = program.String();
            assert_eq!(actual, expected);
        }
    }
}
