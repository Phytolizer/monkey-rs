use crate::ast::BlockStatement;
use crate::ast::Boolean;
use crate::ast::CallExpression;
use crate::ast::ExpressionEnum;
use crate::ast::ExpressionStatement;
use crate::ast::FunctionLiteral;
use crate::ast::Identifier;
use crate::ast::IfExpression;
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
    PARSE_BOOLEAN,
    PARSE_GROUPED_EXPRESSION,
    PARSE_IF_EXPRESSION,
    PARSE_FUNCTION_LITERAL,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(clippy::upper_case_acronyms)]
enum InfixDispatcher {
    PARSE_INFIX_EXPRESSION,
    PARSE_CALL_EXPRESSION,
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
            Self::TRUE | Self::FALSE => Some(PrefixDispatcher::PARSE_BOOLEAN),
            Self::LPAREN => Some(PrefixDispatcher::PARSE_GROUPED_EXPRESSION),
            Self::IF => Some(PrefixDispatcher::PARSE_IF_EXPRESSION),
            Self::FUNCTION => Some(PrefixDispatcher::PARSE_FUNCTION_LITERAL),
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
            Self::LPAREN => Some(InfixDispatcher::PARSE_CALL_EXPRESSION),
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
            Self::LPAREN => Precedence::CALL,
            _ => Precedence::LOWEST,
        }
    }
}

pub struct Parser<'src> {
    lexer: Lexer<'src>,

    curToken: Token<'src>,
    peekToken: Token<'src>,

    pub errors: Vec<String>,
}

impl<'src> Parser<'src> {
    pub fn New(lexer: Lexer<'src>) -> Self {
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

    pub fn ParseProgram(&mut self) -> Program<'src> {
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
            PrefixDispatcher::PARSE_BOOLEAN => Some(self.parseBoolean().into()),
            PrefixDispatcher::PARSE_GROUPED_EXPRESSION => self.parseGroupedExpression(),
            PrefixDispatcher::PARSE_IF_EXPRESSION => self.parseIfExpression().map(Into::into),
            PrefixDispatcher::PARSE_FUNCTION_LITERAL => self.parseFunctionLiteral().map(Into::into),
        }
    }

    fn parseFunctionLiteral(&mut self) -> Option<FunctionLiteral<'src>> {
        let token = self.curToken.clone();
        if !self.expectPeek(TokenKind::LPAREN) {
            return None;
        }

        let parameters = self.parseFunctionParameters()?;
        if !self.expectPeek(TokenKind::LBRACE) {
            return None;
        }

        let body = self.parseBlockStatement();
        Some(FunctionLiteral {
            token,
            parameters,
            body: Box::new(body),
        })
    }

    fn parseFunctionParameters(&mut self) -> Option<Vec<Identifier<'src>>> {
        let mut identifiers = vec![];
        if self.peekTokenIs(TokenKind::RPAREN) {
            self.nextToken();
            return Some(identifiers);
        }

        self.nextToken();
        identifiers.push(Identifier {
            token: self.curToken.clone(),
            value: self.curToken.literal,
        });
        while self.peekTokenIs(TokenKind::COMMA) {
            self.nextToken();
            self.nextToken();
            identifiers.push(Identifier {
                token: self.curToken.clone(),
                value: self.curToken.literal,
            });
        }

        if !self.expectPeek(TokenKind::RPAREN) {
            return None;
        }

        Some(identifiers)
    }

    fn parseIfExpression(&mut self) -> Option<IfExpression<'src>> {
        let token = self.curToken.clone();
        if !self.expectPeek(TokenKind::LPAREN) {
            return None;
        }
        self.nextToken();
        let condition = self.parseExpression(Precedence::LOWEST)?;
        if !self.expectPeek(TokenKind::RPAREN) {
            return None;
        }
        if !self.expectPeek(TokenKind::LBRACE) {
            return None;
        }
        let consequence = self.parseBlockStatement();

        let mut alternative = None;
        if self.peekTokenIs(TokenKind::ELSE) {
            self.nextToken();
            if !self.expectPeek(TokenKind::LBRACE) {
                return None;
            }
            alternative = Some(self.parseBlockStatement());
        }

        Some(IfExpression {
            token,
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: alternative.map(Box::new),
        })
    }

    fn parseBlockStatement(&mut self) -> BlockStatement<'src> {
        let token = self.curToken.clone();
        let mut statements = vec![];
        self.nextToken();
        while !self.curTokenIs(TokenKind::RBRACE) && !self.curTokenIs(TokenKind::EOF) {
            let stmt = self.parseStatement();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }
            self.nextToken();
        }
        BlockStatement { token, statements }
    }

    fn parseGroupedExpression(&mut self) -> Option<ExpressionEnum<'src>> {
        self.nextToken();
        let exp = self.parseExpression(Precedence::LOWEST);
        if !self.expectPeek(TokenKind::RPAREN) {
            return None;
        }
        exp
    }

    fn parseBoolean(&mut self) -> Boolean<'src> {
        Boolean {
            token: self.curToken.clone(),
            value: self.curTokenIs(TokenKind::TRUE),
        }
    }

    fn dispatchInfix(
        &mut self,
        left: ExpressionEnum<'src>,
        dispatcher: InfixDispatcher,
    ) -> Option<ExpressionEnum<'src>> {
        match dispatcher {
            InfixDispatcher::PARSE_INFIX_EXPRESSION => self.parseInfixExpression(left),
            InfixDispatcher::PARSE_CALL_EXPRESSION => self.parseCallExpression(left),
        }
    }

    fn parseCallExpression(
        &mut self,
        function: ExpressionEnum<'src>,
    ) -> Option<ExpressionEnum<'src>> {
        let token = self.curToken.clone();
        let arguments = self.parseCallArguments()?;
        Some(
            CallExpression {
                token,
                function: Box::new(function),
                arguments,
            }
            .into(),
        )
    }

    fn parseCallArguments(&mut self) -> Option<Vec<ExpressionEnum<'src>>> {
        let mut args = vec![];

        if self.peekTokenIs(TokenKind::RPAREN) {
            self.nextToken();
            return Some(args);
        }

        self.nextToken();
        args.push(self.parseExpression(Precedence::LOWEST)?);

        while self.peekTokenIs(TokenKind::COMMA) {
            self.nextToken();
            self.nextToken();
            args.push(self.parseExpression(Precedence::LOWEST)?);
        }

        if !self.expectPeek(TokenKind::RPAREN) {
            return None;
        }

        Some(args)
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

    use crate::ast::Boolean;
    use crate::ast::CallExpression;
    use crate::ast::ExpressionStatement;
    use crate::ast::FunctionLiteral;
    use crate::ast::IfExpression;
    use crate::ast::InfixExpression;
    use crate::ast::IntegerLiteral;
    use crate::ast::LetStatement;
    use crate::ast::Node;
    use crate::ast::PrefixExpression;
    use crate::ast::ReturnStatement;
    use crate::ast::StatementEnum;
    use std::convert::TryInto;

    enum MonkeyLiteral {
        Int(i64),
        Bool(bool),
        Ident(&'static str),
    }

    impl From<bool> for MonkeyLiteral {
        fn from(val: bool) -> Self {
            Self::Bool(val)
        }
    }

    impl From<i64> for MonkeyLiteral {
        fn from(val: i64) -> Self {
            Self::Int(val)
        }
    }

    impl From<&'static str> for MonkeyLiteral {
        fn from(val: &'static str) -> Self {
            Self::Ident(val)
        }
    }

    fn testLetStatement(stmt: StatementEnum, expectedName: &str) {
        assert_eq!(stmt.TokenLiteral(), "let");
        let let_stmt: LetStatement = stmt.try_into().unwrap();
        assert_eq!(let_stmt.name.value, expectedName);
        assert_eq!(let_stmt.name.TokenLiteral(), expectedName);
    }

    fn testBooleanLiteral(exp: ExpressionEnum, expected: bool) {
        let b: Boolean = exp.try_into().unwrap();
        assert_eq!(b.value, expected);
        assert_eq!(b.TokenLiteral(), expected.to_string());
    }

    fn testIdentifier(exp: ExpressionEnum, ident: &'static str) {
        let i: Identifier = exp.try_into().unwrap();
        assert_eq!(i.TokenLiteral(), ident);
    }

    fn testLiteralExpression(exp: ExpressionEnum, expected: MonkeyLiteral) {
        match expected {
            MonkeyLiteral::Int(i) => testIntegerLiteral(exp, i),
            MonkeyLiteral::Bool(b) => testBooleanLiteral(exp, b),
            MonkeyLiteral::Ident(i) => testIdentifier(exp, i),
        }
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
        let prefix_tests: Vec<(&str, &str, MonkeyLiteral)> = vec![
            ("!5;", "!", 5i64.into()),
            ("-15;", "-", 15.into()),
            ("!true;", "!", true.into()),
            ("!false;", "!", false.into()),
        ];

        for (input, operator, value) in prefix_tests.into_iter() {
            let l = Lexer::New(input);
            let mut p = Parser::New(l);
            let program = p.ParseProgram();
            checkParserErrors(&p);
            assert_eq!(program.statements.len(), 1);
            let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
            let exp: PrefixExpression = stmt.expression.unwrap().try_into().unwrap();
            assert_eq!(exp.operator, operator);
            testLiteralExpression(*exp.right, value);
        }
    }

    #[test]
    fn InfixExpressions() {
        let infix_tests: Vec<(&str, MonkeyLiteral, &str, MonkeyLiteral)> = vec![
            ("5 + 5;", 5.into(), "+", 5.into()),
            ("5 - 5;", 5.into(), "-", 5.into()),
            ("5 * 5;", 5.into(), "*", 5.into()),
            ("5 / 5;", 5.into(), "/", 5.into()),
            ("5 < 5;", 5.into(), "<", 5.into()),
            ("5 > 5;", 5.into(), ">", 5.into()),
            ("5 == 5;", 5.into(), "==", 5.into()),
            ("5 != 5;", 5.into(), "!=", 5.into()),
            ("true == true", true.into(), "==", true.into()),
            ("true != false", true.into(), "!=", false.into()),
            ("false == false", false.into(), "==", false.into()),
        ];

        for (input, leftValue, operator, rightValue) in infix_tests {
            let l = Lexer::New(input);
            let mut p = Parser::New(l);
            let program = p.ParseProgram();
            checkParserErrors(&p);
            assert_eq!(program.statements.len(), 1);
            let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
            let exp: InfixExpression = stmt.expression.unwrap().try_into().unwrap();
            testLiteralExpression(*exp.left, leftValue);
            assert_eq!(exp.operator, operator);
            testLiteralExpression(*exp.right, rightValue);
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
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
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

    #[test]
    fn BooleanExpression() {
        let input = "true;";
        let l = Lexer::New(input);
        let mut p = Parser::New(l);
        let program = p.ParseProgram();
        checkParserErrors(&p);
        assert_eq!(program.statements.len(), 1);
        let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
        let b: Boolean = stmt.expression.unwrap().try_into().unwrap();
        assert_eq!(b.value, true);
        assert_eq!(b.TokenLiteral(), "true");
    }

    #[test]
    fn ParseIfExpression() {
        let input = "if (x < y) { x }";
        let l = Lexer::New(input);
        let mut p = Parser::New(l);
        let program = p.ParseProgram();
        checkParserErrors(&p);
        assert_eq!(program.statements.len(), 1);
        let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
        let exp: IfExpression = stmt.expression.unwrap().try_into().unwrap();
        testInfixExpression(*exp.condition, "x".into(), "<", "y".into());
        assert_eq!(exp.consequence.statements.len(), 1);
        let consequence: ExpressionStatement =
            exp.consequence.statements[0].clone().try_into().unwrap();
        testIdentifier(consequence.expression.unwrap(), "x");
        assert!(exp.alternative.is_none());
    }

    fn testInfixExpression(
        exp: ExpressionEnum,
        left: MonkeyLiteral,
        operator: &str,
        right: MonkeyLiteral,
    ) {
        let infix: InfixExpression = exp.try_into().unwrap();
        testLiteralExpression(*infix.left, left);
        assert_eq!(infix.operator, operator);
        testLiteralExpression(*infix.right, right);
    }

    #[test]
    fn IfElseExpression() {
        let input = "if(x < y) { x } else { y }";
        let l = Lexer::New(input);
        let mut p = Parser::New(l);
        let program = p.ParseProgram();
        checkParserErrors(&p);
        assert_eq!(program.statements.len(), 1);
        let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
        let exp: IfExpression = stmt.expression.unwrap().try_into().unwrap();
        testInfixExpression(*exp.condition, "x".into(), "<", "y".into());
        assert_eq!(exp.consequence.statements.len(), 1);
        let consequence: ExpressionStatement =
            exp.consequence.statements[0].clone().try_into().unwrap();
        assert_matches!(
            consequence.expression,
            Some(ExpressionEnum::Identifier(Identifier { value: "x", .. }))
        );
        assert_eq!(exp.alternative.as_ref().unwrap().statements.len(), 1);
        let alternative: ExpressionStatement = exp.alternative.unwrap().statements[0]
            .clone()
            .try_into()
            .unwrap();
        assert_matches!(
            alternative.expression,
            Some(ExpressionEnum::Identifier(Identifier { value: "y", .. }))
        )
    }

    #[test]
    fn ParseFunctionLiteral() {
        let input = "fn(x, y) { x + y; }";
        let l = Lexer::New(input);
        let mut p = Parser::New(l);
        let program = p.ParseProgram();
        checkParserErrors(&p);
        assert_eq!(program.statements.len(), 1);
        let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
        let function: FunctionLiteral = stmt.expression.unwrap().try_into().unwrap();
        assert_eq!(function.parameters.len(), 2);
        testLiteralExpression(function.parameters[0].clone().into(), "x".into());
        testLiteralExpression(function.parameters[1].clone().into(), "y".into());
        assert_eq!(function.body.statements.len(), 1);
        let body: ExpressionStatement = function.body.statements[0].clone().try_into().unwrap();
        testInfixExpression(body.expression.unwrap(), "x".into(), "+", "y".into());
    }

    #[test]
    fn FunctionParameters() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];
        for (input, expectedParams) in tests {
            let l = Lexer::New(input);
            let mut p = Parser::New(l);
            let program = p.ParseProgram();
            checkParserErrors(&p);
            assert_eq!(program.statements.len(), 1);
            let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
            let function: FunctionLiteral = stmt.expression.unwrap().try_into().unwrap();
            assert_eq!(function.parameters.len(), expectedParams.len());
            for (actual, expected) in function
                .parameters
                .into_iter()
                .zip(expectedParams.into_iter())
            {
                testLiteralExpression(actual.into(), expected.into());
            }
        }
    }

    #[test]
    fn ParseCallExpression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let l = Lexer::New(input);
        let mut p = Parser::New(l);
        let program = p.ParseProgram();
        checkParserErrors(&p);
        assert_eq!(program.statements.len(), 1);
        let stmt: ExpressionStatement = program.statements[0].clone().try_into().unwrap();
        let exp: CallExpression = stmt.expression.unwrap().try_into().unwrap();
        testIdentifier(*exp.function, "add");
        assert_eq!(exp.arguments.len(), 3);
        testLiteralExpression(exp.arguments[0].clone(), 1.into());
        testInfixExpression(exp.arguments[1].clone(), 2.into(), "*", 3.into());
        testInfixExpression(exp.arguments[2].clone(), 4.into(), "+", 5.into());
    }
}
