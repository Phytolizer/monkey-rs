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

#[cfg(test)]
const TRACE: bool = true;
#[cfg(not(test))]
const TRACE: bool = false;

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

    curToken: Token,
    peekToken: Token,

    pub errors: Vec<String>,
    indentLevel: usize,
}

impl<'src> Parser<'src> {
    pub fn New(lexer: Lexer<'src>) -> Self {
        let mut p = Self {
            lexer,
            curToken: Token::default(),
            peekToken: Token::default(),
            errors: Vec::default(),
            indentLevel: 0,
        };

        p.nextToken();
        p.nextToken();

        p
    }

    pub fn ParseProgram(&mut self) -> Program {
        self.trace_begin("program");
        let mut statements = vec![];

        while self.curToken.kind != TokenKind::EOF {
            let stmt = self.parseStatement();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }
            self.nextToken();
        }

        self.trace_end("program");
        Program { statements }
    }

    fn indent(&self) -> String {
        itertools::join(std::iter::once("  ").cycle().take(self.indentLevel), "")
    }

    fn trace_begin(&mut self, message: &str) {
        if TRACE {
            eprintln!("{}BEGIN {}", self.indent(), message);
            self.indentLevel += 1;
        }
    }

    fn trace_end(&mut self, message: &str) {
        if TRACE {
            self.indentLevel -= 1;
            eprintln!("{}END   {}", self.indent(), message);
        }
    }

    fn peekPrecedence(&self) -> Precedence {
        self.peekToken.kind.binary_precedence()
    }

    fn curPrecedence(&self) -> Precedence {
        self.curToken.kind.binary_precedence()
    }

    fn parseStatement(&mut self) -> Option<StatementEnum> {
        self.trace_begin("statement");
        let out = match self.curToken.kind {
            TokenKind::LET => self.parseLetStatement(),
            TokenKind::RETURN => self.parseReturnStatement().map(Into::into),
            _ => self.parseExpressionStatement().map(Into::into),
        };

        self.trace_end("statement");
        out
    }

    fn parseLetStatement(&mut self) -> Option<StatementEnum> {
        self.trace_begin("let statement");
        let token = self.curToken.clone();

        if !self.expectPeek(TokenKind::IDENT) {
            return None;
        }

        let name = Identifier {
            token: self.curToken.clone(),
            value: self.curToken.literal.clone(),
        };

        if !self.expectPeek(TokenKind::ASSIGN) {
            return None;
        }

        self.nextToken();

        let value = self.parseExpression(Precedence::LOWEST)?;

        if self.peekTokenIs(TokenKind::SEMICOLON) {
            self.nextToken();
        }

        self.trace_end("let statement");
        Some(LetStatement { token, name, value }.into())
    }

    fn parseReturnStatement(&mut self) -> Option<ReturnStatement> {
        self.trace_begin("return statement");
        let token = self.curToken.clone();
        self.nextToken();

        let returnValue = self.parseExpression(Precedence::LOWEST)?;

        if self.peekTokenIs(TokenKind::SEMICOLON) {
            self.nextToken();
        }

        self.trace_end("return statement");
        Some(ReturnStatement { token, returnValue })
    }

    fn parseExpressionStatement(&mut self) -> Option<ExpressionStatement> {
        self.trace_begin("expression statement");
        let token = self.curToken.clone();

        let expression = self.parseExpression(Precedence::LOWEST)?;

        if self.peekTokenIs(TokenKind::SEMICOLON) {
            self.nextToken();
        }

        self.trace_end("expression statement");
        Some(ExpressionStatement { token, expression })
    }

    fn dispatchPrefix(&mut self, dispatcher: PrefixDispatcher) -> Option<ExpressionEnum> {
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

    fn parseFunctionLiteral(&mut self) -> Option<FunctionLiteral> {
        self.trace_begin("function literal");
        let token = self.curToken.clone();
        if !self.expectPeek(TokenKind::LPAREN) {
            return None;
        }

        let parameters = self.parseFunctionParameters()?;
        if !self.expectPeek(TokenKind::LBRACE) {
            return None;
        }

        let body = self.parseBlockStatement();
        self.trace_end("function literal");
        Some(FunctionLiteral {
            token,
            parameters,
            body: Box::new(body),
        })
    }

    fn parseFunctionParameters(&mut self) -> Option<Vec<Identifier>> {
        self.trace_begin("function parameters");
        let mut identifiers = vec![];
        if self.peekTokenIs(TokenKind::RPAREN) {
            self.nextToken();
            self.trace_end("function parameters");
            return Some(identifiers);
        }

        self.nextToken();
        identifiers.push(Identifier {
            token: self.curToken.clone(),
            value: self.curToken.literal.clone(),
        });
        while self.peekTokenIs(TokenKind::COMMA) {
            self.nextToken();
            self.nextToken();
            identifiers.push(Identifier {
                token: self.curToken.clone(),
                value: self.curToken.literal.clone(),
            });
        }

        if !self.expectPeek(TokenKind::RPAREN) {
            return None;
        }

        self.trace_end("function parameters");
        Some(identifiers)
    }

    fn parseIfExpression(&mut self) -> Option<IfExpression> {
        self.trace_begin("if expression");
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

        self.trace_end("if expression");
        Some(IfExpression {
            token,
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: alternative.map(Box::new),
        })
    }

    fn parseBlockStatement(&mut self) -> BlockStatement {
        self.trace_begin("block statement");
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
        self.trace_end("block statement");
        BlockStatement { token, statements }
    }

    fn parseGroupedExpression(&mut self) -> Option<ExpressionEnum> {
        self.trace_begin("grouped expression");
        self.nextToken();
        let exp = self.parseExpression(Precedence::LOWEST);
        if !self.expectPeek(TokenKind::RPAREN) {
            return None;
        }
        self.trace_end("grouped expression");
        exp
    }

    fn parseBoolean(&mut self) -> Boolean {
        self.trace_begin("boolean literal");
        self.trace_end("boolean literal");
        Boolean {
            token: self.curToken.clone(),
            value: self.curTokenIs(TokenKind::TRUE),
        }
    }

    fn dispatchInfix(
        &mut self,
        left: ExpressionEnum,
        dispatcher: InfixDispatcher,
    ) -> Option<ExpressionEnum> {
        match dispatcher {
            InfixDispatcher::PARSE_INFIX_EXPRESSION => self.parseInfixExpression(left),
            InfixDispatcher::PARSE_CALL_EXPRESSION => self.parseCallExpression(left),
        }
    }

    fn parseCallExpression(&mut self, function: ExpressionEnum) -> Option<ExpressionEnum> {
        self.trace_begin("call expression");
        let token = self.curToken.clone();
        let arguments = self.parseCallArguments()?;
        self.trace_end("call expression");
        Some(
            CallExpression {
                token,
                function: Box::new(function),
                arguments,
            }
            .into(),
        )
    }

    fn parseCallArguments(&mut self) -> Option<Vec<ExpressionEnum>> {
        self.trace_begin("call arguments");
        let mut args = vec![];

        if self.peekTokenIs(TokenKind::RPAREN) {
            self.nextToken();
            self.trace_end("call arguments");
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

        self.trace_end("call arguments");
        Some(args)
    }

    fn parsePrefixExpression(&mut self) -> Option<PrefixExpression> {
        self.trace_begin("prefix expression");
        let token = self.curToken.clone();
        let operator = self.curToken.literal.clone();
        self.nextToken();
        let right = self.parseExpression(Precedence::PREFIX)?;
        self.trace_end("prefix expression");
        Some(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parseIdentifier(&mut self) -> Identifier {
        self.trace_begin("identifier");
        self.trace_end("identifier");
        Identifier {
            token: self.curToken.clone(),
            value: self.curToken.literal.clone(),
        }
    }

    fn parseIntegerLiteral(&mut self) -> Option<IntegerLiteral> {
        self.trace_begin("integer literal");
        let token = self.curToken.clone();

        let value: i64 = self.curToken.literal.parse().ok().or_else(|| {
            self.errors.push(format!(
                r#"could not parse "{}" as integer"#,
                self.curToken.literal
            ));
            None
        })?;

        self.trace_end("integer literal");
        Some(IntegerLiteral { token, value })
    }

    fn parseExpression(&mut self, precedence: Precedence) -> Option<ExpressionEnum> {
        self.trace_begin("expression");
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
        self.trace_end("expression");
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

    fn parseInfixExpression(&mut self, left: ExpressionEnum) -> Option<ExpressionEnum> {
        self.trace_begin("infix expression");
        let token = self.curToken.clone();
        let operator = self.curToken.literal.clone();
        let precedence = self.curPrecedence();
        self.nextToken();
        let right = self.parseExpression(precedence)?;
        self.trace_end("infix expression");
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
mod tests;
