use enum_dispatch::enum_dispatch;

use crate::token::Token;

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum NodeEnum<'src> {
    Expression(ExpressionEnum<'src>),
    Statement(StatementEnum<'src>),
    Program(Program<'src>),
}

#[enum_dispatch(NodeEnum)]
pub trait Node<'src>: std::fmt::Debug + Clone {
    fn TokenLiteral(&self) -> &'src str;
    fn String(&self) -> String;
}

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum StatementEnum<'src> {
    Let(LetStatement<'src>),
    Return(ReturnStatement<'src>),
    Expression(ExpressionStatement<'src>),
}

impl<'src> Node<'src> for StatementEnum<'src> {
    fn TokenLiteral(&self) -> &'src str {
        match self {
            Self::Let(s) => s.TokenLiteral(),
            Self::Return(s) => s.TokenLiteral(),
            Self::Expression(s) => s.TokenLiteral(),
        }
    }

    fn String(&self) -> String {
        match self {
            Self::Let(s) => s.String(),
            Self::Return(s) => s.String(),
            Self::Expression(s) => s.String(),
        }
    }
}

#[enum_dispatch(StatementEnum)]
pub trait Statement<'src>: Node<'src> {}

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum ExpressionEnum<'src> {
    Identifier(Identifier<'src>),
    IntegerLiteral(IntegerLiteral<'src>),
    PrefixExpression(PrefixExpression<'src>),
    InfixExpression(InfixExpression<'src>),
    Boolean(Boolean<'src>),
    IfExpression(IfExpression<'src>),
    FunctionLiteral(FunctionLiteral<'src>),
    CallExpression(CallExpression<'src>),
}

impl<'src> Node<'src> for ExpressionEnum<'src> {
    fn TokenLiteral(&self) -> &'src str {
        match self {
            Self::Identifier(e) => e.TokenLiteral(),
            Self::IntegerLiteral(e) => e.TokenLiteral(),
            Self::PrefixExpression(e) => e.TokenLiteral(),
            Self::InfixExpression(e) => e.TokenLiteral(),
            Self::Boolean(e) => e.TokenLiteral(),
            Self::IfExpression(e) => e.TokenLiteral(),
            Self::FunctionLiteral(e) => e.TokenLiteral(),
            Self::CallExpression(e) => e.TokenLiteral(),
        }
    }

    fn String(&self) -> String {
        match self {
            Self::Identifier(e) => e.String(),
            Self::IntegerLiteral(e) => e.String(),
            Self::PrefixExpression(e) => e.String(),
            Self::InfixExpression(e) => e.String(),
            Self::Boolean(e) => e.String(),
            Self::IfExpression(e) => e.String(),
            Self::FunctionLiteral(e) => e.String(),
            Self::CallExpression(e) => e.String(),
        }
    }
}

#[enum_dispatch(ExpressionEnum)]
pub trait Expression<'src>: Node<'src> {}

#[derive(Debug, Clone)]
pub struct Program<'src> {
    pub statements: Vec<StatementEnum<'src>>,
}

impl<'src> Node<'src> for Program<'src> {
    fn TokenLiteral(&self) -> &'src str {
        if !self.statements.is_empty() {
            self.statements[0].TokenLiteral()
        } else {
            ""
        }
    }

    fn String(&self) -> String {
        self.statements
            .iter()
            .map(|stmt| stmt.String())
            .reduce(|acc, stmt| acc + &stmt)
            .unwrap_or_default()
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement<'src> {
    pub token: Token<'src>,
    pub name: Identifier<'src>,
    pub value: ExpressionEnum<'src>,
}

impl<'src> Node<'src> for LetStatement<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        format!("let {} = {};", self.name.String(), self.value.String())
    }
}

impl<'src> Expression<'src> for LetStatement<'src> {}

#[derive(Debug, Clone)]
pub struct Identifier<'src> {
    pub token: Token<'src>,
    pub value: &'src str,
}

impl<'src> Node<'src> for Identifier<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        self.token.literal.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement<'src> {
    pub token: Token<'src>,
    pub returnValue: ExpressionEnum<'src>,
}

impl<'src> Node<'src> for ReturnStatement<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        format!("return {};", self.returnValue.String())
    }
}

impl<'src> Statement<'src> for ReturnStatement<'src> {}

#[derive(Debug, Clone)]
pub struct ExpressionStatement<'src> {
    pub token: Token<'src>,
    pub expression: ExpressionEnum<'src>,
}

impl<'src> Node<'src> for ExpressionStatement<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        self.expression.String()
    }
}

impl<'src> Statement<'src> for ExpressionStatement<'src> {}

#[derive(Debug, Clone)]
pub struct IntegerLiteral<'src> {
    pub token: Token<'src>,
    pub value: i64,
}

impl<'src> Node<'src> for IntegerLiteral<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        self.TokenLiteral().to_string()
    }
}

impl<'src> Expression<'src> for IntegerLiteral<'src> {}

#[derive(Debug, Clone)]
pub struct PrefixExpression<'src> {
    pub token: Token<'src>,
    pub operator: &'src str,
    pub right: Box<ExpressionEnum<'src>>,
}

impl<'src> Node<'src> for PrefixExpression<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        format!("({}{})", self.operator, self.right.String())
    }
}

impl<'src> Expression<'src> for PrefixExpression<'src> {}

#[derive(Debug, Clone)]
pub struct InfixExpression<'src> {
    pub token: Token<'src>,
    pub left: Box<ExpressionEnum<'src>>,
    pub operator: &'src str,
    pub right: Box<ExpressionEnum<'src>>,
}

impl<'src> Node<'src> for InfixExpression<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        format!(
            "({} {} {})",
            self.left.String(),
            self.operator,
            self.right.String()
        )
    }
}

impl<'src> Expression<'src> for InfixExpression<'src> {}

#[derive(Debug, Clone)]
pub struct Boolean<'src> {
    pub token: Token<'src>,
    pub value: bool,
}

impl<'src> Node<'src> for Boolean<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        self.TokenLiteral().to_string()
    }
}

impl<'src> Expression<'src> for Boolean<'src> {}

#[derive(Debug, Clone)]
pub struct IfExpression<'src> {
    pub token: Token<'src>,
    pub condition: Box<ExpressionEnum<'src>>,
    pub consequence: Box<BlockStatement<'src>>,
    pub alternative: Option<Box<BlockStatement<'src>>>,
}

impl<'src> Node<'src> for IfExpression<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        let mut out = format!(
            "if{} {}",
            self.condition.String(),
            self.consequence.String()
        );
        if let Some(alternative) = self.alternative.as_ref() {
            out += &format!(" else {}", alternative.String());
        }
        out
    }
}

impl<'src> Expression<'src> for IfExpression<'src> {}

#[derive(Debug, Clone)]
pub struct BlockStatement<'src> {
    pub token: Token<'src>,
    pub statements: Vec<StatementEnum<'src>>,
}

impl<'src> Node<'src> for BlockStatement<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        itertools::join(self.statements.iter().map(|s| s.String()), "")
    }
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral<'src> {
    pub token: Token<'src>,
    pub parameters: Vec<Identifier<'src>>,
    pub body: Box<BlockStatement<'src>>,
}

impl<'src> Node<'src> for FunctionLiteral<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        format!(
            "{}({}){}",
            self.TokenLiteral(),
            itertools::join(self.parameters.iter().map(|p| p.String()), ", "),
            self.body.String()
        )
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression<'src> {
    pub token: Token<'src>,
    pub function: Box<ExpressionEnum<'src>>,
    pub arguments: Vec<ExpressionEnum<'src>>,
}

impl<'src> Node<'src> for CallExpression<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        format!(
            "{}({})",
            self.function.String(),
            itertools::join(self.arguments.iter().map(|a| a.String()), ", ")
        )
    }
}

impl<'src> Expression<'src> for CallExpression<'src> {}

#[cfg(test)]
mod tests {
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
}
