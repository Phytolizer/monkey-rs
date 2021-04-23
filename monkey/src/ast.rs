use enum_dispatch::enum_dispatch;

use crate::token::Token;

#[enum_dispatch]
pub(crate) enum NodeEnum<'src> {
    Expression(ExpressionEnum<'src>),
    Statement(StatementEnum<'src>),
    Program(Program<'src>),
}

#[enum_dispatch(Node)]
pub(crate) trait Node<'src> {
    fn TokenLiteral(&self) -> &'src str;
    fn String(&self) -> String;
}

#[enum_dispatch]
pub(crate) enum StatementEnum<'src> {
    LetStatement(LetStatement<'src>),
}

impl<'src> Node<'src> for StatementEnum<'src> {
    fn TokenLiteral(&self) -> &'src str {
        todo!()
    }

    fn String(&self) -> String {
        todo!()
    }
}

#[enum_dispatch(StatementEnum)]
pub(crate) trait Statement<'src>: Node<'src> {}

pub(crate) enum ExpressionEnum<'src> {
    Identifier(Identifier<'src>),
}

impl<'src> Node<'src> for ExpressionEnum<'src> {
    fn TokenLiteral(&self) -> &'src str {
        todo!()
    }

    fn String(&self) -> String {
        todo!()
    }
}

#[enum_dispatch(ExpressionEnum)]
pub(crate) trait Expression<'src>: Node<'src> {}

pub(crate) struct Program<'src> {
    pub(crate) statements: Vec<StatementEnum<'src>>,
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

pub(crate) struct LetStatement<'src> {
    pub(crate) token: Token<'src>,
    pub(crate) name: Identifier<'src>,
    pub(crate) value: Option<ExpressionEnum<'src>>,
}

impl<'src> Node<'src> for LetStatement<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        if let Some(value) = &self.value {
            format!("let {} = {};", self.name.String(), value.String())
        } else {
            format!("let {} = ;", self.name.String())
        }
    }
}

impl<'src> Expression<'src> for LetStatement<'src> {}

pub(crate) struct Identifier<'src> {
    pub(crate) token: Token<'src>,
}

impl<'src> Node<'src> for Identifier<'src> {
    fn TokenLiteral(&self) -> &'src str {
        self.token.literal
    }

    fn String(&self) -> String {
        self.token.literal.to_string()
    }
}
