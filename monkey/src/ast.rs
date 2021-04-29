use enum_dispatch::enum_dispatch;

use crate::token::Token;

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum NodeEnum {
    Expression(ExpressionEnum),
    Statement(StatementEnum),
    Program(Program),
}

#[enum_dispatch(NodeEnum)]
pub trait Node: std::fmt::Debug + Clone {
    fn TokenLiteral(&self) -> &str;
    fn String(&self) -> String;
}

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum StatementEnum {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl Node for StatementEnum {
    fn TokenLiteral(&self) -> &str {
        match self {
            Self::Let(s) => s.TokenLiteral(),
            Self::Return(s) => s.TokenLiteral(),
            Self::Expression(s) => s.TokenLiteral(),
            Self::Block(s) => s.TokenLiteral(),
        }
    }

    fn String(&self) -> String {
        match self {
            Self::Let(s) => s.String(),
            Self::Return(s) => s.String(),
            Self::Expression(s) => s.String(),
            Self::Block(s) => s.String(),
        }
    }
}

#[enum_dispatch(StatementEnum)]
pub trait Statement: Node {}

#[enum_dispatch]
#[derive(Debug, Clone)]
pub enum ExpressionEnum {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

impl Node for ExpressionEnum {
    fn TokenLiteral(&self) -> &str {
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
pub trait Expression: Node {}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<StatementEnum>,
}

impl Node for Program {
    fn TokenLiteral(&self) -> &str {
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
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: ExpressionEnum,
}

impl Node for LetStatement {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
    }

    fn String(&self) -> String {
        format!("let {} = {};", self.name.String(), self.value.String())
    }
}

impl Expression for LetStatement {}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
    }

    fn String(&self) -> String {
        self.token.literal.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub returnValue: ExpressionEnum,
}

impl Node for ReturnStatement {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
    }

    fn String(&self) -> String {
        format!("return {};", self.returnValue.String())
    }
}

impl Statement for ReturnStatement {}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: ExpressionEnum,
}

impl Node for ExpressionStatement {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
    }

    fn String(&self) -> String {
        self.expression.String()
    }
}

impl Statement for ExpressionStatement {}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
    }

    fn String(&self) -> String {
        self.TokenLiteral().to_string()
    }
}

impl Expression for IntegerLiteral {}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<ExpressionEnum>,
}

impl Node for PrefixExpression {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
    }

    fn String(&self) -> String {
        format!("({}{})", self.operator, self.right.String())
    }
}

impl Expression for PrefixExpression {}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<ExpressionEnum>,
    pub operator: String,
    pub right: Box<ExpressionEnum>,
}

impl Node for InfixExpression {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
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

impl Expression for InfixExpression {}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
    }

    fn String(&self) -> String {
        self.TokenLiteral().to_string()
    }
}

impl Expression for Boolean {}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<ExpressionEnum>,
    pub consequence: Box<BlockStatement>,
    pub alternative: Option<Box<BlockStatement>>,
}

impl Node for IfExpression {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
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

impl Expression for IfExpression {}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<StatementEnum>,
}

impl Node for BlockStatement {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
    }

    fn String(&self) -> String {
        itertools::join(self.statements.iter().map(|s| s.String()), "")
    }
}

impl Statement for BlockStatement {}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Box<BlockStatement>,
}

impl Node for FunctionLiteral {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
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
pub struct CallExpression {
    pub token: Token,
    pub function: Box<ExpressionEnum>,
    pub arguments: Vec<ExpressionEnum>,
}

impl Node for CallExpression {
    fn TokenLiteral(&self) -> &str {
        &self.token.literal
    }

    fn String(&self) -> String {
        format!(
            "{}({})",
            self.function.String(),
            itertools::join(self.arguments.iter().map(|a| a.String()), ", ")
        )
    }
}

impl Expression for CallExpression {}

#[cfg(test)]
mod tests;
