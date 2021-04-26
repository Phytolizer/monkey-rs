use crate::ast::ExpressionEnum;
use crate::ast::NodeEnum;
use crate::ast::StatementEnum;
use crate::object::Boolean;
use crate::object::Integer;
use crate::object::Null;
use crate::object::ObjectEnum;

pub fn Eval(node: NodeEnum) -> Option<ObjectEnum> {
    match node {
        NodeEnum::Program(p) => evalStatements(p.statements),
        NodeEnum::Statement(StatementEnum::Expression(e)) => Eval(e.expression.into()),
        NodeEnum::Expression(ExpressionEnum::IntegerLiteral(i)) => {
            Some(Integer { value: i.value }.into())
        }
        NodeEnum::Expression(ExpressionEnum::Boolean(b)) => Some(Boolean { value: b.value }.into()),
        NodeEnum::Expression(ExpressionEnum::PrefixExpression(p)) => {
            let right = Eval((*p.right).into())?;
            Some(evalPrefixExpression(p.operator, right))
        }
        _ => Some(Null.into()),
    }
}

fn evalPrefixExpression(operator: &str, right: ObjectEnum) -> ObjectEnum {
    match operator {
        "!" => evalBangOperatorExpression(right),
        "-" => evalMinusPrefixOperatorExpression(right),
        _ => Null.into(),
    }
}

fn evalBangOperatorExpression(right: ObjectEnum) -> ObjectEnum {
    match right {
        ObjectEnum::Boolean(Boolean { value }) => Boolean {
            value: match value {
                true => false,
                false => true,
            },
        }
        .into(),
        ObjectEnum::Null(Null) => Boolean { value: true }.into(),
        _ => Boolean { value: false }.into(),
    }
}

fn evalMinusPrefixOperatorExpression(right: ObjectEnum) -> ObjectEnum {
    match right {
        ObjectEnum::Integer(Integer { value }) => Integer { value: -value }.into(),
        _ => Null.into(),
    }
}

fn evalStatements(statements: Vec<StatementEnum>) -> Option<ObjectEnum> {
    let mut result: Option<ObjectEnum> = Some(Null.into());
    for stmt in statements {
        result = Eval(stmt.into());
    }

    result
}

#[cfg(test)]
mod tests;
