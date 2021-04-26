use std::convert::TryInto;

use crate::ast::BlockStatement;
use crate::ast::ExpressionEnum;
use crate::ast::IfExpression;
use crate::ast::NodeEnum;
use crate::ast::Program;
use crate::ast::StatementEnum;
use crate::object::Boolean;
use crate::object::Integer;
use crate::object::Null;
use crate::object::Object;
use crate::object::ObjectEnum;
use crate::object::ObjectKind;
use crate::object::ReturnValue;
use crate::object::Truthy;

pub fn Eval(node: NodeEnum) -> Option<ObjectEnum> {
    match node {
        NodeEnum::Program(p) => evalProgram(p),
        NodeEnum::Statement(StatementEnum::Expression(e)) => Eval(e.expression.into()),
        NodeEnum::Statement(StatementEnum::Block(b)) => evalBlockStatement(b),
        NodeEnum::Statement(StatementEnum::Return(r)) => {
            let val = Eval(r.returnValue.into())?;
            Some(ReturnValue(Box::new(val)).into())
        }
        NodeEnum::Expression(ExpressionEnum::IntegerLiteral(i)) => {
            Some(Integer { value: i.value }.into())
        }
        NodeEnum::Expression(ExpressionEnum::Boolean(b)) => Some(Boolean { value: b.value }.into()),
        NodeEnum::Expression(ExpressionEnum::PrefixExpression(p)) => {
            let right = Eval((*p.right).into())?;
            Some(evalPrefixExpression(p.operator, right))
        }
        NodeEnum::Expression(ExpressionEnum::InfixExpression(i)) => {
            let left = Eval((*i.left).into())?;
            let right = Eval((*i.right).into())?;
            Some(evalInfixExpression(i.operator, left, right))
        }
        NodeEnum::Expression(ExpressionEnum::IfExpression(i)) => evalIfExpression(i),
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

fn evalInfixExpression(operator: &str, left: ObjectEnum, right: ObjectEnum) -> ObjectEnum {
    if left.Type() == ObjectKind::INTEGER && right.Type() == ObjectKind::INTEGER {
        evalIntegerInfixExpression(operator, left, right)
    } else if operator == "==" {
        Boolean {
            value: left == right,
        }
        .into()
    } else if operator == "!=" {
        Boolean {
            value: left != right,
        }
        .into()
    } else {
        Null.into()
    }
}

fn evalIntegerInfixExpression(operator: &str, left: ObjectEnum, right: ObjectEnum) -> ObjectEnum {
    let left: Integer = left.try_into().unwrap();
    let right: Integer = right.try_into().unwrap();
    match operator {
        "+" => Integer {
            value: left.value + right.value,
        }
        .into(),
        "-" => Integer {
            value: left.value - right.value,
        }
        .into(),
        "*" => Integer {
            value: left.value * right.value,
        }
        .into(),
        "/" => Integer {
            value: left.value / right.value,
        }
        .into(),
        "<" => Boolean {
            value: left.value < right.value,
        }
        .into(),
        ">" => Boolean {
            value: left.value > right.value,
        }
        .into(),
        "==" => Boolean {
            value: left.value == right.value,
        }
        .into(),
        "!=" => Boolean {
            value: left.value != right.value,
        }
        .into(),
        _ => Null.into(),
    }
}

fn evalIfExpression(i: IfExpression) -> Option<ObjectEnum> {
    let condition = Eval((*i.condition).into())?;
    if condition.isTruthy() {
        let consequence_stmt: StatementEnum = (*i.consequence).into();
        Eval(consequence_stmt.into())
    } else if let Some(alternative) = i.alternative {
        let alternative_stmt: StatementEnum = (*alternative).into();
        Eval(alternative_stmt.into())
    } else {
        Some(Null.into())
    }
}

fn evalProgram(program: Program) -> Option<ObjectEnum> {
    let mut result: Option<ObjectEnum> = None;
    for stmt in program.statements {
        result = Eval(stmt.into());

        if let Some(ObjectEnum::ReturnValue(rv)) = result {
            return Some(rv.into());
        }
    }

    result
}

fn evalBlockStatement(bs: BlockStatement) -> Option<ObjectEnum> {
    let mut result: Option<ObjectEnum> = None;
    for statement in bs.statements {
        result = Eval(statement.into());
        if let Some(ObjectEnum::ReturnValue(_)) = &result {
            return result;
        }
    }
    result
}

#[cfg(test)]
mod tests;
