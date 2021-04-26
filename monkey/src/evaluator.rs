use crate::ast::ExpressionEnum;
use crate::ast::NodeEnum;
use crate::ast::StatementEnum;
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
        _ => Some(Null.into()),
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
mod tests {
    use std::convert::TryInto;

    use crate::lexer::Lexer;
    use crate::object::Integer;
    use crate::object::ObjectEnum;
    use crate::parser::Parser;

    use super::Eval;

    fn testEval(input: &str) -> Option<ObjectEnum> {
        let l = Lexer::New(input);
        let mut p = Parser::New(l);
        let program = p.ParseProgram();

        Eval(program.into())
    }

    fn testIntegerObject(obj: ObjectEnum, expected: i64) {
        let result: Integer = obj.try_into().unwrap();
        assert_eq!(result.value, expected);
    }

    #[test]
    fn EvalIntegerExpression() {
        let tests = vec![("5", 5), ("10", 10)];
        for (input, expected) in tests {
            let evaluated = testEval(input);
            testIntegerObject(evaluated.unwrap(), expected);
        }
    }
}
