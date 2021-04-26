use std::convert::TryInto;

use crate::lexer::Lexer;
use crate::object::Boolean;
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

fn testBooleanObject(obj: ObjectEnum, expected: bool) {
    let result: Boolean = obj.try_into().unwrap();
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

#[test]
fn EvalBooleanExpression() {
    let tests = vec![("true", true), ("false", false)];
    for (input, expected) in tests {
        let evaluated = testEval(input);
        testBooleanObject(evaluated.unwrap(), expected);
    }
}

#[test]
fn EvalBangOperator() {
    let tests = vec![
        ("!true;", false),
        ("!false;", true),
        ("!5;", false),
        ("!!true;", true),
        ("!!false;", false),
        ("!!5;", true),
    ];
    for (input, expected) in tests {
        let evaluated = testEval(input);
        testBooleanObject(evaluated.unwrap(), expected);
    }
}
