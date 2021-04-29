use crate::lexer::Lexer;
use crate::object::Null;
use crate::object::Object;
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
    let result = match obj {
        ObjectEnum::Integer(i) => i,
        _ => panic!("testIntegerObject: obj was {}", obj.Inspect()),
    };
    assert_eq!(result.value, expected);
}

fn testBooleanObject(obj: ObjectEnum, expected: bool) {
    let result = match obj {
        ObjectEnum::Boolean(b) => b,
        _ => panic!("testBooleanObject: obj was {}", obj.Inspect()),
    };
    assert_eq!(result.value, expected);
}

#[test]
fn EvalIntegerExpression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];
    for (input, expected) in tests {
        let evaluated = testEval(input);
        testIntegerObject(evaluated.unwrap(), expected);
    }
}

#[test]
fn EvalBooleanExpression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];
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

fn testNullObject(obj: ObjectEnum) {
    assert_eq!(obj, Null.into())
}

#[test]
fn EvalIfExpression() {
    let tests = vec![
        ("if (true) { 10 }", Some(10)),
        ("if (false) { 10 }", None),
        ("if (1) { 10 }", Some(10)),
        ("if (1 < 2) { 10 }", Some(10)),
        ("if (1 > 2) { 10 }", None),
        ("if (1 > 2) { 10 } else { 20 }", Some(20)),
        ("if (1 < 2) { 10 } else { 20 }", Some(10)),
    ];

    for (input, expected) in tests {
        let evaluated = testEval(input);
        if let Some(integer) = expected {
            testIntegerObject(evaluated.unwrap(), integer);
        } else {
            testNullObject(evaluated.unwrap());
        }
    }
}

#[test]
fn EvalReturnStatement() {
    let tests = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        (
            "
            if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }
            ",
            10,
        ),
    ];
    for (input, expected) in tests {
        let evaluated = testEval(input);
        let evaluated = evaluated.unwrap();
        testIntegerObject(evaluated, expected);
    }
}

#[test]
fn ErrorHandling() {
    let tests = vec![
        ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
        ("5; true + false; 5;", "unknown operator: BOOLEAN + BOOLEAN"),
        (
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        (
            "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
    ];
    for (input, expected) in tests {
        let evaluated = testEval(input).unwrap();
        dbg!(&evaluated);
        let err_obj = match evaluated {
            ObjectEnum::Error(e) => e,
            _ => panic!("no error message, got={}", evaluated.Inspect()),
        };
        assert_eq!(err_obj.0.as_str(), expected);
    }
}
