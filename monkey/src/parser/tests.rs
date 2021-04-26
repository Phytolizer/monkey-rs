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
    let ident: Identifier = stmt.expression.try_into().unwrap();
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
    let literal: IntegerLiteral = stmt.expression.try_into().unwrap();
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
        let exp: PrefixExpression = stmt.expression.try_into().unwrap();
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
        let exp: InfixExpression = stmt.expression.try_into().unwrap();
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
    let b: Boolean = stmt.expression.try_into().unwrap();
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
    let exp: IfExpression = stmt.expression.try_into().unwrap();
    testInfixExpression(*exp.condition, "x".into(), "<", "y".into());
    assert_eq!(exp.consequence.statements.len(), 1);
    let consequence: ExpressionStatement =
        exp.consequence.statements[0].clone().try_into().unwrap();
    testIdentifier(consequence.expression, "x");
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
    let exp: IfExpression = stmt.expression.try_into().unwrap();
    testInfixExpression(*exp.condition, "x".into(), "<", "y".into());
    assert_eq!(exp.consequence.statements.len(), 1);
    let consequence: ExpressionStatement =
        exp.consequence.statements[0].clone().try_into().unwrap();
    assert_matches!(
        consequence.expression,
        ExpressionEnum::Identifier(Identifier { value: "x", .. })
    );
    assert_eq!(exp.alternative.as_ref().unwrap().statements.len(), 1);
    let alternative: ExpressionStatement = exp.alternative.unwrap().statements[0]
        .clone()
        .try_into()
        .unwrap();
    assert_matches!(
        alternative.expression,
        ExpressionEnum::Identifier(Identifier { value: "y", .. })
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
    let function: FunctionLiteral = stmt.expression.try_into().unwrap();
    assert_eq!(function.parameters.len(), 2);
    testLiteralExpression(function.parameters[0].clone().into(), "x".into());
    testLiteralExpression(function.parameters[1].clone().into(), "y".into());
    assert_eq!(function.body.statements.len(), 1);
    let body: ExpressionStatement = function.body.statements[0].clone().try_into().unwrap();
    testInfixExpression(body.expression, "x".into(), "+", "y".into());
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
        let function: FunctionLiteral = stmt.expression.try_into().unwrap();
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
    let exp: CallExpression = stmt.expression.try_into().unwrap();
    testIdentifier(*exp.function, "add");
    assert_eq!(exp.arguments.len(), 3);
    testLiteralExpression(exp.arguments[0].clone(), 1.into());
    testInfixExpression(exp.arguments[1].clone(), 2.into(), "*", 3.into());
    testInfixExpression(exp.arguments[2].clone(), 4.into(), "+", 5.into());
}
