use std::{collections::HashMap, fmt::Display};

use lazy_static::lazy_static;

use super::lexer::{Keyword, Operator, Token, Tokens};

#[derive(Debug)]
pub struct Program(pub Function);

#[derive(Debug)]
pub struct Function(pub Identifier, pub Statement);

#[derive(Debug)]
pub enum Statement {
    Return(Expr),
}

#[derive(Debug)]
pub enum Expr {
    IntConstant(i32),
    Unary(UnaryOperator, Box<Expr>),
    Binary(BinaryOperator, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug)]
pub enum UnaryOperator {
    BitwiseComp,
    Negate,
    Not,
}

lazy_static! {
    static ref OPERATOR_PRECEDENCE: HashMap<String, usize> = {
        let mut m = HashMap::new();
        m.insert("*".into(), 50);
        m.insert("/".into(), 50);
        m.insert("%".into(), 50);
        m.insert("+".into(), 45);
        m.insert("-".into(), 45);
        m.insert("<".into(), 35);
        m.insert("<=".into(), 35);
        m.insert(">".into(), 35);
        m.insert(">=".into(), 35);
        m.insert("==".into(), 30);
        m.insert("!=".into(), 30);
        m.insert("&&".into(), 10);
        m.insert("||".into(), 5);
        m
    };
}

#[derive(Clone, Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(String),
    UnexpectedEOF,
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SyntaxError(e) => write!(f, "Syntax error: {}", e),
            Self::UnexpectedEOF => write!(f, "Unexpected EOF"),
        }
    }
}

pub fn do_parse(tokens: &mut Tokens) -> Result<Program, ParseError> {
    let func = parse_function(tokens)?;
    dbg!(&func);
    Ok(Program(func))
}

fn parse_function(tokens: &mut Tokens) -> Result<Function, ParseError> {
    expect(Token::Keyword(Keyword::Int), tokens)?;
    let ident = parse_identifier(tokens)?;
    expect(Token::OpenParen, tokens)?;
    expect(Token::Keyword(Keyword::Void), tokens)?;
    expect(Token::CloseParen, tokens)?;
    expect(Token::OpenBrace, tokens)?;
    let statement = parse_statement(tokens)?;
    expect(Token::CloseBrace, tokens)?;
    Ok(Function(ident, statement))
}

fn parse_statement(tokens: &mut Tokens) -> Result<Statement, ParseError> {
    expect(Token::Keyword(Keyword::Return), tokens)?;
    let expr = parse_expression(tokens, 0)?;
    expect(Token::Semi, tokens)?;
    Ok(Statement::Return(expr))
}

fn parse_factor(tokens: &mut Tokens) -> Result<Expr, ParseError> {
    match peek(tokens) {
        Some(Token::IntConstant(_)) => {
            let t = tokens.pop_front().expect("token error");
            if let Token::IntConstant(c) = t {
                Ok(Expr::IntConstant(c))
            } else {
                Err(ParseError::SyntaxError("malformed int".into()))
            }
        }
        Some(Token::Operator(Operator::BitwiseComp))
        | Some(Token::Operator(Operator::Minus))
        | Some(Token::Operator(Operator::Exclamation)) => {
            let op = parse_unary(tokens)?;
            let inner_exp = parse_factor(tokens)?;
            Ok(Expr::Unary(op, Box::new(inner_exp)))
        }
        Some(Token::OpenParen) => {
            tokens.pop_front();
            let inner_exp = parse_expression(tokens, 0)?;
            expect(Token::CloseParen, tokens)?;
            Ok(inner_exp)
        }
        _ => Err(ParseError::SyntaxError("malformed syntax".into())),
    }
}

fn parse_expression(tokens: &mut Tokens, min_prec: usize) -> Result<Expr, ParseError> {
    let mut left = parse_factor(tokens)?;
    let mut next_token = peek(tokens);
    while let Some(Token::Operator(op)) = next_token {
        let precedence =
            get_precedence(op).ok_or(ParseError::SyntaxError("unexpected symbol".into()))?;
        if precedence >= min_prec {
            let operator = parse_binary(tokens)?;
            let right = parse_expression(tokens, precedence + 1)?;
            left = Expr::Binary(operator, Box::new(left), Box::new(right));
            next_token = peek(tokens);
        } else {
            break;
        }
    }
    Ok(left)
}

fn parse_binary(tokens: &mut Tokens) -> Result<BinaryOperator, ParseError> {
    let t = tokens.pop_front().ok_or(ParseError::UnexpectedEOF)?;
    match t {
        Token::Operator(o) => match o {
            Operator::Asterisk => Ok(BinaryOperator::Multiply),
            Operator::FSlash => Ok(BinaryOperator::Divide),
            Operator::Minus => Ok(BinaryOperator::Subtract),
            Operator::Plus => Ok(BinaryOperator::Add),
            Operator::Percent => Ok(BinaryOperator::Remainder),
            Operator::And => Ok(BinaryOperator::And),
            Operator::Or => Ok(BinaryOperator::Or),
            Operator::EqualTo => Ok(BinaryOperator::Equal),
            Operator::NotEqualTo => Ok(BinaryOperator::NotEqual),
            Operator::LessThan => Ok(BinaryOperator::LessThan),
            Operator::LessThanEqual => Ok(BinaryOperator::LessOrEqual),
            Operator::GreaterThan => Ok(BinaryOperator::GreaterThan),
            Operator::GreaterThanEqual => Ok(BinaryOperator::GreaterOrEqual),
            _ => Err(ParseError::SyntaxError("unexpected binary operator".into())),
        },
        _ => Err(ParseError::SyntaxError("unexpected character".into())),
    }
}

fn parse_unary(tokens: &mut Tokens) -> Result<UnaryOperator, ParseError> {
    let t = tokens.pop_front().ok_or(ParseError::UnexpectedEOF)?;
    match t {
        Token::Operator(Operator::BitwiseComp) => Ok(UnaryOperator::BitwiseComp),
        Token::Operator(Operator::Minus) => Ok(UnaryOperator::Negate),
        Token::Operator(Operator::Exclamation) => Ok(UnaryOperator::Not),
        _ => Err(ParseError::SyntaxError("malformed syntax".into())),
    }
}

fn parse_identifier(tokens: &mut Tokens) -> Result<Identifier, ParseError> {
    let t = tokens.pop_front().ok_or(ParseError::UnexpectedEOF)?;
    if let Token::Ident(i) = t {
        Ok(Identifier(i))
    } else {
        Err(ParseError::SyntaxError("invalid identifier".into()))
    }
}

fn expect(expected: Token, tokens: &mut Tokens) -> Result<(), ParseError> {
    let actual = tokens.pop_front().ok_or(ParseError::UnexpectedEOF)?;
    if expected != actual {
        Err(ParseError::SyntaxError("Unexpected token".into()))
    } else {
        Ok(())
    }
}

fn peek(tokens: &Tokens) -> Option<&Token> {
    tokens.get(0)
}

fn get_precedence(op: &Operator) -> Option<usize> {
    OPERATOR_PRECEDENCE.get(&op.to_string()).cloned()
}
