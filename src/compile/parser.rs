use std::fmt::Display;

use super::lexer::{Keyword, Token, Tokens};

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
    IntConstant(isize),
}

#[derive(Debug)]
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
    let expr = parse_expression(tokens)?;
    expect(Token::Semi, tokens)?;
    Ok(Statement::Return(expr))
}

fn parse_expression(tokens: &mut Tokens) -> Result<Expr, ParseError> {
    let t = tokens.pop_front().ok_or(ParseError::UnexpectedEOF)?;
    if let Token::IntConstant(c) = t {
        Ok(Expr::IntConstant(c))
    } else {
        Err(ParseError::SyntaxError("Invalid integer".into()))
    }
}

fn parse_identifier(tokens: &mut Tokens) -> Result<Identifier, ParseError> {
    let t = tokens.pop_front().ok_or(ParseError::UnexpectedEOF)?;
    if let Token::Ident(i) = t {
        Ok(Identifier(i))
    } else {
        Err(ParseError::SyntaxError("Invalid identifier".into()))
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
