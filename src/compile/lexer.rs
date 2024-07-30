use std::{collections::VecDeque, fmt::Display};

use lazy_static::lazy_static;
use regex::Regex;

pub type Tokens = VecDeque<Token>;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Ident(String),
    IntConstant(i32),
    Keyword(Keyword),
    UnaryOp(UnaryOp),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semi,
}

#[derive(PartialEq, Clone, Debug)]
pub enum UnaryOp {
    BitwiseComp,
    Negate,
    Decrement,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Keyword {
    Int,
    Void,
    Return,
}

#[derive(Clone, Debug)]
pub enum LexerErr {
    NoToken,
    InvalidToken(String),
}
impl Display for LexerErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidToken(e) => write!(f, "Invalid token: {}", e),
            Self::NoToken => write!(f, "Big fail"),
        }
    }
}

lazy_static! {
    static ref WORD: Regex = Regex::new(r"^[a-zA-Z_]\w*\b").unwrap();
    static ref INT_CONSTANT: Regex = Regex::new(r"^[0-9]+\b").unwrap();
    static ref INT_KEYWORD: Regex = Regex::new(r"^int\b").unwrap();
    static ref VOID_KEYWORD: Regex = Regex::new(r"^void\b").unwrap();
    static ref RETURN_KEYWORD: Regex = Regex::new(r"^return\b").unwrap();
    static ref DECREMENT_OP: Regex = Regex::new(r"^--").unwrap();
}

pub fn do_lexing(source: String) -> Result<Tokens, LexerErr> {
    let mut curr_src = source.as_str();
    let mut tokens = VecDeque::new();
    while !curr_src.trim_start().is_empty() {
        let (token, new_src) = pop_next(curr_src.trim_start())?;
        tokens.push_back(token);
        curr_src = new_src;
    }
    dbg!(&tokens);
    Ok(tokens)
}

fn pop_next(source: &str) -> Result<(Token, &str), LexerErr> {
    if let Some(word) = WORD.find(&source) {
        match match_keyword(word.as_str()) {
            Some(kw) => Ok((Token::Keyword(kw), &source[word.as_str().len()..])),
            None => Ok((
                Token::Ident(word.as_str().to_string()),
                &source[word.as_str().len()..],
            )),
        }
    } else if let Some(int_const) = INT_CONSTANT.find(source) {
        Ok((
            Token::IntConstant(int_const.as_str().parse().expect("invalid int")),
            &source[int_const.len()..],
        ))
    } else if DECREMENT_OP.is_match(source) {
        Ok((Token::UnaryOp(UnaryOp::Decrement), &source[2..]))
    } else {
        let t = match source.chars().nth(0) {
            Some('(') => Ok(Token::OpenParen),
            Some(')') => Ok(Token::CloseParen),
            Some('{') => Ok(Token::OpenBrace),
            Some('}') => Ok(Token::CloseBrace),
            Some(';') => Ok(Token::Semi),
            Some('~') => Ok(Token::UnaryOp(UnaryOp::BitwiseComp)),
            Some('-') => Ok(Token::UnaryOp(UnaryOp::Negate)),
            Some(t) => Err(LexerErr::InvalidToken(t.to_string())),
            None => Err(LexerErr::NoToken),
        }?;
        Ok((t, &source[1..]))
    }
}

fn match_keyword(word: &str) -> Option<Keyword> {
    if INT_KEYWORD.is_match(word) {
        Some(Keyword::Int)
    } else if VOID_KEYWORD.is_match(word) {
        Some(Keyword::Void)
    } else if RETURN_KEYWORD.is_match(word) {
        Some(Keyword::Return)
    } else {
        None
    }
}
