mod code_emit;
mod codegen;
mod lexer;
mod parser;
mod tac;

use std::{fmt::Display, fs, io, path::PathBuf};

use code_emit::write_assembly;
use codegen::{do_codegen, CodegenError};
use lexer::{do_lexing, LexerErr};
use parser::{do_parse, ParseError};
use tac::{do_tac, TACError};

use crate::cli::Cli;

pub enum CompileError {
    LexerErr(LexerErr),
    ParseError(ParseError),
    CodegenError(CodegenError),
    WriteError(io::Error),
    TACError(TACError),
}
impl From<LexerErr> for CompileError {
    fn from(value: LexerErr) -> Self {
        Self::LexerErr(value)
    }
}
impl From<ParseError> for CompileError {
    fn from(value: ParseError) -> Self {
        Self::ParseError(value)
    }
}
impl From<CodegenError> for CompileError {
    fn from(value: CodegenError) -> Self {
        Self::CodegenError(value)
    }
}
impl From<io::Error> for CompileError {
    fn from(value: io::Error) -> Self {
        Self::WriteError(value)
    }
}
impl From<TACError> for CompileError {
    fn from(value: TACError) -> Self {
        Self::TACError(value)
    }
}
impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexerErr(e) => write!(f, "Unable to read source file: {}", e),
            Self::ParseError(e) => write!(f, "Parse error: {}", e),
            Self::CodegenError(e) => write!(f, "Codegen error: {}", e),
            Self::WriteError(e) => write!(f, "Write error: {}", e),
            Self::TACError(e) => write!(f, "IR error: {}", e),
        }
    }
}

pub fn do_compile(source: &PathBuf, output_path: &PathBuf, cfg: &Cli) -> Result<(), CompileError> {
    let contents = fs::read_to_string(source).expect("unable to read source");

    let mut tokens = do_lexing(contents)?;
    // dbg!(&tokens);
    if cfg.lex {
        return Ok(());
    }

    let ast = do_parse(&mut tokens)?;
    if cfg.parse {
        return Ok(());
    }

    let tac = do_tac(ast)?;
    if cfg.tac {
        return Ok(());
    }

    let codegen = do_codegen(tac)?;
    if cfg.codegen {
        return Ok(());
    }

    write_assembly(codegen, output_path)?;

    Ok(())
}
