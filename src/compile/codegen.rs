use std::fmt::Display;

use super::parser::{Expr, Function, Identifier, Program, Statement};

#[derive(Debug)]
pub struct AProgram(pub AFunction);

#[derive(Debug)]
pub struct AFunction(pub Identifier, pub Vec<Instruction>);

#[derive(Debug)]
pub enum Instruction {
    Mov(Operand, Operand),
    Ret,
}

#[derive(Debug)]
pub enum Operand {
    Imm(isize),
    Register,
}

#[derive(Debug)]
pub enum CodegenError {
    Err(String),
}
impl Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Err(e) => write!(f, "Codegen error: {}", e),
        }
    }
}

pub fn do_codegen(ast: Program) -> Result<AProgram, CodegenError> {
    Ok(AProgram(gen_function(ast.0)?))
}

fn gen_function(func: Function) -> Result<AFunction, CodegenError> {
    let statements = vec![func.1];
    let mut instructions: Vec<Instruction> = Vec::new();

    for s in statements {
        let s_instructions = gen_instructions(s)?;
        instructions.extend(s_instructions);
    }

    Ok(AFunction(func.0, instructions))
}

fn gen_instructions(statement: Statement) -> Result<Vec<Instruction>, CodegenError> {
    match statement {
        Statement::Return(expr) => {
            let val = gen_expression(expr)?;
            Ok(vec![
                Instruction::Mov(val, Operand::Register),
                Instruction::Ret,
            ])
        }
    }
}

fn gen_expression(expr: Expr) -> Result<Operand, CodegenError> {
    match expr {
        Expr::IntConstant(c) => Ok(Operand::Imm(c)),
    }
}
