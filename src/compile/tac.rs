use std::{
    fmt::Display,
    sync::atomic::{AtomicUsize, Ordering},
};

use super::parser::{Expr, Function, Identifier, Program, Statement, UnaryOperation};

#[derive(Debug)]
pub struct TACProgram(pub TACFunction);

#[derive(Debug)]
pub struct TACFunction(pub Identifier, pub Vec<TACInstruction>);

#[derive(Debug)]
pub enum TACInstruction {
    Return(Val),
    Unary(TACUnary, Val, Val),
}

#[derive(Debug)]
pub enum TACUnary {
    BitwiseComp,
    Negate,
}

#[derive(Clone, Debug)]
pub enum Val {
    Constant(i32),
    Var(Identifier),
}

#[derive(Debug)]
pub enum TACError {}
impl Display for TACError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TAC Error")
    }
}

static TMP_VAR: AtomicUsize = AtomicUsize::new(0);
fn make_temp() -> String {
    let current = TMP_VAR.fetch_add(1, Ordering::SeqCst);
    format!("tmp.{}", current)
}

pub fn do_tac(ast: Program) -> Result<TACProgram, TACError> {
    let p = TACProgram(tac_function(ast.0)?);
    dbg!(&p);
    Ok(p)
}

fn tac_function(function: Function) -> Result<TACFunction, TACError> {
    let statements = vec![function.1];
    let mut instructions: Vec<TACInstruction> = Vec::new();

    for s in statements {
        tac_instruction(s, &mut instructions)?;
    }
    Ok(TACFunction(function.0, instructions))
}

fn tac_instruction(
    statement: Statement,
    instructions: &mut Vec<TACInstruction>,
) -> Result<(), TACError> {
    match statement {
        Statement::Return(expr) => {
            let v = tac_expr(expr, instructions)?;
            instructions.push(TACInstruction::Return(v));
            Ok(())
        }
    }
}

fn tac_expr(expr: Expr, instructions: &mut Vec<TACInstruction>) -> Result<Val, TACError> {
    match expr {
        Expr::IntConstant(c) => Ok(Val::Constant(c)),
        Expr::Unary(op, inner) => {
            let src = tac_expr(*inner, instructions)?;
            let dst_name = make_temp();
            let dst = Val::Var(Identifier(dst_name));
            let op = tac_op(op);
            instructions.push(TACInstruction::Unary(op, src, dst.clone()));
            Ok(dst)
        }
    }
}

fn tac_op(op: UnaryOperation) -> TACUnary {
    match op {
        UnaryOperation::BitwiseComp => TACUnary::BitwiseComp,
        UnaryOperation::Negate => TACUnary::Negate,
    }
}
