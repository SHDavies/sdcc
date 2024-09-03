use std::{
    fmt::Display,
    sync::atomic::{AtomicUsize, Ordering},
};

use super::parser::{
    BinaryOperator, Expr, Function, Identifier, Program, Statement, UnaryOperator,
};

#[derive(Debug)]
pub struct TACProgram(pub TACFunction);

#[derive(Debug)]
pub struct TACFunction(pub Identifier, pub Vec<TACInstruction>);

#[derive(Debug)]
pub enum TACInstruction {
    Return(Val),
    // Operator, src, dst
    Unary(TACUnary, Val, Val),
    // Operator, src1, src2, dst
    Binary(TACBinary, Val, Val, Val),
    // src, dst
    Copy(Val, Val),
    Jump(Identifier),
    JumpIfZero(Val, Identifier),
    JumpIfNotZero(Val, Identifier),
    Label(Identifier),
}

#[derive(Debug)]
pub enum TACBinary {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    And,
    Or,
}

#[derive(Debug)]
pub enum TACUnary {
    BitwiseComp,
    Negate,
    Not,
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

static LABEL_VAR: AtomicUsize = AtomicUsize::new(0);
fn make_label(name: &str) -> Identifier {
    let current = LABEL_VAR.fetch_add(1, Ordering::SeqCst);
    let label = format!("{}.{}", name, current);
    Identifier(label)
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
            let op = tac_unary_op(op);
            instructions.push(TACInstruction::Unary(op, src, dst.clone()));
            Ok(dst)
        }
        Expr::Binary(BinaryOperator::And, lhs, rhs) => {
            let false_label = make_label("and_false");
            let end_label = make_label("and_end");

            let lhs_val = tac_expr(*lhs, instructions)?;
            let lhs_name = make_temp();
            let lhs_dst = Val::Var(Identifier(lhs_name));
            instructions.push(TACInstruction::Copy(lhs_val, lhs_dst.clone()));
            instructions.push(TACInstruction::JumpIfZero(lhs_dst, false_label.clone()));

            let rhs_val = tac_expr(*rhs, instructions)?;
            let rhs_name = make_temp();
            let rhs_dst = Val::Var(Identifier(rhs_name));
            instructions.push(TACInstruction::Copy(rhs_val, rhs_dst.clone()));
            instructions.push(TACInstruction::JumpIfZero(rhs_dst, false_label.clone()));

            let result_name = make_temp();
            let result_dst = Val::Var(Identifier(result_name));
            instructions.push(TACInstruction::Copy(Val::Constant(1), result_dst.clone()));
            instructions.push(TACInstruction::Jump(end_label.clone()));

            instructions.push(TACInstruction::Label(false_label));
            instructions.push(TACInstruction::Copy(Val::Constant(0), result_dst.clone()));
            instructions.push(TACInstruction::Label(end_label));
            Ok(result_dst)
        }
        Expr::Binary(BinaryOperator::Or, lhs, rhs) => {
            let true_label = make_label("or_true");
            let end_label = make_label("or_end");

            let lhs_val = tac_expr(*lhs, instructions)?;
            let lhs_name = make_temp();
            let lhs_dst = Val::Var(Identifier(lhs_name));
            instructions.push(TACInstruction::Copy(lhs_val, lhs_dst.clone()));
            instructions.push(TACInstruction::JumpIfNotZero(lhs_dst, true_label.clone()));

            let rhs_val = tac_expr(*rhs, instructions)?;
            let rhs_name = make_temp();
            let rhs_dst = Val::Var(Identifier(rhs_name));
            instructions.push(TACInstruction::Copy(rhs_val, rhs_dst.clone()));
            instructions.push(TACInstruction::JumpIfNotZero(rhs_dst, true_label.clone()));

            let result_name = make_temp();
            let result_dst = Val::Var(Identifier(result_name));
            instructions.push(TACInstruction::Copy(Val::Constant(0), result_dst.clone()));
            instructions.push(TACInstruction::Jump(end_label.clone()));

            instructions.push(TACInstruction::Label(true_label));
            instructions.push(TACInstruction::Copy(Val::Constant(1), result_dst.clone()));
            instructions.push(TACInstruction::Label(end_label));
            Ok(result_dst)
        }
        Expr::Binary(operator, lhs, rhs) => {
            let lhs_val = tac_expr(*lhs, instructions)?;
            let rhs_val = tac_expr(*rhs, instructions)?;
            let dst_name = make_temp();
            let dst = Val::Var(Identifier(dst_name));
            let op = tac_binary_op(operator);
            instructions.push(TACInstruction::Binary(op, lhs_val, rhs_val, dst.clone()));
            Ok(dst)
        }
    }
}

fn tac_unary_op(op: UnaryOperator) -> TACUnary {
    match op {
        UnaryOperator::BitwiseComp => TACUnary::BitwiseComp,
        UnaryOperator::Negate => TACUnary::Negate,
        UnaryOperator::Not => TACUnary::Not,
    }
}

fn tac_binary_op(op: BinaryOperator) -> TACBinary {
    match op {
        BinaryOperator::Add => TACBinary::Add,
        BinaryOperator::Subtract => TACBinary::Subtract,
        BinaryOperator::Multiply => TACBinary::Multiply,
        BinaryOperator::Divide => TACBinary::Divide,
        BinaryOperator::Remainder => TACBinary::Remainder,
        BinaryOperator::And => TACBinary::And,
        BinaryOperator::Or => TACBinary::Or,
        BinaryOperator::Equal => TACBinary::Equal,
        BinaryOperator::NotEqual => TACBinary::NotEqual,
        BinaryOperator::LessThan => TACBinary::LessThan,
        BinaryOperator::LessOrEqual => TACBinary::LessOrEqual,
        BinaryOperator::GreaterThan => TACBinary::GreaterThan,
        BinaryOperator::GreaterOrEqual => TACBinary::GreaterOrEqual,
    }
}
