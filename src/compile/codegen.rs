use std::{collections::HashMap, fmt::Display};

use super::{
    parser::Identifier,
    tac::{TACBinary, TACFunction, TACInstruction, TACProgram, TACUnary, Val},
};

#[derive(Debug)]
pub struct AProgram(pub AFunction);

#[derive(Debug)]
pub struct AFunction(pub Identifier, pub Vec<Instruction>, pub Option<i32>);

#[derive(Debug)]
pub enum Instruction {
    Mov(Operand, Operand),
    Unary(AUnary, Operand),
    Binary(ABinary, Operand, Operand),
    IDiv(Operand),
    Cdq,
    AllocateStack(i32),
    Ret,
}

#[derive(Clone, Copy, Debug)]
pub enum ABinary {
    Add,
    Mult,
    Sub,
}

#[derive(Clone, Copy, Debug)]
pub enum AUnary {
    Neg,
    Not,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Imm(i32),
    Reg(Reg),
    Pseudo(Identifier),
    Stack(i32),
}

#[derive(Clone, Copy, Debug)]
pub enum Reg {
    AX,
    DX,
    R10,
    R11,
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

pub fn do_codegen(ast: TACProgram) -> Result<AProgram, CodegenError> {
    let first = do_first_pass(ast)?;
    dbg!(&first);
    let second = do_second_pass(first);
    dbg!(&second);
    let finish = do_third_pass(second);
    dbg!(&finish);
    Ok(finish)
}

fn do_first_pass(ast: TACProgram) -> Result<AProgram, CodegenError> {
    let function = gen_function_first(ast.0)?;
    Ok(AProgram(function))
}

fn gen_function_first(func: TACFunction) -> Result<AFunction, CodegenError> {
    let mut a_instructions: Vec<Instruction> = Vec::new();

    for i in func.1 {
        let i_instructions = gen_instructions_first(i)?;
        a_instructions.extend(i_instructions);
    }

    Ok(AFunction(func.0, a_instructions, None))
}

fn gen_instructions_first(instruction: TACInstruction) -> Result<Vec<Instruction>, CodegenError> {
    match instruction {
        TACInstruction::Return(v) => {
            let val = gen_operand_first(v)?;
            Ok(vec![
                Instruction::Mov(val, Operand::Reg(Reg::AX)),
                Instruction::Ret,
            ])
        }
        TACInstruction::Unary(op, src, dst) => {
            let operator = gen_unary_operator(op);
            let src_v = gen_operand_first(src)?;
            let dst_v = gen_operand_first(dst)?;
            Ok(vec![
                Instruction::Mov(src_v, dst_v.clone()),
                Instruction::Unary(operator, dst_v),
            ])
        }
        TACInstruction::Binary(TACBinary::Divide, src1, src2, dst) => {
            let src1 = gen_operand_first(src1)?;
            let src2 = gen_operand_first(src2)?;
            let dst = gen_operand_first(dst)?;
            Ok(vec![
                Instruction::Mov(src1, Operand::Reg(Reg::AX)),
                Instruction::Cdq,
                Instruction::IDiv(src2),
                Instruction::Mov(Operand::Reg(Reg::AX), dst),
            ])
        }
        TACInstruction::Binary(TACBinary::Remainder, src1, src2, dst) => {
            let src1 = gen_operand_first(src1)?;
            let src2 = gen_operand_first(src2)?;
            let dst = gen_operand_first(dst)?;
            Ok(vec![
                Instruction::Mov(src1, Operand::Reg(Reg::AX)),
                Instruction::Cdq,
                Instruction::IDiv(src2),
                Instruction::Mov(Operand::Reg(Reg::DX), dst),
            ])
        }
        TACInstruction::Binary(op, src1, src2, dst) => {
            let operator = gen_binary_operator(op)?;
            let src1 = gen_operand_first(src1)?;
            let src2 = gen_operand_first(src2)?;
            let dst = gen_operand_first(dst)?;
            Ok(vec![
                Instruction::Mov(src1, dst.clone()),
                Instruction::Binary(operator, src2, dst),
            ])
        }
        TACInstruction::Copy(_, _) => todo!(),
        TACInstruction::Jump(_) => todo!(),
        TACInstruction::JumpIfZero(_, _) => todo!(),
        TACInstruction::JumpIfNotZero(_, _) => todo!(),
        TACInstruction::Label(_) => todo!(),
    }
}

fn gen_binary_operator(operator: TACBinary) -> Result<ABinary, CodegenError> {
    match operator {
        TACBinary::Add => Ok(ABinary::Add),
        TACBinary::Subtract => Ok(ABinary::Sub),
        TACBinary::Multiply => Ok(ABinary::Mult),
        TACBinary::Equal => todo!(),
        TACBinary::NotEqual => todo!(),
        TACBinary::LessThan => todo!(),
        TACBinary::LessOrEqual => todo!(),
        TACBinary::GreaterThan => todo!(),
        TACBinary::GreaterOrEqual => todo!(),
        _ => Err(CodegenError::Err("invalid binary operator".into())),
    }
}

fn gen_unary_operator(operator: TACUnary) -> AUnary {
    match operator {
        TACUnary::BitwiseComp => AUnary::Not,
        TACUnary::Negate => AUnary::Neg,
        TACUnary::Not => todo!(),
    }
}

fn gen_operand_first(val: Val) -> Result<Operand, CodegenError> {
    match val {
        Val::Constant(c) => Ok(Operand::Imm(c)),
        Val::Var(i) => Ok(Operand::Pseudo(i)),
    }
}

fn do_second_pass(mut program: AProgram) -> AProgram {
    program.0 = function_second_pass(program.0);
    program
}

fn function_second_pass(mut function: AFunction) -> AFunction {
    let mut stack = StackGen::new();

    function.1 = function
        .1
        .into_iter()
        .map(|instruction| match instruction {
            Instruction::Mov(mut src, mut dst) => {
                if let Operand::Pseudo(ident) = src {
                    src = stack.get_operand(&ident.0);
                }
                if let Operand::Pseudo(ident) = dst {
                    dst = stack.get_operand(&ident.0);
                }

                Instruction::Mov(src, dst)
            }
            Instruction::Unary(op, mut dst) => {
                if let Operand::Pseudo(ident) = dst {
                    dst = stack.get_operand(&ident.0);
                }

                Instruction::Unary(op, dst)
            }
            Instruction::IDiv(mut src) => {
                if let Operand::Pseudo(ident) = src {
                    src = stack.get_operand(&ident.0);
                }

                Instruction::IDiv(src)
            }
            Instruction::Binary(op, mut src, mut dst) => {
                if let Operand::Pseudo(ident) = src {
                    src = stack.get_operand(&ident.0);
                }
                if let Operand::Pseudo(ident) = dst {
                    dst = stack.get_operand(&ident.0);
                }

                Instruction::Binary(op, src, dst)
            }
            _ => instruction,
        })
        .collect();

    function.2 = Some(stack.offset);
    function
}

#[derive(Debug)]
struct StackGen {
    pub offset: i32,
    stack_map: HashMap<String, i32>,
}
impl StackGen {
    fn new() -> Self {
        Self {
            offset: -4,
            stack_map: HashMap::new(),
        }
    }

    fn get_operand(&mut self, ident: &str) -> Operand {
        let address = self.stack_map.entry(ident.to_string()).or_insert_with(|| {
            let tmp = self.offset.clone();
            self.offset -= 4;
            tmp
        });
        Operand::Stack(*address)
    }
}

fn do_third_pass(mut program: AProgram) -> AProgram {
    program.0 = function_third_pass(program.0);
    program
}

fn function_third_pass(mut function: AFunction) -> AFunction {
    let prologue = vec![Instruction::AllocateStack(
        function.2.expect("missing offset").abs(),
    )];
    let instructions = function
        .1
        .into_iter()
        .flat_map(|instruction| match instruction {
            Instruction::Mov(Operand::Stack(src), Operand::Stack(dst)) => {
                vec![
                    Instruction::Mov(Operand::Stack(src), Operand::Reg(Reg::R10)),
                    Instruction::Mov(Operand::Reg(Reg::R10), Operand::Stack(dst)),
                ]
            }
            Instruction::IDiv(Operand::Imm(c)) => {
                vec![
                    Instruction::Mov(Operand::Imm(c), Operand::Reg(Reg::R10)),
                    Instruction::IDiv(Operand::Reg(Reg::R10)),
                ]
            }
            Instruction::Binary(ABinary::Mult, src, Operand::Stack(dst)) => {
                vec![
                    Instruction::Mov(Operand::Stack(dst), Operand::Reg(Reg::R11)),
                    Instruction::Binary(ABinary::Mult, src, Operand::Reg(Reg::R11)),
                    Instruction::Mov(Operand::Reg(Reg::R11), Operand::Stack(dst)),
                ]
            }
            Instruction::Binary(op, Operand::Stack(src), Operand::Stack(dst)) => {
                vec![
                    Instruction::Mov(Operand::Stack(src), Operand::Reg(Reg::R10)),
                    Instruction::Binary(op, Operand::Reg(Reg::R10), Operand::Stack(dst)),
                ]
            }
            _ => vec![instruction],
        })
        .collect::<Vec<Instruction>>();

    function.1 = prologue;
    function.1.extend(instructions);
    function
}

// fn gen_stack() -> Box<dyn Fn(&str) -> Operand> {
//     let mut stack_offset = -4;
//     let mut stack_map: HashMap<String, i32> = HashMap::new();

//     Box::new(|ident| {
//         let address_reg_o = stack_map.get(ident);
//         match address_reg_o {
//             Some(reg) => Operand::Stack(*reg),
//             None => {
//                 stack_offset -= 4;
//                 stack_map.insert(ident.to_string(), stack_offset);
//                 Operand::Stack(stack_offset)
//             }
//         }
//     })
// }

// fn gen_instructions(statement: Statement) -> Result<Vec<Instruction>, CodegenError> {
//     match statement {
//         Statement::Return(expr) => {
//             let val = gen_expression(expr)?;
//             Ok(vec![
//                 Instruction::Mov(val, Operand::Register),
//                 Instruction::Ret,
//             ])
//         }
//     }
// }

// fn gen_expression(expr: Expr) -> Result<Operand, CodegenError> {
//     match expr {
//         Expr::IntConstant(c) => Ok(Operand::Imm(c)),
//         _ => unimplemented!(),
//     }
// }
