use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
};

use super::codegen::{ABinary, AFunction, AProgram, AUnary, Instruction, Operand, Reg};

pub fn write_assembly(ast: AProgram, path: &PathBuf) -> io::Result<()> {
    let mut file = File::create(path)?;
    write_program(ast, &mut file)?;

    Ok(())
}

fn write_program(program: AProgram, file: &mut File) -> io::Result<()> {
    Ok(write_function(program.0, file)?)
}

fn write_function(function: AFunction, file: &mut File) -> io::Result<()> {
    writeln!(file, "\t.globl _{}", function.0 .0)?;
    writeln!(file, "_{}:", function.0 .0)?;
    writeln!(file, "\tpushq %rbp")?;
    writeln!(file, "\tmovq %rsp, %rbp")?;
    for instruction in function.1 {
        write_instruction(instruction, file)?;
    }
    Ok(())
}

fn write_instruction(instruction: Instruction, file: &mut File) -> io::Result<()> {
    match instruction {
        Instruction::Mov(lhs, rhs) => {
            let lhs = write_operand(lhs);
            let rhs = write_operand(rhs);
            writeln!(file, "\tmovl {}, {}", lhs, rhs)
        }
        Instruction::Ret => {
            writeln!(file, "\tmovq %rbp, %rsp")?;
            writeln!(file, "\tpopq %rbp")?;
            writeln!(file, "\tret")
        }
        Instruction::Unary(op, operand) => {
            let operator = write_unary(op);
            let operand = write_operand(operand);
            writeln!(file, "\t{} {}", operator, operand)
        }
        Instruction::Binary(op, src, dst) => {
            let operator = write_binary(op);
            let src = write_operand(src);
            let dst = write_operand(dst);
            writeln!(file, "\t{} {}, {}", operator, src, dst)
        }
        Instruction::IDiv(src) => {
            let src = write_operand(src);
            writeln!(file, "\tidivl {}", src)
        }
        Instruction::Cdq => {
            writeln!(file, "\tcdq")
        }
        Instruction::AllocateStack(size) => {
            writeln!(file, "\tsubq ${}, %rsp", size)
        }
    }
}

fn write_binary(op: ABinary) -> String {
    match op {
        ABinary::Add => "addl".into(),
        ABinary::Sub => "subl".into(),
        ABinary::Mult => "imull".into(),
    }
}

fn write_unary(op: AUnary) -> String {
    match op {
        AUnary::Neg => "negl".into(),
        AUnary::Not => "notl".into(),
    }
}

fn write_operand(op: Operand) -> String {
    match op {
        Operand::Imm(c) => format!("${}", c),
        Operand::Reg(Reg::AX) => "%eax".into(),
        Operand::Reg(Reg::DX) => "%edx".into(),
        Operand::Reg(Reg::R10) => "%r10d".into(),
        Operand::Reg(Reg::R11) => "%r11d".into(),
        Operand::Stack(offset) => format!("{}(%rbp)", offset),
        _ => panic!("unsupported operand"),
    }
}
