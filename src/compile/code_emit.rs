use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
};

use super::codegen::{AFunction, AProgram, Instruction, Operand};

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
            writeln!(file, "\tret")
        }
    }
}

fn write_operand(op: Operand) -> String {
    match op {
        Operand::Imm(c) => format!("${}", c),
        Operand::Register => "%eax".into(),
    }
}
