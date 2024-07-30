use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
pub struct Cli {
    pub name: PathBuf,
    #[arg(long)]
    pub lex: bool,
    #[arg(long)]
    pub parse: bool,
    #[arg(long)]
    pub codegen: bool,
    #[arg(long)]
    pub tac: bool,
}
