use std::{fs, process::Command};

use clap::Parser;
use sdcc::{cli::Cli, compile::do_compile};

const PREPROCESS_FILE: &str = "pp.i";

fn main() {
    let cli = Cli::parse();

    let _preprocess_output = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(&cli.name)
        .arg("-o")
        .arg(PREPROCESS_FILE)
        .output()
        .expect("failed to preprocess");

    let assembly_file = cli.name.to_str().expect("invalid path").replace(".c", ".s");
    let output_file = cli.name.to_str().expect("invalid path").replace(".c", "");

    let r = do_compile(&PREPROCESS_FILE.into(), &assembly_file.clone().into(), &cli);
    if let Err(e) = r {
        panic!("{}", e);
    }

    let assemble_output = Command::new("gcc")
        .arg(&assembly_file)
        .arg("-o")
        .arg(&output_file)
        .output()
        .expect("failed to assemble");

    if !assemble_output.status.success() {
        panic!("{}", String::from_utf8(assemble_output.stderr).unwrap());
    }

    fs::remove_file(&PREPROCESS_FILE).expect("failed to delete preprocessed file");
    // fs::remove_file(&assembly_file).expect("failed to delete assembly file");
}
