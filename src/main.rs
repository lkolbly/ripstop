extern crate pest;
#[macro_use]
extern crate pest_derive;

use clap::{Parser, Subcommand};
use std::io::{Read, Write};

mod ast;
mod compile;
mod error;
mod ir;
mod parse;
mod tree;
mod verilog_ast;

use ast::*;
use compile::*;
use parse::parse;

use crate::verilog_ast::verilog_ast_to_string;

#[derive(Parser, Debug)]
#[clap(version, about)]
struct Args {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Compiles a single Ripstop file into a single verilog file
    Build {
        #[clap(value_parser)]
        input: std::path::PathBuf,

        #[clap(short, long, value_parser)]
        output: Option<std::path::PathBuf>,
    },
}

fn compile(input: std::path::PathBuf, output: Option<std::path::PathBuf>) -> i32 {
    let inputpath = input.clone();
    println!("Compiling {:?}", input);

    let input = std::fs::read_to_string(&inputpath).unwrap();

    let mut a = parse(&input);
    println!("{}\n", a);

    let result = compile_module(&mut a);
    if result.errors.len() > 0 {
        for error in result.errors.iter() {
            eprintln!("{:?}", error);
        }
        return -1;
    }
    let v_a = result.result.unwrap();

    println!("Bare tree:\n\n{}\n\n", v_a);

    let compiled = verilog_ast_to_string(v_a.find_head().unwrap(), &v_a, 0);
    println!("Verilog Compiled:\n\n{}", compiled);

    if let Some(output) = output {
        // Do a fake compilation
        // See if we can find a ".compiled.v" file
        let mut mock_output_path = inputpath.clone();
        mock_output_path.set_extension("compiled.v");
        if mock_output_path.is_file() {
            let mut f = std::fs::File::create(output).unwrap();
            let mut source = std::fs::File::open(mock_output_path).unwrap();
            let mut mock = vec![];
            source.read_to_end(&mut mock).unwrap();
            f.write_all(&mock).unwrap();
        } else {
            let mut f = std::fs::File::create(output).unwrap();
            f.write_all(compiled.as_bytes()).unwrap();
            f.write_all("\n".as_bytes()).unwrap();
        }
    }

    0
}

fn main() {
    let args = Args::parse();

    match args.command {
        Commands::Build { input, output } => {
            std::process::exit(compile(input, output));
        }
    }
}
