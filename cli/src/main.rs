extern crate pest;
#[macro_use]
extern crate pest_derive;

use clap::{Parser, Subcommand};
use std::collections::HashMap;
use std::io::{Read, Write};

mod ast;
mod compile;
mod error;
mod ir;
mod parse;
mod simulation;
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

    /// Simulates a Ripstop program
    Simulate {
        #[clap(value_parser)]
        input: std::path::PathBuf,

        #[clap(value_parser)]
        top: String,
    },
}

fn compile(input: std::path::PathBuf, output: Option<std::path::PathBuf>) -> i32 {
    let inputpath = input.clone();
    println!("Compiling {:?}", input);

    let input = std::fs::read_to_string(&inputpath).unwrap();

    let mut a = match parse(&input) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("{:?}", e);
            return -1;
        }
    };
    println!("{}\n", a);

    let result = compile_document(&mut a);
    //let result = compile_module(&mut a);
    if result.errors.len() > 0 {
        for error in result.errors.iter() {
            eprintln!("{:?}", error);
        }
        return -1;
    }
    let (_, v_a) = result.result.unwrap();

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
        Commands::Simulate { input, top } => {
            let m = crate::simulation::Module::new(input, &top).unwrap();
            let mut instance = m.instantiate();

            instance.reset_step(crate::simulation::Values(HashMap::from([
                ("data_in".to_string(), 0),
                ("save".to_string(), 0),
            ])));
            instance.reset_step(crate::simulation::Values(HashMap::from([
                ("data_in".to_string(), 0),
                ("save".to_string(), 0),
            ])));

            for i in 0..10 {
                println!(
                    "{:?}",
                    instance.step(crate::simulation::Values(HashMap::from([
                        ("data_in".to_string(), 0),
                        ("save".to_string(), 0),
                    ])))
                );
            }

            // Bit 0 is data_in, bit 1 is save
            println!(
                "{:?}",
                instance.step(crate::simulation::Values(HashMap::from([
                    ("data_in".to_string(), 1),
                    ("save".to_string(), 1),
                ])))
            ); // save 1
            println!(
                "Should be 1: {:?}",
                instance.step(crate::simulation::Values(HashMap::from([
                    ("data_in".to_string(), 0),
                    ("save".to_string(), 0),
                ])))
            ); // Should still be 1
            println!(
                "Should be 1: {:?}",
                instance.step(crate::simulation::Values(HashMap::from([
                    ("data_in".to_string(), 1),
                    ("save".to_string(), 0),
                ])))
            ); // Should still be 1
            println!(
                "Should be 0: {:?}",
                instance.step(crate::simulation::Values(HashMap::from([
                    ("data_in".to_string(), 0),
                    ("save".to_string(), 1),
                ])))
            ); // save 0

            instance.finish();
        }
    }
}
