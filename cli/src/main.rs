use clap::{Parser, Subcommand};
use std::collections::HashMap;
use std::io::{Read, Write};

use ripstop::compile::*;
use ripstop::parse::parse;
use ripstop::verilog_ast::verilog_ast_to_string;

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

    let input = std::fs::read_to_string(&inputpath).unwrap();

    let mut a = match parse(&input) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("{:?}", e);
            return -1;
        }
    };

    let result = compile_document(&mut a);
    //let result = compile_module(&mut a);
    if result.errors.len() > 0 {
        for error in result.errors.iter() {
            eprintln!("{:?}", error);
        }
        return -1;
    }
    let (_, v_a) = result.result.unwrap();

    let compiled = verilog_ast_to_string(v_a.find_head().unwrap(), &v_a, 0);

    if let Some(output) = output {
        let mut f = std::fs::File::create(output).unwrap();
        f.write_all(compiled.as_bytes()).unwrap();
        f.write_all("\n".as_bytes()).unwrap();
    } else {
        // Send it to stdout
        let mut stdout = std::io::stdout();
        stdout.write_all(compiled.as_bytes()).unwrap();
        stdout.write_all("\n".as_bytes()).unwrap();
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
            let m = ripstop::simulation::Module::new(input, &top).unwrap();
            let mut instance = m.instantiate();

            instance.reset_step(ripstop::simulation::Values(HashMap::from([
                ("data_in".to_string(), 0),
                ("save".to_string(), 0),
            ])));
            instance.reset_step(ripstop::simulation::Values(HashMap::from([
                ("data_in".to_string(), 0),
                ("save".to_string(), 0),
            ])));

            for _ in 0..10 {
                println!(
                    "{:?}",
                    instance.step(ripstop::simulation::Values(HashMap::from([
                        ("data_in".to_string(), 0),
                        ("save".to_string(), 0),
                    ])))
                );
            }

            // Bit 0 is data_in, bit 1 is save
            println!(
                "{:?}",
                instance.step(ripstop::simulation::Values(HashMap::from([
                    ("data_in".to_string(), 1),
                    ("save".to_string(), 1),
                ])))
            ); // save 1
            println!(
                "Should be 1: {:?}",
                instance.step(ripstop::simulation::Values(HashMap::from([
                    ("data_in".to_string(), 0),
                    ("save".to_string(), 0),
                ])))
            ); // Should still be 1
            println!(
                "Should be 1: {:?}",
                instance.step(ripstop::simulation::Values(HashMap::from([
                    ("data_in".to_string(), 1),
                    ("save".to_string(), 0),
                ])))
            ); // Should still be 1
            println!(
                "Should be 0: {:?}",
                instance.step(ripstop::simulation::Values(HashMap::from([
                    ("data_in".to_string(), 0),
                    ("save".to_string(), 1),
                ])))
            ); // save 0

            instance.finish();
        }
    }
}
