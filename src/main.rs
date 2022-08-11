extern crate pest;
#[macro_use]
extern crate pest_derive;

use clap::{Parser, Subcommand};
use std::io::{Read, Write};

mod ast;
mod compile;
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

    let v_a = match compile_module(&mut a) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("{:?}", e);
            return -1;
        }
    };

    println!("Bare tree:\n\n{}\n\n", v_a);

    println!(
        "Verilog Compiled:\n\n{}",
        verilog_ast_to_string(v_a.find_head().unwrap(), &v_a, 0)
    );

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
            println!("Couldn't find mock output, not pretending to compile.");
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
    return;

    println!("{:?}", args);

    let mut a = parse(
        "module hello() -> (bit led) {
            led[t] = ~led[t-2]; // Perform the bitwise NOT
        }",
    );
    println!("{:#?}\n", a);

    let v_a = compile_module(&mut a).unwrap();

    println!("Bare tree:\n\n{:#?}\n\n", v_a);

    /*println!(
        "Verilog Compiled:\n\n{}",
        verilog_ast_to_string(v_a.find_head().unwrap(), &v_a, 0)
    );*/

    //println!("{:#?}\n", a);

    //verify_ast(&mut a).unwrap();

    //println!("Post-verify:\n\n{}\n\n", compile_module(&a));
    //println!("{:#?}\n", a);

    /*let b = parse(
        "module add(bit b, bit c) -> (bit a) {
        a[t] = a[t-1] + b[t] + c[t-1];
    }",
    );
    println!("{:#?}", b);

    println!("{:#?}", b);
    println!("{}", compile_module(&b));*/
}
