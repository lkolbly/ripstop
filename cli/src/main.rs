use clap::{Parser, Subcommand};
use std::collections::HashMap;
use std::io::Write;

use ripstop::compile::*;
use ripstop::parse::parse;
use ripstop::test::parse_tests_from_comment;
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

        #[clap(long)]
        test: bool,
    },

    /// Simulates a Ripstop program
    Simulate {
        #[clap(value_parser)]
        input: std::path::PathBuf,

        #[clap(value_parser)]
        top: String,
    },

    /// Runs all of the tests
    Test {
        #[clap(value_parser)]
        input: std::path::PathBuf,
    },

    /// Enumerates the external modules referenced (directly or indirectly) by the given top module
    EnumerateExtern {
        #[clap(value_parser)]
        input: std::path::PathBuf,

        #[clap(value_parser)]
        top: String,
    },
}

fn compile(input: String, output: Option<std::path::PathBuf>) -> i32 {
    let mut a = match parse(&input) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("{:?}\n", e);
            return -1;
        }
    };

    let result = compile_document(&mut a);
    if result.errors.len() > 0 {
        for error in result.errors.iter() {
            eprintln!("{:?}\n", error);
        }
        return -1;
    }
    let (_, _, v_a) = result.result.unwrap();

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

    env_logger::init();

    match args.command {
        Commands::Build {
            input,
            output,
            test,
        } => {
            let mut input = std::fs::read_to_string(&input).unwrap();
            if test {
                // This is a test suite .test file, so just extract the "### CODE" part
                let lines: Vec<_> = input.split("\n").collect();
                let mut result = vec![];
                let mut found_code = false;
                let mut is_actually_test = false;
                for line in lines.iter() {
                    if line.starts_with("###") {
                        if line.contains("### CODE") {
                            found_code = true;
                            is_actually_test = true;
                            continue;
                        } else {
                            found_code = false;
                        }
                    }

                    if found_code {
                        result.push(line.to_owned());
                    }
                }
                if is_actually_test {
                    input = result.join("\n");
                } else {
                    // If we never found the "### CODE", continue like normal
                    input = lines.join("\n");
                }
            }
            std::process::exit(compile(input, output));
        }
        Commands::Test { input } => {
            let input_contents = match std::fs::read_to_string(&input) {
                Ok(x) => x,
                Err(e) => {
                    eprintln!("Error reading file '{}'", input.display());
                    eprintln!("{:?}", e);
                    std::process::exit(-1);
                }
            };

            let mut ast = match parse(&input_contents) {
                Ok(x) => x,
                Err(e) => {
                    eprintln!("{:?}", e);
                    std::process::exit(-1);
                }
            };

            let result = compile_document(&mut ast);
            if result.errors.len() > 0 {
                for error in result.errors.iter() {
                    eprintln!("{:?}", error);
                }
                std::process::exit(-1);
            }
            let (_, modules, _) = result.result.unwrap();

            // Find all the tests in all the modules
            let tests: Vec<_> = modules
                .iter()
                .flat_map(|module| {
                    parse_tests_from_comment(&module, &module.doc_comment)
                        .into_iter()
                        .map(move |test| (module, test))
                })
                .collect();

            println!(
                "Collected {} tests from {} modules",
                tests.len(),
                modules.len()
            );

            for (i, (module, test)) in tests.iter().enumerate() {
                let sim_module = ripstop::simulation::Module::new::<&'static str>(
                    input.clone(),
                    &module.name,
                    None,
                    HashMap::new(),
                )
                .expect("Couldn't compile simulation module");
                let mut instance = sim_module.instantiate();

                print!("{}/{}", module.name, i);

                let mut pass = true;
                for step in test.steps.iter() {
                    print!(".");
                    let outputs = if step.reset {
                        instance.reset_step(step.inputs.clone()).unwrap()
                    } else {
                        instance.step(step.inputs.clone()).unwrap()
                    };
                    for (k, v) in step.expected_outputs.0.iter() {
                        let actual_value = outputs.0.get(k).unwrap();
                        if v != actual_value {
                            println!(
                                "Failed on step {}, expected {} for {} but got {}",
                                i, v, k, actual_value
                            );
                            pass = false;
                        }
                    }
                }
                if pass {
                    println!("PASS");
                } else {
                    println!("FAIL");
                }
                instance.finish().unwrap();
            }
        }
        Commands::Simulate { input, top } => {
            let m =
                ripstop::simulation::Module::new::<&'static str>(input, &top, None, HashMap::new())
                    .unwrap();
            let mut instance = m.instantiate();

            instance
                .reset_step(ripstop::simulation::Values(HashMap::from([
                    ("data_in".to_string(), 0),
                    ("save".to_string(), 0),
                ])))
                .unwrap();
            instance
                .reset_step(ripstop::simulation::Values(HashMap::from([
                    ("data_in".to_string(), 0),
                    ("save".to_string(), 0),
                ])))
                .unwrap();

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

            instance.finish().unwrap();
        }
        Commands::EnumerateExtern { input, top } => {
            let inputpath = input.clone();

            let input = std::fs::read_to_string(&inputpath).unwrap();

            let mut a = match parse(&input) {
                Ok(x) => x,
                Err(e) => {
                    eprintln!("{:?}\n", e);
                    std::process::exit(-1);
                }
            };

            let result = compile_document(&mut a);
            if result.errors.len() > 0 {
                for error in result.errors.iter() {
                    eprintln!("{:?}\n", error);
                }
                std::process::exit(-1);
            }
            let (declarations, modules, _) = result.result.unwrap();

            let extern_modules = match collect_external_modules(&top, &declarations, &modules) {
                Ok(x) => x,
                Err(_) => {
                    eprintln!("Could not find top module {}", top);
                    std::process::exit(-1);
                }
            };
            println!("Found {} external modules", extern_modules.len());
            for m in extern_modules.iter() {
                println!("{} is {}", m.instance_path.join("."), m.module_name);
            }
        }
    }
}
