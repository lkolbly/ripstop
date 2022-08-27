use clap::{Parser, Subcommand};
use std::fs::File;
use std::io::{Read, Write};
use subprocess::{Popen, PopenConfig, Redirection};

use crate::ast::*;
use crate::compile::*;
use crate::parse::{parse, Range};
use crate::verilog_ast::verilog_ast_to_string;

pub struct Module {
    module_name: String,
    inputs: Vec<(String, Type, Range)>,
    outputs: Vec<(String, Type, Range)>,
}

impl Module {
    pub fn new(input: std::path::PathBuf) -> Option<Self> {
        let inputpath = input.clone();
        println!("Compiling {:?}", input);

        let input = std::fs::read_to_string(&inputpath).unwrap();

        let mut a = match parse(&input) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("{:?}", e);
                return None;
            }
        };
        println!("{}\n", a);

        let result = compile_module(&mut a);
        if result.errors.len() > 0 {
            for error in result.errors.iter() {
                eprintln!("{:?}", error);
            }
            return None;
        }
        let (module, v_a) = result.result.unwrap();

        println!("Bare tree:\n\n{}\n\n", v_a);

        let compiled = verilog_ast_to_string(v_a.find_head().unwrap(), &v_a, 0);
        println!("Verilog Compiled:\n\n{}", compiled);

        let inputs: Vec<_> = module
            .inputs
            .iter()
            .map(|input| {
                let t = module.variables.get(input).unwrap();
                (input.to_string(), *t)
            })
            .scan(0, |state, (name, vartype)| {
                let low = *state;
                let high = *state + vartype.bit_size() - 1;
                *state = high + 1;
                Some((
                    name,
                    vartype,
                    Range {
                        low: low as u32,
                        high: high as u32,
                    },
                ))
            })
            .collect();
        let outputs: Vec<_> = module
            .outputs
            .iter()
            .map(|output| {
                let t = module.variables.get(output).unwrap();
                (output.to_string(), *t)
            })
            .scan(0, |state, (name, vartype)| {
                let low = *state;
                let high = *state + vartype.bit_size() - 1;
                *state = high + 1;
                Some((
                    name,
                    vartype,
                    Range {
                        low: low as u32,
                        high: high as u32,
                    },
                ))
            })
            .collect();

        let input_size = inputs[inputs.len() - 1].2.high + 1;
        let output_size = outputs[outputs.len() - 1].2.high + 1;

        let input_size = if input_size == 0 {
            32
        } else if input_size % 32 != 0 {
            input_size + 32 - input_size % 32
        } else {
            input_size
        };
        let output_size = if output_size == 0 {
            32
        } else if output_size % 32 != 0 {
            output_size + 32 - output_size % 32
        } else {
            output_size
        };

        println!("{inputs:?} {outputs:?}");
        println!("Input size: {input_size} output_size: {output_size}");

        // Build the template
        let tera = tera::Tera::new("templates/**/*.v").unwrap();

        let input_bytes = input_size / 8;
        let output_words = output_size / 32;

        let mut context = tera::Context::new();
        context.insert("compiled", &compiled);
        context.insert("module_name", &module.name);
        context.insert("inputs", &inputs);
        context.insert("outputs", &outputs);
        context.insert("input_size", &input_size);
        context.insert("output_size", &output_size);
        context.insert("input_bytes", &input_bytes);
        context.insert("output_words", &output_words);
        let harness = tera.render("simulation_harness.v", &context).unwrap();

        {
            let mut f = std::fs::File::create("sim.v").unwrap();
            f.write_all(harness.as_bytes()).unwrap();
        }

        // Compile
        let mut p = subprocess::Popen::create(
            &[
                "/home/lane/Downloads/iverilog/install/bin/iverilog",
                "sim.v",
            ],
            subprocess::PopenConfig {
                ..Default::default()
            },
        )
        .unwrap();

        p.communicate(None).unwrap();

        Some(Self {
            module_name: module.name,
            inputs,
            outputs,
        })
    }

    pub fn instantiate(&self) -> Instance {
        Instance::new(self)
    }
}

pub struct Instance<'a> {
    module: &'a Module,
    proc: Popen,
}

impl<'a> Instance<'a> {
    fn new(module: &'a Module) -> Self {
        let mut p = subprocess::Popen::create(
            &["./a.out"],
            subprocess::PopenConfig {
                stdin: subprocess::Redirection::Pipe,
                stdout: subprocess::Redirection::Pipe,
                ..Default::default()
            },
        )
        .unwrap();

        // iverilog prints out this exact string when we open the dumpfile
        // If we could make it not do that, that'd be great
        let hdr = "VCD info: dumpfile dump.vcd opened for output.\n";
        let mut v = vec![0; hdr.len()];
        p.stdout.as_ref().unwrap().read_exact(&mut v).unwrap();

        Self { module, proc: p }
    }

    pub fn reset_step(&mut self, input: u32) -> u32 {
        self.send_command(109);
        self.stdin().write_all(&input.to_be_bytes());
        self.stdin().flush();

        // Reset
        self.send_command(106);
        self.send_command(104);
        let o = self.read_outputs();
        self.send_command(108);
        o
    }

    pub fn step(&mut self, input: u32) -> u32 {
        self.send_command(109);
        self.stdin().write_all(&input.to_be_bytes());
        self.stdin().flush();

        // Clear reset
        self.send_command(107);
        self.send_command(104);
        let o = self.read_outputs();
        self.send_command(108);
        o
    }

    pub fn finish(mut self) {
        self.send_command(105);
        self.proc.terminate().unwrap();
    }

    fn stdin(&self) -> &File {
        self.proc.stdin.as_ref().unwrap()
    }

    fn stdout(&self) -> &File {
        self.proc.stdout.as_ref().unwrap()
    }

    fn send_command(&self, cmd: u8) {
        self.stdin().write_all(&[cmd]).unwrap();
        self.stdin().flush().unwrap();
    }

    fn read_outputs(&self) -> u32 {
        let mut v = [0; 4];
        self.stdout().read_exact(&mut v).unwrap();
        u32::from_le_bytes(v)
    }
}
