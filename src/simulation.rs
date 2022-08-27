use clap::{Parser, Subcommand};
use std::collections::HashMap;
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
    input_bytes: usize,
    output_words: usize,
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
            input_bytes: input_bytes as usize,
            output_words: output_words as usize,
        })
    }

    pub fn instantiate(self) -> Instance {
        Instance::new(self)
    }
}

#[derive(Debug)]
pub struct Values(pub HashMap<String, u32>);

impl Values {
    fn to_bytes(&self, input_bytes: usize, mapping: &[(String, Type, Range)]) -> Vec<u8> {
        let mut input_bytes = vec![0u8; input_bytes];
        self.0.iter().for_each(|(k, v)| {
            let (_, _, r) = mapping
                .iter()
                .filter(|(name, _, _)| name == k)
                .next()
                .expect("Couldn't find input");
            let mask = (1 << (r.high - r.low + 1)) - 1;
            if v & !mask != 0 {
                panic!("Received step input that was too large for the mask!");
            }
            let v = v & mask;

            let lo_byte = r.low / 8;
            let hi_byte = r.high / 8;

            input_bytes[lo_byte as usize] |= (v << (r.low % 8)) as u8;
            for b in lo_byte + 1..=hi_byte {
                input_bytes[b as usize] |= ((v >> (8 * (b - lo_byte) - r.low % 8)) & 0xff) as u8;
            }
        });
        input_bytes
    }

    fn from_words(input: &[u32], mapping: &[(String, Type, Range)]) -> Self {
        let mut result = HashMap::new();
        mapping.iter().for_each(|(name, _, range)| {
            let lo_word = range.low / 32;
            let hi_word = range.high / 32;

            if hi_word - lo_word > 1 {
                // This is the case where a single field spans multiple words
                // which is currently impossible, since Value uses u32 to
                // represent values.
                todo!();
            }

            let mask = (1 << (range.high - range.low + 1)) - 1;
            let mut v = (input[lo_word as usize] >> (range.low % 32)) & mask;
            if lo_word != hi_word {
                v |= input[hi_word as usize] << (32 - range.low % 32);
            }

            result.insert(name.to_string(), v);
        });

        Self(result)
    }
}

pub struct Instance {
    module: Module,
    proc: Popen,
}

impl Instance {
    fn new(module: Module) -> Self {
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

    pub fn reset_step(&mut self, input: Values) -> Values {
        self.send_command(109);

        let input = input.to_bytes(self.module.input_bytes, &self.module.inputs[..]);
        self.stdin().write_all(&input[..]);
        self.stdin().flush();

        // Reset
        self.send_command(106);
        self.send_command(104);
        let o = self.read_outputs();
        self.send_command(108);
        o
    }

    pub fn step(&mut self, input: Values) -> Values {
        self.send_command(109);

        let input = input.to_bytes(self.module.input_bytes, &self.module.inputs[..]);
        self.stdin().write_all(&input[..]);
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

    fn read_outputs(&self) -> Values {
        let mut v = vec![0u32; self.module.output_words];
        for i in 0..self.module.output_words {
            let mut word_value = [0u8; 4];
            self.stdout().read_exact(&mut word_value).unwrap();
            v[i] = u32::from_le_bytes(word_value);
        }
        Values::from_words(&v, &self.module.outputs)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_values_to_bytes() {
        let mapping = [
            ("data_in".to_string(), Type::Bit, Range { low: 0, high: 4 }),
            ("save".to_string(), Type::Bit, Range { low: 5, high: 5 }),
        ];

        let res = Values(HashMap::from([
            ("data_in".to_string(), 0),
            ("save".to_string(), 1),
        ]))
        .to_bytes(1, &mapping[..]);
        assert_eq!(res, &[32]);

        let res = Values(HashMap::from([
            ("data_in".to_string(), 10),
            ("save".to_string(), 0),
        ]))
        .to_bytes(1, &mapping[..]);
        assert_eq!(res, &[10]);

        let res = Values(HashMap::from([
            ("data_in".to_string(), 1),
            ("save".to_string(), 1),
        ]))
        .to_bytes(1, &mapping[..]);
        assert_eq!(res, &[33]);
    }

    #[test]
    fn test_values_to_bytes_bridging_bytes() {
        let mapping = [
            ("a".to_string(), Type::Bit, Range { low: 0, high: 4 }),
            ("b".to_string(), Type::Bit, Range { low: 5, high: 13 }),
            ("c".to_string(), Type::Bit, Range { low: 14, high: 20 }),
        ];

        let res = Values(HashMap::from([
            ("a".to_string(), 10),
            ("b".to_string(), 123),
            ("c".to_string(), 19),
        ]))
        .to_bytes(3, &mapping[..]);
        assert_eq!(res, &[0x6a, 0xcf, 0x4]);
    }

    #[test]
    fn test_values_to_bytes_multibyte() {
        let mapping = [
            ("a".to_string(), Type::Bit, Range { low: 0, high: 9 }),
            ("b".to_string(), Type::Bit, Range { low: 10, high: 13 }),
            ("c".to_string(), Type::Bit, Range { low: 14, high: 35 }),
        ];

        let res = Values(HashMap::from([
            ("a".to_string(), 300),
            ("b".to_string(), 5),
            ("c".to_string(), 1_000_000),
        ]))
        .to_bytes(5, &mapping[..]);
        assert_eq!(res, &[0x2c, 0x15, 0x90, 0xd0, 0x3]);
    }

    #[test]
    fn test_values_from_words() {
        let mapping = [
            ("data_in".to_string(), Type::Bit, Range { low: 0, high: 4 }),
            ("save".to_string(), Type::Bit, Range { low: 5, high: 5 }),
        ];

        let values = Values::from_words(&[32], &mapping);
        assert_eq!(*values.0.get("data_in").unwrap(), 0);
        assert_eq!(*values.0.get("save").unwrap(), 1);
    }

    #[test]
    fn test_values_from_words_word_boundary() {
        let mapping = [
            ("a".to_string(), Type::Bit, Range { low: 0, high: 28 }),
            ("b".to_string(), Type::Bit, Range { low: 29, high: 37 }),
        ];

        let values = Values::from_words(&[0x600f_4240, 0x0f], &mapping);
        assert_eq!(*values.0.get("a").unwrap(), 1_000_000);
        assert_eq!(*values.0.get("b").unwrap(), 123);
    }
}
