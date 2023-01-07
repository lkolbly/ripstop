use std::collections::HashMap;
use std::fs::File;
use std::io::{Read, Write};
use std::sync::Arc;
use subprocess::Popen;
use thiserror::Error;

use crate::ast::*;
use crate::compile::*;
use crate::error::CompileError;
use crate::parse::{parse, Range};
use crate::verilog_ast::verilog_ast_to_string;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Couldn't find iverilog (is RIPSTOP_IVERILOG_BIN set?)")]
    IverilogNotFound(#[from] subprocess::PopenError),

    #[error("Error running iverilog compilation")]
    IverilogCompileFailed(#[from] std::io::Error),

    #[error("Couldn't compile simulation harness (this is a bug!)")]
    SimulationCompileFailed(#[from] Box<dyn std::error::Error>),

    #[error("Ripstop compilation error")]
    RipstopError(Vec<CompileError>),

    #[error("Error creating temporary file")]
    TemporaryFileError(#[from] tempfile::PersistError),
}

#[derive(Debug, Error)]
pub enum StepError {
    #[error("Error running step")]
    StepError(#[from] std::io::Error),
}

impl std::convert::From<CompileError> for Error {
    fn from(e: CompileError) -> Self {
        Self::RipstopError(vec![e])
    }
}

type Result<T> = std::result::Result<T, Error>;
type StepResult<T> = std::result::Result<T, StepError>;

#[derive(Debug)]
struct ExecutableFile(std::path::PathBuf);

impl Drop for ExecutableFile {
    fn drop(&mut self) {
        match std::fs::remove_file(&self.0) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("Error removing simulation executable: {:?}", e);
            }
        }
    }
}

pub struct Module {
    inputs: Vec<(String, Type, Range)>,
    outputs: Vec<(String, Type, Range)>,
    input_bytes: usize,
    output_words: usize,
    executable_file: Arc<ExecutableFile>,
    dumpfile: Option<std::path::PathBuf>,
}

impl Module {
    pub fn new<P: AsRef<std::path::Path>>(
        input: std::path::PathBuf,
        top: &str,
        dumpfile: Option<P>,
    ) -> Result<Self> {
        let inputpath = input.clone();

        let input = std::fs::read_to_string(&inputpath)
            .map_err(|e| Error::SimulationCompileFailed(Box::new(e)))?;

        let mut a = parse(&input)?;

        let result = compile_document(&mut a);
        if result.errors.len() > 0 {
            return Err(Error::RipstopError(result.errors));
        }
        let (_, mut modules, v_a) = result.result.unwrap();

        let module = modules
            .drain(..)
            .filter(|m| m.name == top)
            .next()
            .expect("Couldn't find module");

        let compiled = verilog_ast_to_string(v_a.find_head().unwrap(), &v_a, 0);

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

        let input_size = if inputs.len() == 0 {
            0
        } else {
            inputs[inputs.len() - 1].2.high + 1
        };
        let output_size = if outputs.len() == 0 {
            0
        } else {
            outputs[outputs.len() - 1].2.high + 1
        };

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

        // Build the template
        let mut tera = tera::Tera::default();
        tera.add_raw_template(
            "simulation_harness.v",
            include_str!("../../templates/simulation_harness.v"),
        )
        .unwrap();

        let input_bytes = input_size / 8;
        let output_words = output_size / 32;
        let output_word_iterator: Vec<u32> = (0..output_words).collect();

        let mut context = tera::Context::new();
        context.insert("compiled", &compiled);
        context.insert("module_name", &module.name);
        context.insert("inputs", &inputs);
        context.insert("outputs", &outputs);
        context.insert("input_size", &input_size);
        context.insert("output_size", &output_size);
        context.insert("input_bytes", &input_bytes);
        context.insert("output_words", &output_words);
        context.insert("output_word_iterator", &output_word_iterator);

        if let Some(dumpfile) = &dumpfile {
            context.insert("output_dumpfile", &true);
            context.insert("dumpfile", dumpfile.as_ref());
        } else {
            context.insert("output_dumpfile", &false);
        }

        let harness = tera.render("simulation_harness.v", &context).unwrap();

        let mut sim_file = tempfile::NamedTempFile::new()?;

        sim_file.write_all(harness.as_bytes())?;
        sim_file.flush()?;

        let output_file = tempfile::NamedTempFile::new()?;
        let (_, output_file) = output_file.keep()?;

        // Compile
        let iverilog_path = std::env::var("RIPSTOP_IVERILOG_BIN").unwrap_or("iverilog".to_string());
        let sim_file_path = format!("{}", sim_file.path().display());
        let output_file_path = format!("{}", output_file.display());
        let mut p = subprocess::Popen::create(
            &[&iverilog_path, &sim_file_path, "-o", &output_file_path][..],
            subprocess::PopenConfig {
                ..Default::default()
            },
        )?;

        p.communicate(None)?;

        Ok(Self {
            inputs,
            outputs,
            input_bytes: input_bytes as usize,
            output_words: output_words as usize,
            executable_file: Arc::new(ExecutableFile(output_file)),
            dumpfile: dumpfile.map(|p| p.as_ref().to_path_buf()),
        })
    }

    pub fn instantiate(self) -> Instance {
        Instance::new(self)
    }
}

#[derive(Clone, Debug)]
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
            // TODO: Check that the mask never exceeds 32 bits
            // (found a bug with ddr_tester, which had a field going from 14 to 45)
            let mask = ((1u64 << (r.high as u64 - r.low as u64 + 1u64)) - 1u64) as u32;
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

            // TODO: Same thing here as above for the other shift
            let mask = ((1u64 << (range.high as u64 - range.low as u64 + 1u64)) - 1u64) as u32;
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
        let executable_path = format!("{}", module.executable_file.0.display());
        let p = subprocess::Popen::create(
            &[executable_path],
            subprocess::PopenConfig {
                stdin: subprocess::Redirection::Pipe,
                stdout: subprocess::Redirection::Pipe,
                ..Default::default()
            },
        )
        .unwrap();

        // iverilog prints out this exact string when we open the dumpfile
        // If we could make it not do that, that'd be great
        if module.dumpfile.is_some() {
            let hdr = "VCD info: dumpfile dump.vcd opened for output.\n";
            let mut v = vec![0; hdr.len()];
            p.stdout.as_ref().unwrap().read_exact(&mut v).unwrap();
        }

        Self { module, proc: p }
    }

    pub fn reset_step(&mut self, input: Values) -> StepResult<Values> {
        self.send_command(109)?;

        let input = input.to_bytes(self.module.input_bytes, &self.module.inputs[..]);
        self.stdin().write_all(&input[..])?;
        self.stdin().flush()?;

        // Reset
        self.send_command(106)?;
        self.send_command(104)?;
        let o = self.read_outputs()?;
        self.send_command(108)?;
        Ok(o)
    }

    pub fn step(&mut self, input: Values) -> StepResult<Values> {
        self.send_command(109)?;

        let input = input.to_bytes(self.module.input_bytes, &self.module.inputs[..]);
        self.stdin().write_all(&input[..])?;
        self.stdin().flush()?;

        // Clear reset
        self.send_command(107)?;
        self.send_command(104)?;
        let o = self.read_outputs()?;
        self.send_command(108)?;
        Ok(o)
    }

    pub fn finish(mut self) -> StepResult<()> {
        self.send_command(105)?;
        self.proc.terminate()?;
        Ok(())
    }

    fn stdin(&self) -> &File {
        self.proc.stdin.as_ref().unwrap()
    }

    fn stdout(&self) -> &File {
        self.proc.stdout.as_ref().unwrap()
    }

    fn send_command(&self, cmd: u8) -> StepResult<()> {
        self.stdin().write_all(&[cmd])?;
        self.stdin().flush()?;
        Ok(())
    }

    fn read_outputs(&self) -> StepResult<Values> {
        let mut v = vec![0u32; self.module.output_words];
        for i in 0..self.module.output_words {
            let mut word_value = [0u8; 4];
            self.stdout().read_exact(&mut word_value)?;
            v[i] = u32::from_le_bytes(word_value);
        }
        Ok(Values::from_words(&v, &self.module.outputs))
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
