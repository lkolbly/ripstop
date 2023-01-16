use serde::{Deserialize, Serialize};
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
use crate::types::Type;
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

    #[error("Error in module simulator")]
    SimulatorError(Box<dyn std::error::Error>),
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

/// Represents a section of a variable within a word.
#[derive(Debug, PartialEq, Serialize)]
struct NamedVariableInWord {
    name: String,

    /// The part of the variable contained in this word
    var_section: Range,

    /// The part of the word that this snippet takes up
    word_section: Range,
}

/// Represents a mapping of some set of typed variables to a linear set of bits
#[derive(Debug)]
struct LinearVariableMapping {
    variables: Vec<(String, Type, Range)>,
}

impl LinearVariableMapping {
    fn new(mut variables: Vec<(String, Type)>) -> Self {
        let x: Vec<_> = variables
            .drain(..)
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
        Self { variables: x }
    }

    fn from_variable_names(variable_table: &HashMap<String, Type>, variables: &[String]) -> Self {
        let variables: Vec<_> = variables
            .iter()
            .map(|varname| {
                let t = variable_table.get(varname).unwrap();
                (varname.to_string(), t.clone())
            })
            .collect();
        Self::new(variables)
    }

    /// Converts this representation into a list of words, where each word contains the
    /// variables (and their ranges) contained in that 32-bit word.
    fn to_words(&self) -> Vec<Vec<NamedVariableInWord>> {
        let mut res = vec![];
        for i in 0..self.get_size_bits() / 32 {
            let i = i as u32;
            let low = i * 32;
            let high = (i + 1) * 32 - 1;
            let mut x: Vec<_> = self
                .variables
                .iter()
                .filter(|(_, _, range)| range.low <= high && range.high >= low)
                .collect();
            x.sort_by_key(|(_, _, r)| -(r.low as i32));
            let x: Vec<_> = x
                .iter()
                .map(|(name, _, range)| {
                    let word_low = range.low.saturating_sub(32 * i);
                    let word_high = (range.high - 32 * i).min(31);
                    let var_low = if range.low < 32 * i {
                        32 * i - range.low
                    } else {
                        0
                    };
                    let var_high = var_low + word_high - word_low;
                    println!("{} {:?} {} {} {}", name, range, i, var_low, var_high);
                    NamedVariableInWord {
                        name: name.to_owned(),
                        var_section: Range {
                            low: var_low,
                            high: var_high,
                        },
                        word_section: Range {
                            low: word_low,
                            high: word_high,
                        },
                    }
                })
                .collect();
            res.push(x);
        }
        res
    }

    /// Returns the number of bits needed to represent this mapping, rounded
    /// to the correct number of words
    fn get_size_bits(&self) -> usize {
        let bits = if self.variables.len() == 0 {
            0
        } else {
            self.variables[self.variables.len() - 1].2.high as usize + 1
        };
        if bits == 0 {
            32
        } else if bits % 32 != 0 {
            bits + 32 - bits % 32
        } else {
            bits
        }
    }
}

pub trait ModuleSimulator {
    fn step(&self, inputs: Values) -> Values;
}

/// This is the struct used to generate the simulation's shims
#[derive(Clone, Debug, Serialize, Deserialize)]
struct ShimModuleDefinition {
    pub name: String,
    pub path: Vec<String>,
    pub inputs: Vec<(String, Type, usize)>,
    pub outputs: Vec<(String, Type, usize)>,
}

impl std::convert::From<&ExternalModule> for ShimModuleDefinition {
    fn from(value: &ExternalModule) -> Self {
        Self {
            name: value.module_name.clone(),
            path: value.instance_path.clone(),
            inputs: value
                .declaration
                .inputs
                .iter()
                .map(|(vartype, name)| (name.to_string(), vartype.clone(), vartype.bit_size()))
                .collect(),
            outputs: value
                .declaration
                .outputs
                .iter()
                .map(|(vartype, name)| (name.to_string(), vartype.clone(), vartype.bit_size()))
                .collect(),
        }
    }
}

pub struct Module {
    inputs: LinearVariableMapping,
    outputs: LinearVariableMapping,
    external_outputs: LinearVariableMapping,
    external_inputs: LinearVariableMapping,
    input_bytes: usize,
    output_words: usize,
    external_output_bytes: usize,
    external_input_words: usize,
    executable_file: Arc<ExecutableFile>,
    dumpfile: Option<std::path::PathBuf>,
    simulators: HashMap<String, Box<dyn ModuleSimulator + Send>>,
    external_modules: Vec<ShimModuleDefinition>,
}

impl Module {
    pub fn new<P: AsRef<std::path::Path>>(
        input: std::path::PathBuf,
        top: &str,
        dumpfile: Option<P>,
        simulators: HashMap<String, Box<dyn ModuleSimulator + Send>>,
    ) -> Result<Self> {
        let inputpath = input.clone();

        let input = std::fs::read_to_string(&inputpath)
            .map_err(|e| Error::SimulationCompileFailed(Box::new(e)))?;

        let mut a = parse(&input)?;

        let result = compile_document(&mut a);
        if result.errors.len() > 0 {
            return Err(Error::RipstopError(result.errors));
        }
        let (module_declarations, mut modules, v_a) = result.result.unwrap();

        let external_modules =
            collect_external_modules(top, &module_declarations, &modules).unwrap();
        let external_modules: Vec<ShimModuleDefinition> =
            external_modules.iter().map(|x| x.into()).collect();

        // TODO: Verify that all required external modules are specified

        let external_inputs = LinearVariableMapping::new(
            external_modules
                .iter()
                .flat_map(|module| {
                    let v: Vec<_> = module
                        .inputs
                        .iter()
                        .map(|(a, b, _)| (format!("{}.{}", module.path.join("."), a), b.clone()))
                        .collect();
                    v
                })
                .collect(),
        );
        let external_outputs = LinearVariableMapping::new(
            external_modules
                .iter()
                .flat_map(|module| {
                    let v: Vec<_> = module
                        .outputs
                        .iter()
                        .map(|(a, b, _)| {
                            (format!("{}.__rp_{}", module.path.join("."), a), b.clone())
                        })
                        .collect();
                    v
                })
                .collect(),
        );
        println!("{:#?}", external_inputs);
        println!("{:#?}", external_outputs);

        // Format the external inputs as required for the template
        // An array of words, each word contains the variables in it
        let template_external_inputs = external_inputs.to_words();

        // Find the top module and compile it
        let module = modules
            .drain(..)
            .filter(|m| m.name == top)
            .next()
            .expect("Couldn't find module");

        let compiled = verilog_ast_to_string(v_a.find_head().unwrap(), &v_a, 0);

        let inputs = LinearVariableMapping::from_variable_names(&module.variables, &module.inputs);
        let outputs =
            LinearVariableMapping::from_variable_names(&module.variables, &module.outputs);
        let input_size = inputs.get_size_bits();
        let output_size = outputs.get_size_bits();

        // Build the template
        let mut tera = tera::Tera::default();
        tera.add_raw_template(
            "simulation_harness.v",
            include_str!("../../templates/simulation_harness.v"),
        )
        .unwrap();

        let input_bytes = input_size / 8;
        let output_words = output_size / 32;
        let output_word_iterator: Vec<u32> = (0..output_words as u32).collect();

        let mut context = tera::Context::new();
        context.insert("compiled", &compiled);
        context.insert("module_name", &module.name);
        context.insert("inputs", &inputs.variables);
        context.insert("outputs", &outputs.variables);
        context.insert("input_size", &input_size);
        context.insert("output_size", &output_size);
        context.insert("input_bytes", &input_bytes);
        context.insert("output_words", &output_words);
        context.insert("output_word_iterator", &output_word_iterator);
        context.insert("external_modules", &external_modules);
        let external_output_bytes = external_outputs.get_size_bits() / 8;
        context.insert("external_output_bytes", &external_output_bytes);
        context.insert("external_outputs", &external_outputs.variables);
        let external_input_words = external_inputs.get_size_bits() / 32;
        context.insert("external_input_words", &external_input_words);
        context.insert("external_inputs", &template_external_inputs);

        if let Some(dumpfile) = &dumpfile {
            context.insert("output_dumpfile", &true);
            context.insert("dumpfile", dumpfile.as_ref());
        } else {
            context.insert("output_dumpfile", &false);
        }

        let harness = tera.render("simulation_harness.v", &context).unwrap();

        let mut sim_file = tempfile::NamedTempFile::new()?;

        //println!("{}", harness);
        //todo!();

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

        // Temporary for debugging
        sim_file.keep()?;

        p.communicate(None)?;

        Ok(Self {
            inputs,
            outputs,
            input_bytes: input_bytes as usize,
            output_words: output_words as usize,
            external_output_bytes,
            external_outputs,
            external_input_words,
            external_inputs,
            executable_file: Arc::new(ExecutableFile(output_file)),
            dumpfile: dumpfile.map(|p| p.as_ref().to_path_buf()),
            simulators,
            external_modules,
        })
    }

    pub fn instantiate(self) -> Instance {
        Instance::new(self)
    }
}

#[derive(Clone, Debug)]
pub struct Values(pub HashMap<String, u32>);

impl Values {
    fn to_bytes(&self, input_bytes: usize, mapping: &LinearVariableMapping) -> Vec<u8> {
        let mut input_bytes = vec![0u8; input_bytes];
        self.0.iter().for_each(|(k, v)| {
            let (_, _, r) = mapping
                .variables
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

    fn from_words(input: &[u32], mapping: &LinearVariableMapping) -> Self {
        let mut result = HashMap::new();
        mapping.variables.iter().for_each(|(name, _, range)| {
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

        let input = input.to_bytes(self.module.input_bytes, &self.module.inputs);
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

        let input = input.to_bytes(self.module.input_bytes, &self.module.inputs);
        self.stdin().write_all(&input[..])?;
        self.stdin().flush()?;

        // Clear reset
        self.send_command(107)?;

        self.update_external_modules()?;

        // Retrieve the output
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

    fn update_external_modules(&self) -> StepResult<()> {
        // Get the external module data
        self.send_command(110)?;

        let mut v = vec![0u32; self.module.external_input_words];
        for i in 0..self.module.external_input_words {
            let mut word_value = [0u8; 4];
            self.stdout().read_exact(&mut word_value)?;
            v[i] = u32::from_le_bytes(word_value);
        }
        let external_inputs = Values::from_words(&v, &self.module.external_inputs);
        println!("{:?}", v);
        println!("{:#?}", external_inputs);
        println!("{:#?}", self.module.external_modules);

        let mut eo = Values(HashMap::new());
        for extern_module in self.module.external_modules.iter() {
            let path = extern_module.path.join(".");

            // Collect all of the inputs
            let mut inputs = HashMap::new();
            for input in extern_module.inputs.iter() {
                let path = format!("{}.{}", path, input.0);
                inputs.insert(input.0.clone(), *external_inputs.0.get(&path).unwrap());
            }
            let inputs = Values(inputs);
            println!("{} {:?}", path, inputs);

            // Get the simulator
            let sim = self.module.simulators.get(&path).unwrap();
            let outputs = sim.step(inputs);
            println!("{:?}", outputs);

            for (out_name, out_value) in outputs.0.iter() {
                // TODO: We need to check that the given variable actually exists
                eo.0.insert(format!("{}.__rp_{}", path, out_name), *out_value);
            }
        }

        // Set the external module data
        self.stdin().write_all(&[111])?;
        let eo = eo.to_bytes(
            self.module.external_output_bytes,
            &self.module.external_outputs,
        );
        self.stdin().write_all(&eo)?;
        self.stdin().flush()?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn var(name: &str, size: usize) -> (String, Type) {
        (name.to_owned(), Type::Bits { size })
    }

    #[test]
    fn test_values_to_bytes() {
        let mapping = LinearVariableMapping::new(vec![var("data_in", 5), var("save", 1)]);

        let res = Values(HashMap::from([
            ("data_in".to_string(), 0),
            ("save".to_string(), 1),
        ]))
        .to_bytes(1, &mapping);
        assert_eq!(res, &[32]);

        let res = Values(HashMap::from([
            ("data_in".to_string(), 10),
            ("save".to_string(), 0),
        ]))
        .to_bytes(1, &mapping);
        assert_eq!(res, &[10]);

        let res = Values(HashMap::from([
            ("data_in".to_string(), 1),
            ("save".to_string(), 1),
        ]))
        .to_bytes(1, &mapping);
        assert_eq!(res, &[33]);
    }

    #[test]
    fn test_values_to_bytes_bridging_bytes() {
        let mapping = LinearVariableMapping::new(vec![var("a", 5), var("b", 9), var("c", 6)]);

        let res = Values(HashMap::from([
            ("a".to_string(), 10),
            ("b".to_string(), 123),
            ("c".to_string(), 19),
        ]))
        .to_bytes(3, &mapping);
        assert_eq!(res, &[0x6a, 0xcf, 0x4]);
    }

    #[test]
    fn test_values_to_bytes_multibyte() {
        let mapping = LinearVariableMapping::new(vec![var("a", 10), var("b", 4), var("c", 22)]);

        let res = Values(HashMap::from([
            ("a".to_string(), 300),
            ("b".to_string(), 5),
            ("c".to_string(), 1_000_000),
        ]))
        .to_bytes(5, &mapping);
        assert_eq!(res, &[0x2c, 0x15, 0x90, 0xd0, 0x3]);
    }

    #[test]
    fn test_values_from_words() {
        let mapping = LinearVariableMapping::new(vec![var("data_in", 5), var("save", 1)]);

        let values = Values::from_words(&[32], &mapping);
        assert_eq!(*values.0.get("data_in").unwrap(), 0);
        assert_eq!(*values.0.get("save").unwrap(), 1);
    }

    #[test]
    fn test_values_from_words_word_boundary() {
        let mapping = LinearVariableMapping::new(vec![var("a", 29), var("b", 9)]);

        let values = Values::from_words(&[0x600f_4240, 0x0f], &mapping);
        assert_eq!(*values.0.get("a").unwrap(), 1_000_000);
        assert_eq!(*values.0.get("b").unwrap(), 123);
    }

    fn named_range(
        name: &str,
        high: usize,
        low: usize,
        word_high: usize,
        word_low: usize,
    ) -> NamedVariableInWord {
        NamedVariableInWord {
            name: name.to_owned(),
            var_section: Range {
                high: high as u32,
                low: low as u32,
            },
            word_section: Range {
                high: word_high as u32,
                low: word_low as u32,
            },
        }
    }

    #[test]
    fn test_make_words() {
        let mapping = LinearVariableMapping::new(vec![var("a", 16), var("b", 16)]);
        assert_eq!(
            mapping.to_words(),
            vec![vec![
                named_range("b", 15, 0, 31, 16),
                named_range("a", 15, 0, 15, 0),
            ]]
        );
    }

    #[test]
    fn test_words_multi() {
        let mapping =
            LinearVariableMapping::new(vec![var("a", 5), var("b", 16), var("c", 11), var("d", 32)]);
        assert_eq!(
            mapping.to_words(),
            vec![
                vec![
                    named_range("c", 10, 0, 31, 21),
                    named_range("b", 15, 0, 20, 5),
                    named_range("a", 4, 0, 4, 0),
                ],
                vec![named_range("d", 31, 0, 31, 0)],
            ]
        );
    }

    #[test]
    fn test_words_split() {
        let mapping =
            LinearVariableMapping::new(vec![var("a", 5), var("b", 16), var("c", 23), var("d", 20)]);
        assert_eq!(
            mapping.to_words(),
            vec![
                vec![
                    named_range("c", 10, 0, 31, 21),
                    named_range("b", 15, 0, 20, 5),
                    named_range("a", 4, 0, 4, 0),
                ],
                vec![
                    named_range("d", 19, 0, 31, 12),
                    named_range("c", 22, 11, 11, 0),
                ],
            ]
        );
    }

    #[test]
    fn test_empty() {
        // When the mapping is empty, we should still output one word
        let mapping = LinearVariableMapping::new(vec![]);
        let expected: Vec<Vec<NamedVariableInWord>> = vec![vec![]];
        assert_eq!(mapping.to_words(), expected);
    }
}
