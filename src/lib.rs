extern crate pest;
#[macro_use]
extern crate pest_derive;

use pyo3::prelude::*;
use pyo3::types::{PyAny, PyType};
use std::collections::HashMap;

mod ast;
mod compile;
mod error;
mod ir;
mod parse;
mod simulation;
mod tree;
mod verilog_ast;

/// Formats the sum of two numbers as string.
#[pyfunction]
fn sum_as_string(a: usize, b: usize) -> PyResult<String> {
    Ok((a + b).to_string())
}

#[pyclass]
struct SimulationInstance {
    instance: Option<crate::simulation::Instance>,
}

#[pymethods]
impl SimulationInstance {
    #[new]
    fn new(path: String) -> Self {
        let module = crate::simulation::Module::new(std::path::PathBuf::from(path)).unwrap();
        Self {
            instance: Some(module.instantiate()),
        }
    }

    fn reset_step(&mut self, input: HashMap<String, u32>) -> HashMap<String, u32> {
        self.instance
            .as_mut()
            .unwrap()
            .reset_step(crate::simulation::Values(input))
            .0
    }

    fn step(&mut self, input: HashMap<String, u32>) -> HashMap<String, u32> {
        self.instance
            .as_mut()
            .unwrap()
            .step(crate::simulation::Values(input))
            .0
    }

    fn __enter__(&mut self) -> PyResult<()> {
        // This is actually a no-op, since new does the construction
        Ok(())
    }

    fn __exit__(
        &mut self,
        ty: Option<&PyType>,
        _value: Option<&PyAny>,
        _traceback: Option<&PyAny>,
    ) -> PyResult<bool> {
        self.instance.take().unwrap().finish();
        Ok(false)
    }
}

/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn ripstop_lib(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(sum_as_string, m)?)?;
    m.add_class::<SimulationInstance>()?;
    Ok(())
}
