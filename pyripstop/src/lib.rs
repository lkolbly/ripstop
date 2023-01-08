use pyo3::exceptions::PyRuntimeError;
use pyo3::prelude::*;
use pyo3::types::{PyAny, PyType};
use std::collections::HashMap;

use ripstop;

/// Formats the sum of two numbers as string.
#[pyfunction]
fn sum_as_string(a: usize, b: usize) -> PyResult<String> {
    Ok((a + b).to_string())
}

#[pyclass]
struct SimulationInstance {
    instance: Option<ripstop::simulation::Instance>,
}

struct Error(ripstop::simulation::Error);

impl std::convert::From<Error> for PyErr {
    fn from(e: Error) -> Self {
        let message = match e.0 {
            ripstop::simulation::Error::IverilogCompileFailed(e) => {
                format!("Couldn't run iverilog: {}", e)
            }
            ripstop::simulation::Error::IverilogNotFound(e) => {
                format!("Couldn't find iverilog (set the RIPSTOP_IVERILOG_BIN environment variable): {}", e)
            }
            ripstop::simulation::Error::RipstopError(e) => {
                let mut s = String::new();
                s.push_str("Couldn't compile ripstop: ");
                for err in e.iter() {
                    s.push_str(&format!("{:?}", err));
                }
                s
            }
            ripstop::simulation::Error::SimulationCompileFailed(e) => {
                format!("Simulation compilation failed: {}", e)
            }
            ripstop::simulation::Error::TemporaryFileError(e) => {
                format!("Couldn't create temporary file: {}", e)
            }
        };
        PyRuntimeError::new_err(message)
    }
}

impl std::convert::From<ripstop::simulation::Error> for Error {
    fn from(err: ripstop::simulation::Error) -> Self {
        Self(err)
    }
}

struct PythonModuleSim {
    object: Py<PyAny>,
}

impl ripstop::simulation::ModuleSimulator for PythonModuleSim {
    fn step(&self, inputs: ripstop::simulation::Values) -> ripstop::simulation::Values {
        let x = Python::with_gil(|py| {
            let res = self.object.call(py, (inputs.0,), None);
            let res = res.unwrap();
            let res: &pyo3::types::PyDict = res.cast_as(py).unwrap();
            let mut d: HashMap<String, u32> = HashMap::new();
            for (k, v) in res.iter() {
                let k: &pyo3::types::PyString = k.cast_as().unwrap();
                let v: u32 = v.extract().unwrap();
                d.insert(k.to_string_lossy().to_string(), v);
            }
            d
        });
        ripstop::simulation::Values(x)
    }
}

#[pymethods]
impl SimulationInstance {
    #[new]
    fn new(
        path: String,
        top: String,
        simulators: Option<HashMap<String, Py<PyAny>>>,
    ) -> PyResult<Self> {
        let simulators = simulators.unwrap_or(HashMap::new());
        let mut rust_simulators: HashMap<
            String,
            Box<dyn ripstop::simulation::ModuleSimulator + Send>,
        > = HashMap::new();
        for sim in simulators.iter() {
            rust_simulators.insert(
                sim.0.clone(),
                Box::new(PythonModuleSim {
                    object: sim.1.clone(),
                }),
            );
        }
        let module = ripstop::simulation::Module::new::<&'static str>(
            std::path::PathBuf::from(path),
            &top,
            None,
            rust_simulators,
        )
        .map_err(|e| Error(e))?;
        Ok(Self {
            instance: Some(module.instantiate()),
        })
    }

    fn reset_step(&mut self, input: HashMap<String, u32>) -> PyResult<HashMap<String, u32>> {
        self.instance
            .as_mut()
            .unwrap()
            .reset_step(ripstop::simulation::Values(input))
            .map_err(|e| PyRuntimeError::new_err(format!("Error running step: {}", e)))
            .map(|x| x.0)
    }

    fn step(&mut self, input: HashMap<String, u32>) -> PyResult<HashMap<String, u32>> {
        self.instance
            .as_mut()
            .unwrap()
            .step(ripstop::simulation::Values(input))
            .map_err(|e| PyRuntimeError::new_err(format!("Error running step: {}", e)))
            .map(|x| x.0)
    }

    fn __enter__(&mut self) -> PyResult<()> {
        // This is actually a no-op, since new does the construction
        Ok(())
    }

    fn __exit__(
        &mut self,
        _ty: Option<&PyType>,
        _value: Option<&PyAny>,
        _traceback: Option<&PyAny>,
    ) -> PyResult<bool> {
        self.instance
            .take()
            .unwrap()
            .finish()
            .map_err(|e| PyRuntimeError::new_err(format!("Couldn't stop instance: {}", e)))?;
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
