use crate::ir::Module;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
pub enum TestValue {
    DontCare,
    Number(u32),
}

impl TestValue {
    fn from_str(s: &str) -> Self {
        if s == "x" {
            Self::DontCare
        } else {
            Self::Number(s.parse().unwrap())
        }
    }
}

#[derive(Debug)]
pub struct TestStep {
    pub reset: bool,
    pub inputs: crate::simulation::Values,
    pub expected_outputs: crate::simulation::Values,
}

#[derive(Debug)]
pub struct TestCase {
    pub auto_reset: bool,
    pub steps: Vec<TestStep>,
}

impl TestCase {
    fn from_csv<S: AsRef<str>>(module_spec: &Module, lines: &[S]) -> TestCase {
        // Strip out blank lines and comments
        let lines: Vec<_> = lines
            .iter()
            .map(|line| line.as_ref())
            .map(|line| {
                // Remove comments
                line.split("#").next().unwrap().trim().to_string()
            })
            .filter(|line| line.len() > 0)
            .map(|line| {
                line.split(",")
                    .map(|s| s.trim().to_string())
                    .collect::<Vec<_>>()
            })
            .collect();

        let header = &lines[0];
        let lines = &lines[1..];

        let auto_reset = header.contains(&"rst".to_string());

        // Check that the header includes all inputs
        if !module_spec
            .inputs
            .iter()
            .all(|input| header.contains(input))
        {
            eprintln!("Test header doesn't specify all module inputs");
            todo!("Return gracefully on error");
        }

        let mut steps = vec![];
        for (i, values) in lines.iter().enumerate() {
            if values.len() != header.len() {
                eprintln!("Line {} in test case doesn't have enough values!", i);
                todo!("Return gracefully on error");
            }

            let values: HashMap<_, _> = values
                .iter()
                .zip(header.iter())
                .map(|(value, header)| {
                    let value: TestValue = TestValue::from_str(value);
                    (header.to_string(), value)
                })
                .collect();

            let mut inputs: HashMap<_, _> = module_spec
                .inputs
                .iter()
                .map(|input| {
                    let value = match values.get(input) {
                        Some(TestValue::Number(x)) => *x,
                        _ => {
                            eprintln!("Expected number value for input");
                            todo!("Handle error gracefully");
                        }
                    };
                    (input.to_string(), value)
                })
                .collect();
            let reset = match values.get("rst") {
                Some(TestValue::Number(x)) => *x == 1,
                _ => false,
            };

            let outputs: HashMap<_, _> = module_spec
                .outputs
                .iter()
                .filter_map(|input| {
                    let input = input.to_string();
                    match values.get(&input) {
                        Some(TestValue::Number(x)) => Some((input, *x)),
                        Some(TestValue::DontCare) => None,
                        None => None,
                    }
                })
                .collect();

            steps.push(TestStep {
                reset,
                inputs: crate::simulation::Values(inputs),
                expected_outputs: crate::simulation::Values(outputs),
            });
        }

        TestCase { auto_reset, steps }
    }
}

pub fn parse_tests_from_comment(module: &Module, comment: &str) -> Vec<TestCase> {
    // Split the comment into lines, find test cases bounded by "```test" and "```"
    let mut tests = vec![];
    let mut current_test: Option<Vec<&str>> = None;
    for line in comment.lines() {
        if current_test.is_none() {
            if line.contains("```test") {
                current_test = Some(vec![]);
            }
        } else {
            if line.contains("```") {
                tests.push(current_test.take().unwrap());
            } else {
                current_test.as_mut().unwrap().push(line);
            }
        }
    }

    let tests: Vec<_> = tests
        .iter()
        .map(|test| TestCase::from_csv(module, test))
        .collect();

    tests
}
