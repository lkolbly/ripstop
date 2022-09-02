use crate::ast::ASTNode;
use crate::ast::ASTNodeType;
use crate::ast::StringContext;
use crate::ast::Type;
use crate::error::CompileError;
use crate::tree::NodeId;
use crate::tree::Tree;

use lazy_static::lazy_static;

use pest::iterators::Pair;
use pest::prec_climber::Assoc;
use pest::prec_climber::Operator;
use pest::prec_climber::PrecClimber;
use pest::Parser;

use serde::Serialize;
use std::{cell::RefCell, rc::Rc};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct RipstopParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = PrecClimber::new(vec![
        Operator::new(Rule::addition, Assoc::Left)
            | Operator::new(Rule::subtraction, Assoc::Left)
            | Operator::new(Rule::bitwise_and, Assoc::Left)
            | Operator::new(Rule::bitwise_xor, Assoc::Left)
            | Operator::new(Rule::bitwise_or, Assoc::Left)
            | Operator::new(Rule::equal, Assoc::Left)
            | Operator::new(Rule::notequal, Assoc::Left)
            | Operator::new(Rule::greater, Assoc::Left)
            | Operator::new(Rule::less, Assoc::Left)
            | Operator::new(Rule::greater_eq, Assoc::Left)
            | Operator::new(Rule::less_eq, Assoc::Left)
            | Operator::new(Rule::concatenate, Assoc::Left)
    ]);
}

pub fn parse(toparse: &str) -> Result<Tree<ASTNode>, CompileError> {
    let mut parsed = match RipstopParser::parse(Rule::document, toparse) {
        Ok(x) => x,
        Err(e) => match e.variant {
            pest::error::ErrorVariant::ParsingError {
                positives,
                negatives,
            } => {
                println!("{:?} {:?}", positives, negatives);
                return Err(CompileError::ParseError {
                    expected: positives.iter().map(|rule| format!("{:?}", rule)).collect(),
                    location: StringContext::from_location(toparse, &e.line_col),
                });
            }
            pest::error::ErrorVariant::CustomError { message } => {
                todo!();
            }
        },
    };
    let parsed = parsed.next().unwrap();

    let mut tree = Tree::<ASTNode>::new();
    parse_value(&mut tree, parsed);
    return Ok(tree);

    fn parse_value(tree: &mut Tree<ASTNode>, pair: Pair<'_, Rule>) -> Option<NodeId> {
        let rule = pair.as_rule();
        let mut inner_rules = pair.clone().into_inner();

        let node_type = match rule {
            Rule::document => Some(ASTNodeType::Document),
            Rule::module_declaration => Some(ASTNodeType::ModuleDeclaration {
                id: inner_rules.next().unwrap().as_str().to_string(),
                in_values: parse_variable_declarations(inner_rules.next().unwrap()),
                out_values: parse_variable_declarations(inner_rules.next().unwrap()),
            }),
            Rule::assignment => Some(ASTNodeType::Assign),
            Rule::indexed_variable => Some(ASTNodeType::VariableReference {
                var_id: inner_rules.next().unwrap().as_str().to_string(),
            }),
            Rule::variable_index_relative => Some(ASTNodeType::TimeOffsetRelative {
                offset: {
                    match inner_rules.next() {
                        Some(inner) => {
                            let sign = match inner.as_rule() {
                                Rule::addition => 1,
                                Rule::subtraction => -1,
                                _ => unreachable!(),
                            };
                            let unsigned: i64 =
                                NumberLiteral::from_tree(inner_rules.next().unwrap())
                                    .value
                                    .try_into()
                                    .unwrap();
                            sign * unsigned
                        }
                        None => 0,
                    }
                },
            }),
            Rule::variable_index_absolute => Some(ASTNodeType::TimeOffsetAbsolute {
                time: SignedInteger::from_tree(inner_rules.next().unwrap()).0,
            }),
            Rule::unary_operation => Some(
                match inner_rules
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_rule()
                {
                    Rule::bitwise_inverse => ASTNodeType::BitwiseInverse,
                    _ => unreachable!(),
                },
            ),
            Rule::binary_operation => None,
            Rule::variable_declaration => Some(ASTNodeType::VariableDeclaration {
                var_type: parse_type(inner_rules.next().unwrap()),
                var_id: inner_rules.next().unwrap().as_str().to_string(),
            }),
            Rule::index_expression => {
                println!("{:#?}", inner_rules);

                let indexee = inner_rules.next().unwrap();
                let index = inner_rules.next().unwrap();

                //let rules: Vec<_> = inner_rules.collect();
                //assert_eq!(rules.len(), 2);

                let indexee = parse_value(tree, indexee).unwrap();

                // Get the index
                let index = match index.as_rule() {
                    Rule::bits_index => Range::from_tree(index.into_inner().next().unwrap()),
                    _ => {
                        panic!("Second thingy must be a bits_index");
                    }
                };
                let index = ASTNodeType::Index {
                    high: index.high as usize,
                    low: index.low as usize,
                };
                let index_node = tree.new_node(ASTNode::new(index, pair));
                tree.append_to(index_node, indexee).unwrap();
                return Some(index_node);
            }
            Rule::conditional => {
                println!("{:#?}", inner_rules);
                Some(ASTNodeType::Conditional)
            }
            Rule::conditional_block => Some(ASTNodeType::Block),
            Rule::instantiate_statement => {
                println!("{:#?}", inner_rules);
                let module = inner_rules.next().unwrap();
                let instance = inner_rules.next().unwrap();

                let module = match module.as_rule() {
                    Rule::name => module.as_str().to_owned(),
                    _ => {
                        panic!("Module name must be a name");
                    }
                };
                let instance = match instance.as_rule() {
                    Rule::name => instance.as_str().to_owned(),
                    _ => {
                        panic!("Instance name must be a name");
                    }
                };
                Some(ASTNodeType::ModuleInstantiation { module, instance })
            }
            Rule::EOI => None,
            Rule::number_literal => Some(ASTNodeType::NumberLiteral(NumberLiteral::from_tree(
                pair.clone(),
            ))),
            // Hack to not recursively parse number literals
            Rule::full_number_literal => None,
            _ => {
                println!(
                    "Unimplemented rule '{:?}' for {:?}",
                    rule,
                    pair.clone().as_span()
                );
                todo!();
            }
        };

        if let Some(n_type) = node_type {
            let data = ASTNode::new(n_type, pair);
            let node = tree.new_node(data);
            for child in inner_rules {
                if let Some(child_node) = parse_value(tree, child) {
                    tree.append_to(node, child_node).unwrap();
                }
            }
            Some(node)
        } else if let Rule::binary_operation = rule {
            let treerc = Rc::new(RefCell::new(tree));

            PREC_CLIMBER.climb(
                inner_rules.clone(),
                |pair| parse_value(&mut (*treerc).borrow_mut(), pair),
                |lhs, op, rhs| {
                    let n_type = match op.as_rule() {
                        Rule::addition => ASTNodeType::Add,
                        Rule::subtraction => ASTNodeType::Subtract,
                        Rule::bitwise_and => ASTNodeType::BitwiseAnd,
                        Rule::bitwise_xor => ASTNodeType::BitwiseXor,
                        Rule::bitwise_or => ASTNodeType::BitwiseOr,
                        Rule::equal => ASTNodeType::Equal,
                        Rule::notequal => ASTNodeType::NotEqual,
                        Rule::greater => ASTNodeType::Greater,
                        Rule::less => ASTNodeType::Less,
                        Rule::greater_eq => ASTNodeType::GreaterEq,
                        Rule::less_eq => ASTNodeType::LessEq,
                        Rule::concatenate => ASTNodeType::Concatenate,
                        _ => unreachable!(),
                    };
                    let data = ASTNode::new(n_type, op);
                    let child = (&mut (*treerc).borrow_mut()).new_node(data);
                    (&mut (*treerc).borrow_mut())
                        .append_to(child, lhs.unwrap())
                        .unwrap();
                    (&mut (*treerc).borrow_mut())
                        .append_to(child, rhs.unwrap())
                        .unwrap();
                    Some(child)
                },
            )
        } else {
            None
        }
    }

    fn parse_variable_declarations(pair: Pair<Rule>) -> Vec<(Type, String)> {
        return pair.into_inner().map(parse_variable_declaration).collect();
    }

    fn parse_variable_declaration(pair: Pair<Rule>) -> (Type, String) {
        let mut inner_rules = pair.into_inner();
        return (
            parse_type(inner_rules.next().unwrap()),
            inner_rules.next().unwrap().as_str().to_string(),
        );
    }

    fn parse_type(pair: Pair<Rule>) -> Type {
        let child = pair
            .into_inner()
            .next()
            .unwrap()
            .into_inner()
            .next()
            .unwrap();
        match child.as_rule() {
            Rule::bit_type => Type::Bit,
            Rule::bits_type => Type::Bits {
                size: NumberLiteral::from_tree(child.into_inner().next().unwrap())
                    .value
                    .try_into()
                    .unwrap(),
            },
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(transparent)]
pub struct SignedInteger(i64);

impl SignedInteger {
    fn from_tree(tree: Pair<Rule>) -> Self {
        let full_str = tree.as_str();
        Self(full_str.parse().unwrap())
    }
}

/// Represents a doubly-inclusive range, for example [6:4] to indicate bits
/// 6, 5, and 4 of a bits.
#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub struct Range {
    pub low: u32,
    pub high: u32,
}

impl Range {
    fn from_tree(tree: Pair<Rule>) -> Self {
        let rule = tree.as_rule();
        let inner = tree.into_inner();
        let children: Vec<_> = match rule {
            Rule::range => inner.collect(),
            _ => panic!("Unexpected rule"),
        };

        if children.len() == 2 {
            let high: u32 = children[0].as_str().parse().unwrap();
            let low: u32 = children[1].as_str().parse().unwrap();
            if low > high {
                panic!("Low and high are swapped!");
            }
            Self { low, high }
        } else if children.len() == 1 {
            let val: u32 = children[0].as_str().parse().unwrap();
            Self {
                low: val,
                high: val,
            }
        } else {
            panic!("Unexpected # of children!");
        }

        //Self { low: 0, high: 5 }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NumberLiteral {
    pub size_bits: usize,
    pub value: u128, // This is the maximum supported integer literal
}

impl NumberLiteral {
    fn from_tree(tree: Pair<Rule>) -> Self {
        let full_str = tree.as_str();

        let rule = tree.as_rule();
        let inner = tree.into_inner();
        let mut tree = match rule {
            Rule::number_literal => {
                println!("{:?}", inner);
                inner
            }
            _ => {
                panic!("Unexpected rule! {:?}", rule);
            }
        };

        let tree = tree.next().unwrap();

        let mut num_bits = None;
        let mut base = None;
        let mut digits = None;

        let rule = tree.as_rule();
        let inner = tree.into_inner();
        match rule {
            Rule::full_number_literal => {
                println!("{:?}", inner);
                for pair in inner {
                    println!("{:?}", pair);
                    let rule = pair.as_rule();
                    match rule {
                        Rule::number_literal_bits => {
                            num_bits = Some(pair.as_span());
                        }
                        Rule::number_literal_base => {
                            base = Some(pair.as_span());
                        }
                        Rule::number_literal_value => {
                            digits = Some(pair.as_span());
                        }
                        _ => {
                            panic!("Unexpected rule!");
                        }
                    }
                }
            }
            Rule::pos_integer => {
                let int: u128 = full_str.parse().unwrap();
                return Self {
                    size_bits: 64,
                    value: int,
                };
            }
            _ => {
                panic!("Unexpected rule! {:?}", rule);
            }
        };

        println!("{:?} {:?} {:?}", num_bits, base, digits);

        match (num_bits, base, digits) {
            (Some(num_bits), Some(base), Some(digits)) => {
                let num_bits: usize = num_bits.as_str().parse().unwrap();
                let base = match base.as_str() {
                    "b" => 2,
                    "o" => 8,
                    "d" => 10,
                    "h" => 16,
                    _ => {
                        panic!("Unexpected base");
                    }
                };
                let mut value: Option<u128> = Some(0);
                for c in digits.as_str().chars() {
                    if c == '_' {
                        // Ignore - user-provided separater
                        continue;
                    }
                    let digit_value = u8::from_str_radix(&format!("{}", c), 16).unwrap();
                    if digit_value > base {
                        panic!("Got invalid digit for base!");
                    }
                    value = value
                        .and_then(|x| x.checked_mul(base as u128))
                        .and_then(|x| x.checked_add(digit_value as u128));
                }

                let value = value.unwrap();

                if 128 - value.leading_zeros() > num_bits as u32 {
                    panic!("Number was too big!");
                }

                println!("{} {} {}", num_bits, base, value);
                Self {
                    size_bits: num_bits,
                    value,
                }
            }
            _ => {
                unimplemented!();
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_range() {
        let tests = [
            ("5", Range { low: 5, high: 5 }),
            ("6:0", Range { low: 0, high: 6 }),
            ("123:43", Range { low: 43, high: 123 }),
            //("1:2", Range{low: 43, high: 123}), // Should error
        ];

        for (test_case, expected) in tests.iter() {
            let parsed = RipstopParser::parse(Rule::range, test_case)
                .expect("Parse failed!")
                .next()
                .unwrap();

            let literal = Range::from_tree(parsed);
            assert_eq!(&literal, expected);
        }
    }

    #[test]
    fn test_parse_signed_integer() {
        let tests = [("5", SignedInteger(5)), ("-12357", SignedInteger(-12357))];

        for (test_case, expected) in tests.iter() {
            let parsed = RipstopParser::parse(Rule::signed_integer, test_case)
                .expect("Parse failed!")
                .next()
                .unwrap();

            //println!("{:?}", parsed);
            let literal = SignedInteger::from_tree(parsed);
            //println!("{:?}", literal);
            assert_eq!(&literal, expected);
        }
    }

    #[test]
    fn test_parse_integer_literal() {
        let tests = [
            (
                "5'd16",
                NumberLiteral {
                    size_bits: 5,
                    value: 16,
                },
            ),
            (
                "6'b10100",
                NumberLiteral {
                    size_bits: 6,
                    value: 20,
                },
            ),
            (
                "32'hffab",
                NumberLiteral {
                    size_bits: 32,
                    value: 0xffab,
                },
            ),
            (
                "48'habcd_ef01_2345",
                NumberLiteral {
                    size_bits: 48,
                    value: 0xabcd_ef01_2345,
                },
            ),
            (
                "128'hffffffff_ffffffff_ffffffff_ffffffff",
                NumberLiteral {
                    size_bits: 128,
                    value: 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff,
                },
            ),
            (
                "1'h1",
                NumberLiteral {
                    size_bits: 1,
                    value: 1,
                },
            ),
            (
                "1'b0",
                NumberLiteral {
                    size_bits: 1,
                    value: 0,
                },
            ),
            //("129'h5", NumberLiteral { size_bits: 128, value: 0 }), // This should fail
            //("1'h2", NumberLiteral { size_bits: 1, value: 0 }), // Too big
            //("1'h__", NumberLiteral { size_bits: 1, value: 0 }), // This should fail!
            //("1'h1_", NumberLiteral { size_bits: 1, value: 0 }), // This should fail!
            //("1'h_1", NumberLiteral { size_bits: 1, value: 0 }), // This should fail!
            //("128'hffffffff_ffffffff_ffffffff_fffffffff", NumberLiteral { size_bits: 1, value: 0 }), // Too big
            //("32'hffffffff1", NumberLiteral { size_bits: 1, value: 0 }), // Too big
            //("32'd123a", NumberLiteral { size_bits: 1, value: 0 }), // This should fail!
            //("32'b123", NumberLiteral { size_bits: 1, value: 0 }), // Incorrect digit for base
            //("32'o8", NumberLiteral { size_bits: 1, value: 0 }), // Incorrect digit for base. This should fail!
        ];

        for (test_case, expected) in tests.iter() {
            let parsed = RipstopParser::parse(Rule::number_literal, test_case)
                .expect("Parse failed!")
                .next()
                .unwrap();

            println!("{:?}", parsed);
            let literal = NumberLiteral::from_tree(parsed);
            println!("{:?}", literal);
            assert_eq!(&literal, expected);
        }
    }
}
