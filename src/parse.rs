use crate::ast::ASTNode;
use crate::ast::ASTNodeType;
use crate::ast::Type;
use crate::tree::NodeId;
use crate::tree::Tree;

use lazy_static::lazy_static;

use pest::iterators::Pair;
use pest::prec_climber::Assoc;
use pest::prec_climber::Operator;
use pest::prec_climber::PrecClimber;
use pest::Parser;

use std::{cell::RefCell, rc::Rc};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct RipstopParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = PrecClimber::new(vec![
        Operator::new(Rule::addition, Assoc::Left) | Operator::new(Rule::subtraction, Assoc::Left)
    ]);
}

pub fn parse(toparse: &str) -> Tree<ASTNode> {
    let parsed = RipstopParser::parse(Rule::module_declaration, toparse)
        .expect("Parse failed!")
        .next()
        .unwrap();

    let mut tree = Tree::<ASTNode>::new();
    parse_value(&mut tree, parsed);
    return tree;

    fn parse_value(tree: &mut Tree<ASTNode>, pair: Pair<'_, Rule>) -> Option<NodeId> {
        let rule = pair.as_rule();
        let mut inner_rules = pair.clone().into_inner();

        let node_type = match rule {
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
                time: NumberLiteral::from_tree(inner_rules.next().unwrap())
                    .value
                    .try_into()
                    .unwrap(),
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
            Rule::EOI => None,
            _ => {
                println!("Unimplement rule '{:?}'", rule);
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
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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
                panic!("Unexpected rule!");
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
                panic!("Unexpected rule!");
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
