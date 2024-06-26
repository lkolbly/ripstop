use crate::ast::ASTNode;
use crate::ast::ASTNodeType;
use crate::ast::ASTType;
use crate::ast::StringContext;
use crate::error::CompileError;
use crate::tree::NodeId;
use crate::tree::Tree;
use crate::types::Type;

use lazy_static::lazy_static;

use pest::iterators::Pair;
use pest::iterators::Pairs;
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
        Operator::new(Rule::equal, Assoc::Left)
            | Operator::new(Rule::notequal, Assoc::Left)
            | Operator::new(Rule::greater, Assoc::Left)
            | Operator::new(Rule::less, Assoc::Left)
            | Operator::new(Rule::greater_eq, Assoc::Left)
            | Operator::new(Rule::less_eq, Assoc::Left),
        Operator::new(Rule::bitwise_and, Assoc::Left)
            | Operator::new(Rule::bitwise_xor, Assoc::Left)
            | Operator::new(Rule::bitwise_or, Assoc::Left),
        Operator::new(Rule::addition, Assoc::Left) | Operator::new(Rule::subtraction, Assoc::Left),
        Operator::new(Rule::concatenate, Assoc::Left),
    ]);
}

pub fn parse(toparse: &str) -> Result<Tree<ASTNode>, CompileError> {
    let mut parsed = match RipstopParser::parse(Rule::document, toparse) {
        Ok(x) => x,
        Err(e) => match e.variant {
            pest::error::ErrorVariant::ParsingError {
                positives,
                negatives: _,
            } => {
                return Err(CompileError::ParseError {
                    expected: positives.iter().map(|rule| format!("{:?}", rule)).collect(),
                    location: StringContext::from_location(toparse, &e.line_col),
                });
            }
            pest::error::ErrorVariant::CustomError { message } => {
                todo!("Got custom error {}", message);
            }
        },
    };
    let parsed = parsed.next().unwrap();

    let mut tree = Tree::<ASTNode>::new();
    parse_value(&mut tree, parsed)?;
    return Ok(tree);

    fn consume_doc_comments<'a>(rules: &'_ mut Pairs<'a, Rule>) -> (String, Pair<'a, Rule>) {
        let next_elem = rules.next().unwrap();
        match next_elem.as_rule() {
            Rule::doc_comment => {
                let (comment, remaining) = consume_doc_comments(rules);
                (
                    format!(
                        "{}\n{}",
                        next_elem.as_str().strip_prefix("///").unwrap().trim(),
                        comment
                    )
                    .trim()
                    .to_string(),
                    remaining,
                )
            }
            _ => ("".to_string(), next_elem),
        }
    }

    fn parse_value(
        tree: &mut Tree<ASTNode>,
        pair: Pair<'_, Rule>,
    ) -> Result<Option<NodeId>, CompileError> {
        let rule = pair.as_rule();
        let mut inner_rules = pair.clone().into_inner();

        let node_type = match rule {
            Rule::document => Some(ASTNodeType::Document),
            Rule::module_declaration => {
                let (doc_comment, id) = consume_doc_comments(&mut inner_rules);
                let id = id.as_str().to_string();

                let in_values = parse_variable_declarations(inner_rules.next().unwrap())?;
                let out_values = parse_variable_declarations(inner_rules.next().unwrap())?;
                Some(ASTNodeType::ModuleDeclaration {
                    id,
                    doc_comment,
                    in_values,
                    out_values,
                })
            }
            Rule::extern_module_declaration => {
                let (doc_comment, id) = consume_doc_comments(&mut inner_rules);
                let id = id.as_str().to_string();

                let in_values = parse_variable_declarations(inner_rules.next().unwrap())?;
                let out_values = parse_variable_declarations(inner_rules.next().unwrap())?;
                Some(ASTNodeType::ExternModuleDeclaration {
                    id,
                    doc_comment,
                    in_values,
                    out_values,
                })
            }
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
                            let lit = match NumberLiteral::from_tree(inner_rules.next().unwrap()) {
                                Ok(x) => x,
                                Err(e) => {
                                    return Err(CompileError::NumberParseError {
                                        error: e,
                                        context: StringContext::new(inner),
                                    })
                                }
                            };
                            let unsigned: i64 = lit.value.try_into().unwrap();
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
                var_type: parse_type(inner_rules.next().unwrap())?,
                var_id: inner_rules.next().unwrap().as_str().to_string(),
            }),
            Rule::index_expression => {
                let indexee = inner_rules.next().unwrap();
                let index = inner_rules.next().unwrap();

                //let rules: Vec<_> = inner_rules.collect();
                //assert_eq!(rules.len(), 2);

                let indexee = parse_value(tree, indexee)?.unwrap();

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
                return Ok(Some(index_node));
            }
            Rule::conditional => Some(ASTNodeType::Conditional),
            Rule::conditional_block => Some(ASTNodeType::Block),
            Rule::instantiate_statement => {
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
            Rule::struct_definition => {
                let name = inner_rules.next().unwrap().as_str();
                Some(ASTNodeType::StructDefinition {
                    name: name.to_string(),
                })
            }
            Rule::struct_variable => {
                let (t, n) = parse_variable_declaration(inner_rules.next().unwrap())?;
                Some(ASTNodeType::StructVariable {
                    name: n,
                    member_type: t,
                })
            }
            Rule::EOI => None,
            Rule::number_literal => {
                let lit = match NumberLiteral::from_tree(pair.clone()) {
                    Ok(x) => x,
                    Err(e) => {
                        return Err(CompileError::NumberParseError {
                            error: e,
                            context: StringContext::new(pair.clone()),
                        })
                    }
                };
                Some(ASTNodeType::NumberLiteral(lit))
            }
            // Hack to not recursively parse number literals
            Rule::pos_integer => None,
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
                if let Some(child_node) = parse_value(tree, child)? {
                    tree.append_to(node, child_node).unwrap();
                }
            }
            Ok(Some(node))
        } else if let Rule::binary_operation = rule {
            let treerc = Rc::new(RefCell::new(tree));

            PREC_CLIMBER.climb(
                inner_rules.clone(),
                |pair| parse_value(&mut (*treerc).borrow_mut(), pair),
                |lhs, op, rhs| {
                    if lhs.is_err() {
                        return lhs;
                    }
                    if rhs.is_err() {
                        return rhs;
                    }
                    let lhs = lhs.unwrap();
                    let rhs = rhs.unwrap();
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
                    Ok(Some(child))
                },
            )
        } else {
            Ok(None)
        }
    }

    fn parse_variable_declarations(
        pair: Pair<Rule>,
    ) -> Result<Vec<(ASTType, String)>, CompileError> {
        return pair.into_inner().map(parse_variable_declaration).collect();
    }

    fn parse_variable_declaration(pair: Pair<Rule>) -> Result<(ASTType, String), CompileError> {
        let mut inner_rules = pair.into_inner();
        return Ok((
            parse_type(inner_rules.next().unwrap())?,
            inner_rules.next().unwrap().as_str().to_string(),
        ));
    }

    fn parse_type(pair: Pair<Rule>) -> Result<ASTType, CompileError> {
        let child = pair
            .clone()
            .into_inner()
            .next()
            .unwrap()
            .into_inner()
            .next()
            .unwrap();
        match child.as_rule() {
            Rule::name => {
                let name = child.as_str();
                Ok(ASTType {
                    name: name.to_string(),
                    generic_parameter: None,
                })
            }
            Rule::generic_variable_type => {
                let mut inner = child.into_inner();
                let name = inner.next().unwrap();
                let value = inner.next().unwrap();
                let lit = match NumberLiteral::from_tree(value) {
                    Ok(x) => x,
                    Err(e) => {
                        return Err(CompileError::NumberParseError {
                            error: e,
                            context: StringContext::new(pair.clone()),
                        })
                    }
                };
                Ok(ASTType {
                    name: name.as_str().to_string(),
                    generic_parameter: Some(lit.value.try_into().unwrap()),
                })
            }
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

#[derive(Clone, Debug, PartialEq)]
pub struct NumberLiteral {
    pub ty: Type,
    pub value: u128, // This is the maximum supported integer literal
}

impl NumberLiteral {
    fn from_tree(tree: Pair<Rule>) -> Result<Self, Box<dyn std::error::Error>> {
        let full_str = tree.as_str();

        let rule = tree.as_rule();
        let inner = tree.into_inner();
        let mut tree = match rule {
            Rule::number_literal => inner,
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
                for pair in inner {
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
                let full_str = full_str.trim();
                let int: u128 = full_str.parse()?;
                let min_size = if int == 0 {
                    1
                } else {
                    (int + 1).next_power_of_two().trailing_zeros().try_into()?
                };
                return Ok(Self {
                    ty: Type::Literal {
                        minimum_size: min_size,
                    },
                    value: int,
                });
            }
            _ => {
                panic!("Unexpected rule! {:?}", rule);
            }
        };

        match (num_bits, base, digits) {
            (Some(num_bits), Some(base), Some(digits)) => {
                let num_bits: usize = num_bits.as_str().parse()?;
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

                Ok(Self {
                    ty: Type::Bits { size: num_bits },
                    value,
                })
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
                    ty: Type::Bits { size: 5 },
                    value: 16,
                },
            ),
            (
                "6'b10100",
                NumberLiteral {
                    ty: Type::Bits { size: 6 },
                    value: 20,
                },
            ),
            (
                "32'hffab",
                NumberLiteral {
                    ty: Type::Bits { size: 32 },
                    value: 0xffab,
                },
            ),
            (
                "48'habcd_ef01_2345",
                NumberLiteral {
                    ty: Type::Bits { size: 48 },
                    value: 0xabcd_ef01_2345,
                },
            ),
            (
                "128'hffffffff_ffffffff_ffffffff_ffffffff",
                NumberLiteral {
                    ty: Type::Bits { size: 128 },
                    value: 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff,
                },
            ),
            (
                "1'h1",
                NumberLiteral {
                    ty: Type::Bits { size: 1 },
                    value: 1,
                },
            ),
            (
                "1'b0",
                NumberLiteral {
                    ty: Type::Bits { size: 1 },
                    value: 0,
                },
            ),
            (
                "5",
                NumberLiteral {
                    ty: Type::Literal { minimum_size: 3 },
                    value: 5,
                },
            ),
            (
                "32767",
                NumberLiteral {
                    ty: Type::Literal { minimum_size: 15 },
                    value: 32767,
                },
            ),
            (
                "32768",
                NumberLiteral {
                    ty: Type::Literal { minimum_size: 16 },
                    value: 32768,
                },
            ),
            (
                "0",
                NumberLiteral {
                    ty: Type::Literal { minimum_size: 1 },
                    value: 0,
                },
            ),
            (
                "0 ",
                NumberLiteral {
                    ty: Type::Literal { minimum_size: 1 },
                    value: 0,
                },
            ), //("129'h5", NumberLiteral { size_bits: 128, value: 0 }), // This should fail
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
            let literal = NumberLiteral::from_tree(parsed).unwrap();
            println!("{:?}", literal);
            assert_eq!(&literal, expected);
        }
    }

    #[test]
    #[ignore]
    fn test_parse_doccomment() {
        let ast = parse(
            "/// ```test
/// rst,  a, b, c
///   1,  0, 0, x
///   1,  0, 0, x
///   0,  1, 2, 3
///   0, 15, 0, 15
///   0,  0, 0, 0
/// ```
module comb_add(bits<4> a, bits<4> b) -> (bits<4> c) {
    c[t] = a[t] + b[t];
}",
        )
        .unwrap();

        // The AST should have one module, with one doc comment
        let head = ast.find_head().unwrap();
        let modules: Vec<_> = ast
            .iter_subtree(head)
            .filter_map(|node| match &ast.get_node(node).unwrap().data.node_type {
                ASTNodeType::ModuleDeclaration {
                    id, doc_comment, ..
                } => Some((id, doc_comment)),
                _ => None,
            })
            .collect();

        assert_eq!(modules.len(), 1);
        assert_eq!(modules[0].0, "comb_add");
        assert_eq!(
            modules[0].1,
            "```test
rst,  a, b, c
  1,  0, 0, x
  1,  0, 0, x
  0,  1, 2, 3
  0, 15, 0, 15
  0,  0, 0, 0
```"
        );
    }
}
