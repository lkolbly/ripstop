use crate::ast::ASTNode;
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

    fn parse_value(tree: &mut Tree<ASTNode>, pair: Pair<Rule>) -> Option<NodeId> {
        let rule = pair.as_rule();
        let mut inner_rules = pair.into_inner();

        let node_data = match rule {
            Rule::module_declaration => Some(ASTNode::ModuleDeclaration {
                id: inner_rules.next().unwrap().as_str().to_string(),
                in_values: parse_variable_declarations(inner_rules.next().unwrap()),
                out_values: parse_variable_declarations(inner_rules.next().unwrap()),
            }),
            Rule::assignment => Some(ASTNode::Assign),
            Rule::indexed_variable => Some(ASTNode::VariableReference {
                var_id: inner_rules.next().unwrap().as_str().to_string(),
                t_offset: parse_t_offset(inner_rules.next().unwrap()),
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
                    Rule::bitwise_inverse => ASTNode::BitwiseInverse,
                    _ => unreachable!(),
                },
            ),
            Rule::binary_operation => None,
            Rule::variable_declaration => Some(ASTNode::VariableDeclaration {
                var_type: parse_type(inner_rules.next().unwrap()),
                var_id: inner_rules.next().unwrap().as_str().to_string(),
            }),
            Rule::EOI => None,
            _ => {
                println!("Unimplement rule '{:?}'", rule);
                todo!();
            }
        };

        if let Some(data) = node_data {
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
                    let data = match op.as_rule() {
                        Rule::addition => ASTNode::Add,
                        Rule::subtraction => ASTNode::Subtract,
                        _ => unreachable!(),
                    };
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
        match pair.into_inner().next().unwrap().as_rule() {
            Rule::bit_type => Type::Bit,
            _ => unreachable!(),
        }
    }

    fn parse_t_offset(pair: Pair<Rule>) -> i64 {
        let mut inner_rules = pair.into_inner();
        let sign = match inner_rules.next() {
            Some(sign_pair) => match sign_pair.as_rule() {
                Rule::addition => -1,
                Rule::subtraction => 1,
                _ => unreachable!(),
            },
            None => {
                return 0;
            }
        };
        let int: i64 = inner_rules.next().unwrap().as_str().parse().unwrap();
        sign * int
    }
}
