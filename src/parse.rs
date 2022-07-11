use crate::ast::Node;
use crate::ast::Type;

use lazy_static::lazy_static;

use pest::iterators::Pair;
use pest::prec_climber::Assoc;
use pest::prec_climber::Operator;
use pest::prec_climber::PrecClimber;
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct RipstopParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = PrecClimber::new(vec![
        Operator::new(Rule::addition, Assoc::Left) | Operator::new(Rule::subtraction, Assoc::Left)
    ]);
}

pub fn parse(toparse: &str) -> Node {
    let parsed = RipstopParser::parse(Rule::module_declaration, toparse)
        .expect("Parse failed!")
        .next()
        .unwrap();
    return parse_value(parsed);

    fn parse_value(pair: Pair<Rule>) -> Node {
        let rule = pair.as_rule();
        let mut inner_rules = pair.into_inner();
        match rule {
            Rule::module_declaration => Node::ModuleDeclaration {
                id: inner_rules.next().unwrap().as_str().to_string(),
                in_values: parse_variable_declarations(inner_rules.next().unwrap()),
                out_values: parse_variable_declarations(inner_rules.next().unwrap()),
                children: parse_block(inner_rules.next().unwrap()),
            },
            Rule::assignment => Node::Assign {
                lhs: Box::new(parse_value(inner_rules.next().unwrap())),
                rhs: Box::new(parse_value(inner_rules.next().unwrap())),
            },
            Rule::indexed_variable => Node::VariableReference {
                var_id: inner_rules.next().unwrap().as_str().to_string(),
                t_offset: parse_t_offset(inner_rules.next().unwrap()),
            },
            Rule::unary_operation => match inner_rules
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .as_rule()
            {
                Rule::bitwise_inverse => Node::BitwiseInverse {
                    child: Box::new(parse_value(inner_rules.next().unwrap())),
                },
                _ => unreachable!(),
            },
            Rule::binary_operation => {
                PREC_CLIMBER.climb(inner_rules, parse_value, |lhs, op, rhs| {
                    match op.as_rule() {
                        Rule::addition => Node::Add {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        Rule::subtraction => Node::Subtract {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!(),
                    }
                })
            }
            Rule::variable_declaration => Node::VariableDeclaration {
                var_type: parse_type(inner_rules.next().unwrap()),
                var_id: inner_rules.next().unwrap().as_str().to_string(),
            },
            _ => {
                println!("Unimplement rule '{:?}'", rule);
                todo!();
            }
        }
    }

    fn parse_block(pair: Pair<Rule>) -> Vec<Node> {
        return pair.into_inner().map(parse_value).collect();
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
        return sign * int;
    }
}
