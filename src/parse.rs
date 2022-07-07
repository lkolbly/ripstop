use crate::ast::Type;
use crate::ast::Node;

use pest::iterators::Pair;
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct RipstopParser;

pub fn parse(toparse: &str) -> Node {
    let parsed = RipstopParser::parse(Rule::module_declaration, toparse).expect("Parse failed!").next().unwrap();
    return parse_value(parsed);

    fn parse_value(pair: Pair<Rule>) -> Node {
        let rule = pair.as_rule();
        let mut inner_rules = pair.into_inner();
        match rule {
            Rule::module_declaration => {
                Node::ModuleDeclaration {
                    id: inner_rules.next().unwrap().as_str().to_string(),
                    in_values: parse_variable_declarations(inner_rules.next().unwrap()),
                    out_values: parse_variable_declarations(inner_rules.next().unwrap()),
                    children: parse_block(inner_rules.next().unwrap())
                }
            },
            Rule::expression => parse_value(inner_rules.next().unwrap()),
            Rule::assignment => Node::Assign {
                    lhs: Box::new(parse_value(inner_rules.next().unwrap())),
                    rhs: Box::new(parse_value(inner_rules.next().unwrap()))
                },
            Rule::indexed_variable => Node::VariableReference {
                    var_id: inner_rules.next().unwrap().as_str().to_string(),
                    t_offset: parse_t_offset(inner_rules.next().unwrap())
                },
            Rule::unary_operation => match inner_rules.next().unwrap().into_inner().next().unwrap().as_rule() {
                Rule::bitwise_inverse => Node::BitwiseInverse {
                    child: Box::new(parse_value(inner_rules.next().unwrap()))
                },
                _ => unreachable!()
            },
            Rule::statement => parse_value(inner_rules.next().unwrap()),
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
        return (parse_type(inner_rules.next().unwrap()), inner_rules.next().unwrap().as_str().to_string());
    }

    fn parse_type(pair: Pair<Rule>) -> Type {
        match pair.into_inner().next().unwrap().as_rule() {
            Rule::bit_type => Type::Bit,
            _ => unreachable!()
        }
    }

    fn parse_t_offset(pair: Pair<Rule>) -> u64 {
        return match pair.into_inner().next() {
            Some(int_pair) => int_pair.as_str().parse().unwrap(),
            None => 0
        }
    }
}
