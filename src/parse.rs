use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct RipstopParser;

pub fn parse(toparse: &str) {
  let parsed = RipstopParser::parse(Rule::module_declaration, toparse).expect("Parse failed!");
  println!("{:?}", parsed);
}