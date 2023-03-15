use lalrpop_util;
use lalrpop_util::lalrpop_mod;

mod ast;

lalrpop_mod!(parser);

pub fn typst_grammar(input: &str) -> String {
    let input = input.to_string();

    let parser = parser::GrammarParser::new();
    let grammar = parser.parse(&input);

    let grammar = match grammar.clone() {
        Ok(term) => term,
        Err(lalrpop_util::ParseError::UnrecognizedToken {token: e, .. }) => {
            println!("Token: {}`{}`{}", &input[..e.0], &input[e.0..e.2], &input[e.2..]);
            panic!("Unrecognized token: {:?}", grammar)
        },
        Err(e) => {
            panic!("Error: {:?}", e)
        },
    };

    let non_terminals = grammar.rules.iter().map(|r| r.lhs.clone())
        .collect::<std::collections::HashSet<String>>();

    let res = format!("{:#?}", grammar);

    let raw_rules: Vec<(String, Vec<String>)> = grammar.rules.into_iter().map(|r| {
        r.alternatives.iter().map(|a| {
            (r.lhs.clone(), a.rhs.clone())
        }).collect::<Vec<_>>()
    }).flatten().collect();

    "".to_string()
}
