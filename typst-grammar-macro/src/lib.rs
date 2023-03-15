use proc_macro::{TokenStream};
use lalrpop_util;
use quote::quote;

mod ast;

mod parser {
    include!(concat!(env!("OUT_DIR"), "/parser.rs"));
}

#[proc_macro]
pub fn typst_grammar(input: TokenStream) -> TokenStream {
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

    quote!({
        let rules = vec![#(
            GrammarRule {
                lhs: String::from(#raw_rules.0),
                rhs: vec![#(#raw_rules.1),*].into_iter().map(|t| {
                    if non_terminals.contains(&t) {
                        GrammarToken::NonTerminal(t)
                    } else {
                        GrammarToken::Terminal(t)
                    }
                }).collect::<Vec<_>>(),
            }
        ),+];
    }).into()
}
