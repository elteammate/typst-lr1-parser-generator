use std::collections::HashMap;
use lalrpop_util;
use lalrpop_util::lalrpop_mod;
use crate::lr1::*;
use crate::typst_generator::{generate_typst_parser, TypstRule};
use crate::utils::PureRef;

mod ast;

lalrpop_mod!(parser);

pub fn typst_grammar(input: &str) -> String {
    let input = input.to_string();

    let parser = parser::GrammarParser::new();
    let grammar = parser.parse(&input);

    let raw_grammar = match grammar.clone() {
        Ok(term) => term,
        Err(lalrpop_util::ParseError::UnrecognizedToken { token: e, .. }) => {
            let x = e.0;
            println!("Token: {}`{}`{}", &input[x.checked_sub(100).unwrap_or(0)..x], &input[x..x + 1], &input[x + 1..x + 100]);
            panic!("Unrecognized token: {:?}", grammar)
        }
        Err(lalrpop_util::ParseError::InvalidToken { location: x, .. }) => {
            println!("Token: {}`{}`{}", &input[x.checked_sub(100).unwrap_or(0)..x], &input[x..x + 1], &input[x + 1..x + 100]);
            panic!("Unrecognized token: {:?}", grammar)
        }
        Err(e) => {
            panic!("Error: {:?}", e)
        }
    };

    let non_terminals = raw_grammar.rules.iter().map(|r| r.lhs.clone())
        .collect::<std::collections::HashSet<String>>();

    let (rules, actions): (Vec<GrammarRule<String, String>>, Vec<_>) = raw_grammar.rules.iter().map(|r|
        r.alternatives.iter().map(|a| {
            let rhs = a.rhs.iter().map(|t| {
                if non_terminals.contains(t) {
                    GrammarToken::NonTerminal(t.clone())
                } else {
                    GrammarToken::Terminal(t.clone())
                }
            }).collect();

            (GrammarRule {
                lhs: r.lhs.clone(),
                rhs,
            }, &a.action.0)
        })).flatten().unzip();

    let grammar = Grammar::new(rules);
    let table = generate_parsing_table(&grammar);

    let callbacks: HashMap<PureRef<GrammarRule<String, String>>, TypstRule> =
        actions.iter().enumerate().map(|(i, a)| (
            PureRef(&grammar.rules[i]),
            TypstRule::new_raw(&a, grammar.rules[i].rhs.len())
        ))
        .collect();

    generate_typst_parser(
        &table,
        raw_grammar.casts.iter().map(|c| (c.typst_kind.clone().0, c.local_kind.clone())).collect(),
        raw_grammar.casts.iter().map(|c| (c.typst_kind.clone().0, c.cast.clone().0)).collect(),
        callbacks,
    )
}
