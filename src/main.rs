mod utils;
mod macros;
mod lr1;
mod typst_generator;

use std::collections::HashMap;
use std::io::Write;

use utils::*;
use lr1::*;
use typst_generator::*;

fn main() {
    let parser = typst_grammar! {
        {
            Goal => Expr; (a) => a;
            Expr => Expr "+" number; (a, _1, b) => a + b;
            Expr => number; (a) => a;
        } where {
            "typst_token_kind.literal_int" => number, x => x.text;
            "typst_token_kind.punc_plus" => "+";
        }
    };

    println!("{}", parser);

    let mut file = std::fs::File::create("parser.typ").unwrap();
    file.write_all(parser.as_bytes()).unwrap();

    /*
    let grammar = simple_grammar! {
        Goal => Expr;
        Expr => Expr plus Term;
        Expr => Term;
        Term => Term star Factor;
        Term => Factor;
        Factor => lparen Expr rparen;
        Factor => number;
    };

    let table = generate_parsing_table(&grammar);
    table.dump(&grammar);

    let typst_parser = generate_typst_parser(
        &table,
        [
            ("token_type.number".to_string(), "number".to_string()),
            ("token_type.lparen".to_string(), "lparen".to_string()),
            ("token_type.rparen".to_string(), "rparen".to_string()),
            ("token_type.plus".to_string(), "plus".to_string()),
            ("token_type.star".to_string(), "star".to_string()),
        ].into_iter().collect::<HashMap<_, _>>(),
        [
            (PureRef(&grammar.rules[0]), TypstRule::new(vec!["a"], "a")),
            (PureRef(&grammar.rules[1]), TypstRule::new(vec!["a", "_", "b"], "a + b")),
            (PureRef(&grammar.rules[2]), TypstRule::new(vec!["a"], "a")),
            (PureRef(&grammar.rules[3]), TypstRule::new(vec!["a", "_", "b"], "a * b")),
            (PureRef(&grammar.rules[4]), TypstRule::new(vec!["a"], "a")),
            (PureRef(&grammar.rules[5]), TypstRule::new(vec!["_", "a", "_"], "a")),
            (PureRef(&grammar.rules[6]), TypstRule::new(vec!["a"], "a")),
        ].into_iter().collect::<HashMap<_, _>>(),
    );

    println!("{}", typst_parser);
     */
}
