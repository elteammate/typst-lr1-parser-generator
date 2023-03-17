mod utils;
mod macros;
mod lr1;
mod typst_generator;
mod input;

use std::io::{Read, Write};
use std::fs::File;
use crate::input::typst_grammar;

fn main() {
    // let parser = typst_grammar(include_str!("grammar.typst_gr"));
    let mut file = File::open("src/grammar.typst_gr").unwrap();
    let mut grammar_text = String::new();
    file.read_to_string(&mut grammar_text).unwrap();
    drop(file);
    let parser = typst_grammar(&grammar_text);

    let parser = format!(r#"#import "reflection-lexer.typ": *
#import "reflection-ast.typ": *
#let typst_parse = {{
    let A = ast_node_type
    let T = typst_token_kind
    {}
}}"#, parser);

    println!("{}", parser);

    let mut file = std::fs::File::create("parser.typ").unwrap();
    file.write_all(parser.as_bytes()).unwrap();
}
