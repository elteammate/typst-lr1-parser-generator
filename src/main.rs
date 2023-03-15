mod utils;
mod macros;
mod lr1;
mod typst_generator;
mod input;

use std::collections::HashMap;
use std::io::Write;

use utils::*;
use lr1::*;
use typst_generator::*;
use crate::input::typst_grammar;

fn main() {
    let parser = typst_grammar(include_str!("grammar.typst_gr"));

    let parser = format!(r#"#import "reflection-lexer.typ": *
#import "reflection-ast.typ": *
#import "reflection-span.typ": *
#let typst_parse = {{
    let A = ast_node_type
    let T = typst_token_kind
    {}
}}"#, parser);

    println!("{}", parser);
    let mut file = std::fs::File::create("parser.typ").unwrap();
    file.write_all(parser.as_bytes()).unwrap();
}
