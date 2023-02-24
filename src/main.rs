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

    let parser = format!(r#"#import "reflection-lexer.typ": *
#import "reflection-ast.typ": *
#let typst_parse = {}"#, parser);

    println!("{}", parser);
    let mut file = std::fs::File::create("parser.typ").unwrap();
    file.write_all(parser.as_bytes()).unwrap();
}
