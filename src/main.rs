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
    let parser = old_typst_grammar! {
        {
            Goal => Expr;
                (a) => a;
            Expr => Expr "+" number;
                (a, _1, b) => "mk_node(ast_node_type.add, span: span_fast_merge(a, b), left: a, right: b)";
            Expr => number;
                (a) => a;
        } where {
            "typst_token_kind.literal_int" => number,
                x => "mk_node(ast_node_type.literal_int, span: x.span, value: x.text)";
            "typst_token_kind.punc_plus" => "+";
        }
    };

    let parser = format!(r#"#import "reflection-lexer.typ": *
#import "reflection-ast.typ": *
#import "reflection-span.typ": *
#let typst_parse = {}"#, parser);

    println!("{}", parser);
    let mut file = std::fs::File::create("parser.typ").unwrap();
    file.write_all(parser.as_bytes()).unwrap();
}
