mod utils;
mod macros;
mod lr1;
mod typst_generator;

use std::collections::HashMap;
use std::fmt::Debug;

use utils::*;
use lr1::*;
use typst_generator::*;

fn main() {
    /*
    let grammar = simple_grammar! {
        Goal => Program;
        Program => ;
        Program => Program begin_expr Expr;
        Expr => Expr plus Term;
        Expr => Term;
        Term => Term times Factor;
        Term => Factor;
        Factor => lparen Expr rparen;
        Factor => number;
    };
    */

    let grammar = simple_grammar! {
        /* 0 */ Goal => Expr;
        /* 1 */ Expr => Expr plus Term;
        /* 2 */ Expr => Term;
        /* 3 */ Term => Term star Factor;
        /* 4 */ Term => Factor;
        /* 5 */ Factor => lparen Expr rparen;
        /* 6 */ Factor => number;
    };

    #[derive(Debug, Clone, PartialEq)]
    enum Token {
        Number(i64),
        LParen,
        RParen,
        Plus,
        Star,
    }

    impl InputToken<String> for Token {
        fn kind(&self) -> String {
            match self {
                Self::Number(_) => String::from("number"),
                Token::LParen => String::from("lparen"),
                Token::RParen => String::from("rparen"),
                Token::Plus => String::from("plus"),
                Token::Star => String::from("star"),
            }
        }
    }

    let table = generate_parsing_table(&grammar);
    table.dump(&grammar);

    let res = parse(
        &table,
        rules_callback!(grammar<i64> [
            ([a => a]) { a },
            ([a => a], [*], [b => b]) { a + b },
            ([a => a]) { a },
            ([a => a], [*], [b => b]) { a * b },
            ([a => a]) { a },
            ([*], [a => a], [*]) { a },
            ([*Token::Number(a) => a]) { a }
        ]),
        vec![
            Token::Number(5), Token::Star, Token::Number(6), Token::Plus,
            Token::Number(9), Token::Plus, Token::LParen, Token::Number(3),
            Token::Plus, Token::Number(3), Token::RParen, Token::Star,
            Token::Number(5)
        ].into_iter(),
    );

    println!("{res:?}");

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
}
