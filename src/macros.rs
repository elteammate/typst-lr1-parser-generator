#[macro_export]
macro_rules! simple_grammar {
    {$($lhs:ident => $($rhs:ident)*;)+} => {
        simple_grammar!($($lhs => $($rhs)*: { () }; )+)
    };

    ($($lhs:ident => $($rhs:ident)*: { $cb:expr }; )+) => {{
        let non_terminals = vec![$(String::from(stringify!($lhs))),*]
            .into_iter()
            .collect::<std::collections::HashSet<String>>();

        let rules = vec![
            $(
                GrammarRule {
                    lhs: String::from(stringify!($lhs)),
                    rhs: vec![$(String::from(stringify!($rhs))),*].into_iter().map(|t| {
                        if non_terminals.contains(&t) {
                            GrammarToken::NonTerminal(t)
                        } else {
                            GrammarToken::Terminal(t)
                        }
                    }).collect(),
                }
            ),+
        ];
        Grammar::new(rules)
    }};
}

#[macro_export]
macro_rules! rules_callback {
    ($grammar:ident<$A:ty> [
        $(($([$($tt:tt)+]),*) $cb:block),+
    ]) => {{
        let mut rules = $grammar.rules.iter();

        vec![$((
            PureRef(rules.next().unwrap()),
            (|args: Vec<InputTokenOrAst<_, _, $A>>| {
                let mut args = args.into_iter();
                $(
                    rules_callback!(__impl (args.next()) $($tt)+);
                )*
                $cb
            }) as fn(Vec<InputTokenOrAst<_, _, $A>>) -> $A
        )),+].into_iter().collect::<HashMap<_, _>>()
    }};

    (__impl ($src:expr) * $pat:pat => $name:ident) => {
        let $name = {
            let t = $src;
            match t {
                Some(InputTokenOrAst::InputToken($pat, _)) => $name,
                _ => panic!("Expected token of type {}, found {:?}", stringify!($pat), t),
            }
        };
    };

    (__impl ($src:expr) *) => {
        let _ = {
            let t = $src;
            match t {
                Some(InputTokenOrAst::InputToken(_, _)) => (),
                _ => panic!("Expected input token of any kind, found {:?}", t),
            }
        };
    };

    (__impl ($src:expr) $pat:pat => $name:ident) => {
        let $name = {
            let t = $src;
            match t {
                Some(InputTokenOrAst::Ast($pat)) => $name,
                _ => panic!("Expected AST of type {}, found {:?}", stringify!($pat), t),
            }
        };
    };
}
