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

#[macro_export]
macro_rules! typst_grammar {
    {
        {$(
            $lhs:ident => $($rhs:expr)*; ($($args:expr),*) => $cb:expr;
        )+} where {$(
            $typst_kind:expr => $local_kind:expr $(, $arg:ident => $cast:expr)?;
        )*}
    } => {{
        let non_terminals = vec![$(String::from(stringify!($lhs))),*]
            .into_iter()
            .collect::<std::collections::HashSet<String>>();

        let rules = vec![
            $(
                GrammarRule {
                    lhs: String::from(stringify!($lhs)),
                    rhs: vec![$(String::from(typst_grammar!(__impl_stringify_or_lit $rhs))),*].into_iter().map(|t| {
                        if non_terminals.contains(&t) {
                            GrammarToken::NonTerminal(t)
                        } else {
                            GrammarToken::Terminal(t)
                        }
                    }).collect(),
                }
            ),+
        ];

        let grammar = Grammar::new(rules);
        let table = generate_parsing_table(&grammar);
        let mut rules = grammar.rules.iter();

        generate_typst_parser(
            &table,
            [
                $((
                    typst_grammar!(__impl_stringify_or_lit $typst_kind).to_string(),
                    typst_grammar!(__impl_stringify_or_lit $local_kind).to_string(),
                )),*
            ].into_iter().collect::<HashMap<_, _>>(),
            [
                $((
                    typst_grammar!(__impl_stringify_or_lit $typst_kind).to_string(),
                    typst_grammar!(__impl_if_then_else ($($arg)?)
                        ($(format!(
                                "({}) => {{ {} }}",
                                typst_grammar!(__impl_stringify_or_lit $arg),
                                typst_grammar!(__impl_stringify_or_lit $cast),
                        ))?)
                        ("_=>none".to_string())
                    )
                )),+
            ].into_iter().collect::<HashMap<_, _>>(),
            [
                $((PureRef(rules.next().unwrap()), TypstRule::new(vec![$(
                    typst_grammar!(__impl_stringify_or_lit $args)
                ),*], typst_grammar!(__impl_stringify_or_lit $cb)))),+
            ].into_iter().collect::<HashMap<_, _>>(),
        )
    }};

    (__impl_stringify_or_lit $e:literal) => {
        $e
    };

    (__impl_stringify_or_lit $($e:tt)*) => {
        stringify!($($e)*)
    };

    (__impl_if_then_else () ($($non_empty:tt)*) ($($empty:tt)*)) => {
        $($empty)*
    };

    (__impl_if_then_else ($tt:tt) ($($non_empty:tt)*) ($($empty:tt)*)) => {
        $($non_empty)*
    };
}
