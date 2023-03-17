use std::collections::HashMap;
use crate::lr1::*;
use crate::utils::*;

pub struct TypstRule {
    callback: String,
    arg_count: usize,
}

impl TypstRule {
    pub fn new(args: Vec<&str>, implementation: &str) -> Self {
        let args = args.into_iter().enumerate().map(|(i, x)| if x == "_" {
            format!("__{i}")
        } else {
            x.to_string()
        }).collect::<Vec<_>>();

        TypstRule {
            callback: format!("({}) => {{{}}}", args.join(","), implementation),
            arg_count: args.len(),
        }
    }

    pub fn new_raw(implementation: &str, arg_count: usize) -> Self {
        TypstRule {
            callback: implementation.to_string(),
            arg_count: arg_count,
        }
    }
}

pub fn generate_typst_parser<T: Terminal, NT: NonTerminal>(
    parsing_table: &ParsingTable<T, NT>,
    token_kind_mapping: HashMap<String, T>,
    casts: HashMap<String, String>,
    callbacks: HashMap<PureRef<GrammarRule<T, NT>>, TypstRule>,
) -> String {
    let terminals_typst_exprs = token_kind_mapping.keys().collect::<Vec<_>>();
    let mut terminals = terminals_typst_exprs.iter().map(|&x| &token_kind_mapping[x]).collect::<Vec<_>>();
    hash_dedup(&mut terminals);
    let terminals_pos = terminals.iter().enumerate().map(|(i, &x)| (x, i)).collect::<HashMap<_, _>>();

    let terminals_filling_string_case = format!(
        "let res = (:); {} res",
        terminals_typst_exprs.iter().enumerate().map(|(i, &x)|
            format!("res.insert({}, {}); ", x, i)
        ).collect::<Vec<_>>().join(""),
    );

    let enum_name_speculation = casts.iter().next().unwrap().0.split('.').next().unwrap();

    let terminals_filling_int_case = format!(
        "let res = range({}.len()); {} res",
        enum_name_speculation,
        terminals_typst_exprs.iter().enumerate().map(|(i, &x)|
            format!("res.at({}) = {}; ", x, i)
        ).collect::<Vec<_>>().join(""),
    );

    let token_mapping_def = format!(
        "let token_mapping = if type({}) == \"string\" {{{}}} else {{{}}}",
        terminals_typst_exprs.iter().next().unwrap(),
        terminals_filling_string_case,
        terminals_filling_int_case,
    );

    let indexed_callbacks = callbacks.iter().map(|(rule, callback)| (rule, callback)).collect::<Vec<_>>();
    let rule_pos = indexed_callbacks.iter().enumerate().map(|(i, (&r, _))| (r, i)).collect::<HashMap<_, _>>();
    let callbacks_strs = indexed_callbacks.iter().map(|(_, callback)| callback).collect::<Vec<_>>();

    let callbacks_def = format!(
        "let callbacks = ({})",
        callbacks_strs.iter().map(|x| format!("({}),", x.callback)).rev().collect::<Vec<_>>().join("")
    );

    let arg_count_def = format!(
        "let arg_count = ({})",
        callbacks_strs.iter().map(|x| format!("{},", x.arg_count)).rev().collect::<Vec<_>>().join("")
    );

    let cast_table = format!(
        "let cast_table = ({})",
        terminals_typst_exprs.iter().map(|t| casts[&t.to_string()].clone() + ",").collect::<Vec<_>>().join("")
    );

    let total_states = parsing_table.table.keys().map(|(state, _)| *state).max().unwrap() + 1;
    let mut non_terminals = indexed_callbacks.iter().map(|(r, _)| r.lhs.clone()).collect::<Vec<_>>();
    hash_dedup(&mut non_terminals);

    let eof_id = terminals.len();
    let non_terminals_offset = terminals.len() + 1;
    let non_terminals_pos = non_terminals.iter().enumerate()
        .map(|(i, x)| (x, non_terminals_offset + i)).collect::<HashMap<_, _>>();

    let goto_index_def = format!(
        "let goto_index = ({})",
        indexed_callbacks.iter().map(|(r, _)|
            (non_terminals_pos[&r.lhs]).to_string() + ","
        ).rev().collect::<Vec<_>>().join("")
    );

    let mut table_int_repr = vec![vec![0; terminals.len() + 1 + non_terminals.len()]; total_states];

    for ((state, token), action) in parsing_table.table.iter() {
        let token_pos = match token {
            TokenOrEof::Terminal(t) => terminals_pos.get(t).unwrap_or_else(|| panic!("Can't get terminal {}", t)).clone(),
            TokenOrEof::NonTerminal(t) => non_terminals_pos[t],
            TokenOrEof::EOF => eof_id,
        };

        let action = match action {
            Action::Shift(next_state) => *next_state as i32,
            Action::Reduce(rule) => -(rule_pos[rule] as i32) - 1,
            Action::Accept => total_states as i32,
            Action::Conflict(_) => {
                println!("Conflict in parser table!");
                println!("State: {}", state);
                println!("Token: {:?}", token);
                println!("Action: {:?}", action);
                panic!("Conflict in parser table")
            },
        };

        table_int_repr[*state][token_pos] = action;
    }

    let table_def = format!(
        "let table = ({})",
        table_int_repr.iter().map(|row| format!(
            "({})",
            row.iter().map(|x| x.to_string() + ",").collect::<Vec<_>>().join("")
        )).collect::<Vec<_>>().join(",")
    );

    format!(
        r#"(tokens) => {{
    {token_mapping_def}
    {callbacks_def}
    {table_def}
    {arg_count_def}
    {goto_index_def}
    {cast_table}
    let stack = (0, )
    let ast_stack = ()
    let cur_token = 0
    for i in range(9999) {{ for j in range(9999) {{
        let state = stack.last()
        let terminal = if cur_token < tokens.len() {{
            token_mapping.at(tokens.at(cur_token).kind)
        }} else {{
            {eof_id}
        }}
        let action = table.at(state).at(terminal)
        if action == {total_states} {{
            assert(ast_stack.len() == 1)
            return ast_stack.first()
        }} else if action > 0 {{
            stack.push(action)
            ast_stack.push(cast_table.at(terminal)(tokens.at(cur_token)))
            cur_token += 1
        }} else if action < 0 {{
            let rhs = ()
            for _ in range(arg_count.at(action)) {{
                let _ = stack.pop()
                rhs.push(ast_stack.pop())
            }}
            let rule = callbacks.at(action)
            ast_stack.push(rule(..rhs.rev()))
            let goto_state = table.at(stack.last()).at(goto_index.at(action))
            if goto_state > 0 {{
                stack.push(goto_state)
            }} else {{
                panic("Expected shift action")
            }}
        }} else {{
            panic("Parsing error at state: " + repr(stack) + " and token: " +
                repr(if cur_token < tokens.len() {{ tokens.at(cur_token) }} else {{"EOF"}})
                + " at: " + repr(cur_token)
            )
        }}
    }} }}
    panic("too complex")
}}"#
    )
}
