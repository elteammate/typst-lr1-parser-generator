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
}

pub fn generate_typst_parser<T: Terminal, NT: NonTerminal>(
    parsing_table: &ParsingTable<T, NT>,
    token_kind_mapping: HashMap<String, T>,
    callbacks: HashMap<PureRef<GrammarRule<T, NT>>, TypstRule>,
) -> String {
    let mut terminals = token_kind_mapping.values().collect::<Vec<_>>();
    hash_dedup(&mut terminals);
    let terminals_pos = terminals.iter().enumerate().map(|(i, &x)| (x, i)).collect::<HashMap<_, _>>();

    let token_mapping_def = format!(
        "let token_mapping = ({})",
        terminals.iter().enumerate().map(|(i, x)| format!("\"{}\":{}", x, i)).collect::<Vec<_>>().join(",")
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
            TokenOrEof::Terminal(t) => terminals_pos[t],
            TokenOrEof::NonTerminal(t) => non_terminals_pos[t],
            TokenOrEof::EOF => eof_id,
        };

        let action = match action {
            Action::Shift(next_state) => *next_state as i32,
            Action::Reduce(rule) => -(rule_pos[rule] as i32) - 1,
            Action::Accept => total_states as i32,
            Action::Conflict(_) => panic!("Conflict in parser table"),
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
        r#"
#let parse(tokens) = {{
    {token_mapping_def}
    {callbacks_def}
    {table_def}
    {arg_count_def}
    {goto_index_def}
    let stack = (0, )
    let ast_stack = ()
    let cur_token = 0
    while true {{
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
            ast_stack.push(if "value" in tokens.at(cur_token)
                {{ tokens.at(cur_token).value }} else {{ none }})
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
                (:).at("Expected shift action")
            }}
        }} else {{
            (:).at("Parsing error at state: " + repr(stack) + " and token: " +
                repr(if cur_token < tokens.len() {{ tokens.at(cur_token) }} else {{"EOF"}}))
        }}
    }}
}}
        "#
    )
}
