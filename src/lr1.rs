use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use ansi_term::Colour;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use crate::utils::{hash_dedup, PureRef};

pub trait Terminal: Debug + Hash + Eq + Clone + Display {}

pub trait NonTerminal: Debug + Hash + Eq + Clone + Display {}

impl Terminal for String {}

impl NonTerminal for String {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TerminalOrEof<T: Terminal> {
    Terminal(T),
    EOF,
}

impl<T: Terminal> TerminalOrEof<T> {
    fn to_token_or_eof<NT: NonTerminal>(self) -> TokenOrEof<T, NT> {
        match self {
            TerminalOrEof::Terminal(t) => TokenOrEof::Terminal(t),
            TerminalOrEof::EOF => TokenOrEof::EOF,
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum GrammarToken<T: Terminal, NT: NonTerminal> {
    Terminal(T),
    NonTerminal(NT),
}

impl<T: Terminal, NT: NonTerminal> GrammarToken<T, NT> {
    fn to_token_or_eof(self) -> TokenOrEof<T, NT> {
        match self {
            GrammarToken::Terminal(t) => TokenOrEof::Terminal(t),
            GrammarToken::NonTerminal(nt) => TokenOrEof::NonTerminal(nt),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum TokenOrEof<T: Terminal, NT: NonTerminal> {
    Terminal(T),
    NonTerminal(NT),
    EOF,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct GrammarRule<T: Terminal, NT: NonTerminal> {
    pub lhs: NT,
    pub rhs: Vec<GrammarToken<T, NT>>,
}

const RULE_ALIGNMENT: usize = 14;

pub struct Grammar<T: Terminal, NT: NonTerminal> {
    pub rules: Vec<GrammarRule<T, NT>>,
    pub rules_by_lhs: HashMap<NT, Vec<usize>>,
}

impl<T: Terminal, NT: NonTerminal> Grammar<T, NT> {
    pub fn new(rules: Vec<GrammarRule<T, NT>>) -> Self {
        let mut rules_by_lhs = HashMap::new();

        for (i, rule) in rules.iter().enumerate() {
            rules_by_lhs.entry(rule.lhs.clone()).or_insert_with(Vec::new).push(i);
        }

        Self {
            rules,
            rules_by_lhs,
        }
    }

    fn dump(&self) {
        println!("| {}", Colour::Green.paint("Grammar begin"));
        for rule in &self.rules {
            print!("| {} => ", colored_non_terminal_align(&rule.lhs, RULE_ALIGNMENT));
            for token in &rule.rhs {
                print!("{} ", colored_token(&token));
            }
            println!();
        }
        println!("| {}\n", Colour::Green.paint("Grammar end"));
    }

    fn start_symbol(&self) -> NT {
        self.rules[0].lhs.clone()
    }
}

fn colored_terminal(terminal: &impl Terminal) -> String {
    Colour::Blue.paint(format!(r#""{}""#, terminal)).to_string()
}

fn colored_non_terminal(non_terminal: &impl NonTerminal) -> String {
    Colour::Red.paint(format!("<{}>", non_terminal)).to_string()
}

fn colored_non_terminal_align(non_terminal: &impl NonTerminal, align: usize) -> String {
    Colour::Red.paint(format!("{:>align$}", format!("<{}>", non_terminal))).to_string()
}

fn colored_token(token: &GrammarToken<impl Terminal, impl NonTerminal>) -> String {
    match token {
        GrammarToken::Terminal(terminal) => colored_terminal(terminal),
        GrammarToken::NonTerminal(non_terminal) => colored_non_terminal(non_terminal),
    }
}

fn colored_terminal_or_eof(token: &TerminalOrEof<impl Terminal>) -> String {
    match token {
        TerminalOrEof::Terminal(terminal) => colored_terminal(terminal),
        TerminalOrEof::EOF => Colour::Cyan.paint("$").to_string(),
    }
}

fn colored_token_or_eof(token: &TokenOrEof<impl Terminal, impl NonTerminal>, width: usize) -> String {
    match token {
        TokenOrEof::Terminal(terminal) => Colour::Blue.paint(
            format!("{:^width$}", terminal.to_string()[..width.min(terminal.to_string().len())].to_string())
        ).to_string(),

        TokenOrEof::NonTerminal(non_terminal) => Colour::Red.paint(
            format!("{:^width$}", non_terminal.to_string()[..width.min(non_terminal.to_string().len())].to_string())
        ).to_string(),

        TokenOrEof::EOF => Colour::Cyan.paint(format!("{:^width$}", "$")).to_string(),
    }
}

#[derive(Debug)]
struct FirstSet<T: Terminal, NT: NonTerminal> {
    pub first: HashMap<NT, Vec<T>>,
    phantom_grammar: PhantomData<Grammar<T, NT>>,
}

impl<'a, T: Terminal, NT: NonTerminal> FirstSet<T, NT> {
    fn new(grammar: &Grammar<T, NT>) -> Self {
        let mut first = grammar.rules_by_lhs.keys().map(|nt| {
            (nt.clone(), Vec::new())
        }).collect::<HashMap<NT, Vec<T>>>();

        loop {
            let mut changed = false;

            let first_clone = first.clone();

            let mut insert_into_first = |lhs, terminal: &T| {
                if !first[lhs].contains(terminal) {
                    first.get_mut(lhs).unwrap().push(terminal.clone());
                    changed = true;
                }
            };

            for rule in &grammar.rules {
                match rule.rhs.first() {
                    None => (),
                    Some(GrammarToken::Terminal(terminal)) => {
                        insert_into_first(&rule.lhs, terminal);
                    }
                    Some(GrammarToken::NonTerminal(non_terminal)) => {
                        for terminal in &first_clone[non_terminal] {
                            insert_into_first(&rule.lhs, terminal);
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }

        FirstSet { first, phantom_grammar: PhantomData }
    }

    fn dump(&self) {
        println!("| {}", Colour::Green.paint("First set begin"));
        for (lhs, terminals) in &self.first {
            print!("| {} => ", colored_non_terminal_align(lhs, RULE_ALIGNMENT));
            for terminal in terminals {
                print!("{} ", colored_terminal(terminal));
            }
            println!();
        }
        println!("| {}\n", Colour::Green.paint("First set end"));
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
struct Position<'a, T: Terminal, NT: NonTerminal> {
    rule: PureRef<'a, GrammarRule<T, NT>>,
    dot: usize,
    lookahead: TerminalOrEof<T>,
}

const DOT: &str = "\u{25cf}";

impl<'a, T: Terminal, NT: NonTerminal> Position<'a, T, NT> {
    fn new(rule: PureRef<'a, GrammarRule<T, NT>>, dot: usize, lookahead: TerminalOrEof<T>) -> Self {
        Self { rule, dot, lookahead }
    }

    fn locus(&self) -> Option<&GrammarToken<T, NT>> {
        self.rule.rhs.get(self.dot)
    }

    fn after_locus(&self) -> Option<&GrammarToken<T, NT>> {
        self.rule.rhs.get(self.dot + 1)
    }

    fn to_colored_string(&self) -> String {
        let mut s = String::new();
        let dot = Colour::Yellow.bold().paint(DOT).to_string();
        s.push_str(&colored_non_terminal_align(&self.rule.lhs, RULE_ALIGNMENT));
        s.push_str(" => ");
        for (i, token) in self.rule.rhs.iter().enumerate() {
            if i == self.dot {
                s.push_str(&dot);
                s.push(' ');
            }
            s.push_str(&colored_token(token));
            s.push(' ');
        }
        if self.dot == self.rule.rhs.len() {
            s.push_str(&dot);
        }
        s.push_str(&format!(" | {}", colored_terminal_or_eof(&self.lookahead)));
        s
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
struct ItemSet<'a, T: Terminal, NT: NonTerminal> {
    positions: Vec<Position<'a, T, NT>>,
}

impl<'a, T: Terminal, NT: NonTerminal> ItemSet<'a, T, NT> {
    fn new(
        mut positions: Vec<Position<'a, T, NT>>,
        grammar: &'a Grammar<T, NT>,
        first_set: &FirstSet<T, NT>,
    ) -> Self {
        loop {
            let mut changed = false;

            let positions_clone = positions.clone();

            let mut insert_position = |position: Position<'a, T, NT>| {
                if !positions.contains(&position) {
                    positions.push(position);
                    changed = true;
                }
            };

            for position in &positions_clone {
                if let Some(GrammarToken::NonTerminal(non_terminal)) = position.locus() {
                    for rule in grammar.rules_by_lhs[non_terminal].iter() {
                        let mut lookahead = vec![];
                        match position.after_locus() {
                            None => {
                                lookahead.push(position.lookahead.clone());
                            }
                            Some(GrammarToken::Terminal(terminal)) => {
                                lookahead.push(TerminalOrEof::Terminal(terminal.clone()));
                            }
                            Some(GrammarToken::NonTerminal(non_terminal)) => {
                                for terminal in &first_set.first[non_terminal] {
                                    lookahead.push(TerminalOrEof::Terminal(terminal.clone()));
                                }
                            }
                        }

                        for terminal in lookahead {
                            insert_position(Position::new(PureRef(&grammar.rules[*rule]), 0, terminal));
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }

        Self { positions }
    }

    fn dump(&self, number: usize) {
        println!("| {}", Colour::Green.paint(format!("Item set {} begin", number)));
        for position in &self.positions {
            println!("| {}", position.to_colored_string());
        }
        println!("| {}\n", Colour::Green.paint(format!("Item set {} end", number)));
    }
}

fn goto<'a, T: Terminal, NT: NonTerminal>(
    item_set: &ItemSet<'a, T, NT>,
    grammar: &'a Grammar<T, NT>,
    first_set: &FirstSet<T, NT>,
    transition: &GrammarToken<T, NT>,
) -> ItemSet<'a, T, NT> {
    let mut positions = vec![];

    for position in &item_set.positions {
        if let Some(locus) = position.locus() {
            if locus == transition {
                positions.push(Position::new(
                    position.rule,
                    position.dot + 1,
                    position.lookahead.clone(),
                ));
            }
        }
    }

    ItemSet::new(positions, grammar, first_set)
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum Action<'a, T: Terminal, NT: NonTerminal> {
    Shift(usize),
    Reduce(PureRef<'a, GrammarRule<T, NT>>),
    Conflict(Box<(Self, Self)>),
    Accept,
}

pub struct ParsingTable<'a, T: Terminal, NT: NonTerminal> {
    pub table: HashMap<(usize, TokenOrEof<T, NT>), Action<'a, T, NT>>,
}

impl<'a, T: Terminal, NT: NonTerminal> ParsingTable<'a, T, NT> {
    pub fn dump(&self, grammar: &'a Grammar<T, NT>) {
        const ACTION_NUMBER_WIDTH: usize = 4;
        const ACTION_WIDTH: usize = ACTION_NUMBER_WIDTH + 1;
        const TOKEN_WIDTH: usize = ACTION_WIDTH + 2;

        let mut all_tokens = self.table.keys().map(|(_, token)| token)
            .collect::<HashSet<_>>().into_iter().collect::<Vec<_>>();

        let mut states = self.table.keys().map(|(state, _)| state).collect::<Vec<_>>();

        states.sort();
        states.dedup();

        all_tokens.sort_by_key(|token| match token {
            TokenOrEof::Terminal(x) => (0, x.to_string()),
            TokenOrEof::EOF => (1, "$".to_string()),
            TokenOrEof::NonTerminal(x) => (2, x.to_string()),
        });

        println!("| {}", Colour::Green.paint("Parsing table begin"));

        let colored_action = |action: Action<T, NT>| {
            match action {
                Action::Shift(state) => Colour::Blue.paint(format!("s{:<ACTION_NUMBER_WIDTH$}", state)),
                Action::Reduce(rule) => Colour::Yellow.paint(format!(
                    "r{:<ACTION_NUMBER_WIDTH$}",
                    grammar.rules.iter().position(|r| PureRef(r) == rule).unwrap()
                )),
                Action::Conflict(_) => Colour::Red.bold().paint("CONFL"),
                Action::Accept => Colour::Green.bold().paint("ACCPT"),
            }
        };

        print!("| {:<ACTION_NUMBER_WIDTH$} |", "");
        for token in &all_tokens {
            print!("{}|", colored_token_or_eof(token, TOKEN_WIDTH));
        }
        println!();

        for state in states {
            print!("| {} |", Colour::Green.paint(format!("{:<ACTION_NUMBER_WIDTH$}", state)));
            for token in &all_tokens {
                let action = self.table.get(&(state.clone(), (*token).clone()));
                if let Some(action) = action {
                    print!(" {} |", colored_action(action.clone()));
                } else {
                    print!(" {:<ACTION_WIDTH$} |", "");
                }
            }
            println!();
        }

        println!("| {}", Colour::Green.paint("Parsing table end"));
    }
}

pub fn generate_parsing_table<'a, T: Terminal, NT: NonTerminal>(
    grammar: &'a Grammar<T, NT>
) -> ParsingTable<'a, T, NT> {
    grammar.dump();

    let first_set = FirstSet::new(&grammar);
    first_set.dump();

    let start_position = Position::new(PureRef(&grammar.rules[0]), 0, TerminalOrEof::EOF);
    let item_set_0 = ItemSet::new(vec![start_position], &grammar, &first_set);

    let mut item_sets = vec![item_set_0];
    let mut goto_cache: HashMap<(usize, GrammarToken<T, NT>), usize> = HashMap::new();

    {
        let mut i = 0;
        while i < item_sets.len() {
            let item_set = item_sets[i].clone();

            let mut possible_transitions = item_set
                .positions
                .iter()
                .filter_map(|position| position.locus())
                .collect::<Vec<_>>();

            hash_dedup(&mut possible_transitions);

            for transition in possible_transitions {
                if goto_cache.contains_key(&(i, transition.clone())) {
                    continue;
                }

                let goto_item_set = goto(&item_set, &grammar, &first_set, &transition);

                if !item_sets.contains(&goto_item_set) {
                    goto_cache.insert((i, transition.clone()), item_sets.len());
                    item_sets.push(goto_item_set);
                } else {
                    goto_cache.insert(
                        (i, transition.clone()),
                        item_sets.iter().position(|x| x == &goto_item_set).unwrap()
                    );
                }
            }

            item_sets[i].dump(i);

            i += 1;
        }
    }

    let mut parsing_table = HashMap::<(usize, TokenOrEof<T, NT>), Action<'a, T, NT>>::new();

    let mut add_to_parsing_table = |from: usize,
                                    transition: TokenOrEof<T, NT>,
                                    action: Action<'a, T, NT>,
    | {
        let key = (from, transition);
        parsing_table.entry(key).and_modify(|existing_action| {
            if let Action::Conflict(actions) = existing_action {
                let (a1, a2) = (**actions).clone();
                if a1 == action || a2 == action {} else {
                    let old_conflict = Action::Conflict(actions.clone());
                    *existing_action = Action::Conflict(Box::new((old_conflict, action.clone())));
                }
            } else if *existing_action != action {
                let old_action = existing_action.clone();
                *existing_action = Action::Conflict(Box::new((old_action, action.clone())));
            }
        }).or_insert(action);
    };

    for (i, item_set) in item_sets.iter().enumerate() {
        for item in &item_set.positions {
            match item.locus() {
                None => {
                    if item.lookahead == TerminalOrEof::EOF && item.rule.lhs == grammar.start_symbol() {
                        add_to_parsing_table(i, TokenOrEof::EOF, Action::Accept);
                    } else {
                        add_to_parsing_table(
                            i,
                            item.lookahead.clone().to_token_or_eof(),
                            Action::Reduce(item.rule),
                        );
                    }
                }
                Some(token) => {
                    let goto_item_set = goto_cache[&(i, token.clone())];
                    let shift = Action::Shift(goto_item_set);
                    add_to_parsing_table(i, token.clone().to_token_or_eof(), shift);
                }
            }
        }
    }

    ParsingTable { table: parsing_table }
}

pub trait InputToken<T: Terminal> {
    fn kind(&self) -> T;
}

#[derive(Debug)]
pub enum InputTokenOrAst<T: Terminal, I: InputToken<T>, A> {
    InputToken(I, PhantomData<T>),
    Ast(A),
}

#[derive(Debug)]
pub struct ParsingError<T: Terminal, NT: NonTerminal> {
    pub item_set: usize,
    pub token: TokenOrEof<T, NT>,
}

pub fn parse<T: Terminal, NT: NonTerminal, I: InputToken<T>, A>(
    parsing_table: &ParsingTable<T, NT>,
    callbacks: HashMap<
        PureRef<GrammarRule<T, NT>>,
        fn(Vec<InputTokenOrAst<T, I, A>>) -> A,
    >,
    tokens: impl Iterator<Item=I>,
) -> Result<A, ParsingError<T, NT>> {
    let mut stack = vec![0];
    let mut ast_stack = vec![];

    let mut tokens = tokens.peekable();

    loop {
        let state = stack.last().unwrap();
        let terminal = tokens.peek().map(|t|
            TokenOrEof::Terminal(t.kind())
        ).unwrap_or(TokenOrEof::EOF);

        let action = parsing_table.table.get(&(*state, terminal.clone()));
        if let Some(action) = action {
            match action {
                Action::Shift(next_state) => {
                    stack.push(*next_state);
                    ast_stack.push(InputTokenOrAst::InputToken(
                        tokens.next().expect("Input stream ended"), PhantomData
                    ));
                }
                Action::Reduce(rule) => {
                    let mut rhs = vec![];
                    for _ in 0..rule.rhs.len() {
                        stack.pop();
                        rhs.push(ast_stack.pop().unwrap());
                    }
                    rhs.reverse();

                    ast_stack.push(InputTokenOrAst::Ast(callbacks[rule](rhs)));

                    let goto_state = parsing_table.table.get(&(
                        *stack.last().unwrap(),
                        TokenOrEof::NonTerminal(rule.lhs.clone())
                    )).unwrap();

                    if let Action::Shift(goto_state) = goto_state {
                        stack.push(*goto_state);
                    } else {
                        panic!("Expected shift action");
                    }
                }
                Action::Accept => {
                    return Ok(match ast_stack.pop().unwrap() {
                        InputTokenOrAst::Ast(ast, ..) => ast,
                        _ => panic!("Expected AST"),
                    });
                }
                Action::Conflict(_) => {
                    return Err(ParsingError { item_set: *state, token: terminal });
                }
            }
        } else {
            return Err(ParsingError { item_set: *state, token: terminal });
        }
    }
}
