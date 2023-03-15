#[derive(Debug, Clone)]
pub struct Typst(pub String);

#[derive(Debug, Clone)]
pub struct Rule {
    pub lhs: String,
    pub alternatives: Vec<Alternative>
}

#[derive(Debug, Clone)]
pub struct Alternative {
    pub rhs: Vec<String>,
    pub action: Typst,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub typst_kind: Typst,
    pub local_kind: String,
    pub cast: Typst,
}

#[derive(Debug, Clone)]
pub struct Grammar {
    pub rules: Vec<Rule>,
    pub casts: Vec<Cast>,
}
