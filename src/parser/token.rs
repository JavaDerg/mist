use crate::parser::span::{Span, TokenSource};

pub struct Token<S: TokenSource> {
    pub kind: TokenKind,
    pub span: Span,
    pub source: S,
}

pub enum TokenKind {
    Literal(String),
}
