use crate::parser::span::{Span, TokenSource};
use crate::parser::value::num::Number;

pub struct Token<S: TokenSource> {
    pub kind: TokenKind,
    pub span: Span,
    pub source: S,
}

pub enum TokenKind {
    Literal(Number),
}

pub enum Literal {
    String(String),
    Number(Number),
}
