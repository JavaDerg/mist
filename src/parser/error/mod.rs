mod num;

pub use num::NumberSyntaxError;

use crate::parser::span::StrSpan;
use nom::error::{Error as NomError, ErrorKind, ParseError, VerboseError, VerboseErrorKind};
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum SyntaxError<'a> {
    Generic(NomError<StrSpan<'a>>),
    Message {
        msg: Cow<'a, str>,
        span: StrSpan<'a>,
    },
    Number {
        kind: NumberSyntaxError,
        span: StrSpan<'a>,
    },
    Many(Vec<SyntaxError<'a>>),
}

pub enum SyntaxErrorKind {}

impl<'a> SyntaxError<'a> {}

impl<'a> ParseError<StrSpan<'a>> for SyntaxError<'a> {
    fn from_error_kind(input: StrSpan<'a>, kind: ErrorKind) -> Self {
        Self::Generic(NomError::from_error_kind(input, kind))
    }

    fn append(input: StrSpan<'a>, kind: ErrorKind, mut other: Self) -> Self {
        if let SyntaxError::Many(vec) = &mut other {
            vec.push(Self::from_error_kind(input, kind));
            other
        } else {
            Self::Many(vec![other, Self::from_error_kind(input, kind)])
        }
    }

    fn or(mut self, mut other: Self) -> Self {
        if let SyntaxError::Many(vec) = &mut self {
            match other {
                SyntaxError::Many(mut ovec) => vec.append(&mut ovec),
                x => vec.push(x),
            }
            self
        } else if let SyntaxError::Many(vec) = &mut other {
            match self {
                SyntaxError::Many(mut ovec) => vec.append(&mut ovec),
                x => vec.push(x),
            }
            other
        } else {
            SyntaxError::Many(vec![self, other])
        }
    }
}

impl<'a> From<NomError<StrSpan<'a>>> for SyntaxError<'a> {
    fn from(err: NomError<StrSpan<'a>>) -> Self {
        Self::Generic(err)
    }
}

impl<'a> Display for SyntaxError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxError::Generic(NomError { input, code }) => write!(
                f,
                "{:?}\n\tFrom {} to {} in {}",
                code, input.span.start, input.span.end, input.source,
            ),
            SyntaxError::Message { msg, span } => write!(
                f,
                "{}\n\tFrom {} to {} in {}",
                msg, span.span.start, span.span.end, span.source,
            ),
            SyntaxError::Number { kind, span } => write!(
                f,
                "{}\n\tFrom {} to {} in {}",
                kind, span.span.start, span.span.end, span.source
            ),
            SyntaxError::Many(many) => Ok(for err in many {
                writeln!(f, "{}", err)?;
            }),
        }
    }
}
