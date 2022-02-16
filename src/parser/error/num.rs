use crate::parser::error::SyntaxError;
use crate::parser::span::StrSpan;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum NumberSyntaxError {
    NoSignOnUnsigned,
    InvalidPrefix(char),
    InvalidSuffix(char, Cow<'static, str>),
    UnparsableNumber,
    FloatHasRadix,
}

impl Display for NumberSyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberSyntaxError::NoSignOnUnsigned => {
                f.write_str("A unsigned number can not be negative")
            }
            NumberSyntaxError::InvalidPrefix(prefix) => write!(f, "Invalid prefix '0{}'", prefix),
            NumberSyntaxError::InvalidSuffix(suffix, msg) => {
                write!(f, "Invalid suffix '{}'{}", suffix, msg)
            }
            NumberSyntaxError::UnparsableNumber => f.write_str("Number is unparsable"),
            NumberSyntaxError::FloatHasRadix => f.write_str("Doubles may not have a radix prefix"),
        }
    }
}
