use crate::parser::span::StrSpan;
use nom::error::ErrorKind;
use nom::{IResult, InputTakeAtPosition};

pub mod span;
#[cfg(test)]
mod tests;
pub mod token;
pub mod value;

pub fn space(i: StrSpan) -> IResult<StrSpan, StrSpan> {
    if i.inner.is_empty() {
        return Ok((i, i));
    }
    i.split_at_position1_complete(|item| !item.is_whitespace(), ErrorKind::Space)
}
