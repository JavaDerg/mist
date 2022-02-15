use crate::parser::span::StrSpan;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1, hex_digit1};
use nom::combinator::{opt, recognize};
use nom::error::{ErrorKind, ParseError};
use nom::sequence::tuple;
use nom::Err::Error;
use nom::{AsChar, IResult, InputTakeAtPosition, Parser};
use nom::multi::{many0, many0_count};
use crate::Token::String;

pub enum Number {
    I64(i64),
    U64(u64),
    F64(f64),
}

fn num<'a, E: ParseError<StrSpan<'a>>>(i: StrSpan<'a>) -> IResult<StrSpan<'a>, Number, E> {
    let (i, negative) = opt(tag("-"))(i).map(|(i, n)| (i, n.is_some()))?;
    let (i, prefix) = opt(alt((tag("0x"), tag("0b"))))(i)?;
    let digit_fn = match prefix.clone().map(|x| x.inner) {
        None => digit1,
        Some("0x") => hex_digit1,
        Some("0b") => bin_digit1,
        _ => return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Digit))),
    };
    let (i, num_span) = recognize(tuple((
        digit_fn,
        many0_count(alt((
            tag("_"), digit_fn
        ))),
        opt(tuple((
            tag("."),
            many0_count(alt((
                tag("_"), digit_fn
            ))),
        )))
    )))(i)?;

    let (i, suffix) = opt(
        alt((
            tag("u"),
            tag("i"),
            tag("f"),
        ))
    )(i)?;

    let num_s = num_span.inner.chars().filter(|&c| c != '_').collect::<std::string::String>();
    let radix = match prefix.map(|x| x.inner) {
        None => 10,
        Some("0x") => 16,
        Some("0b") => 2,
        _ => unreachable!(),
    };


    todo!()
}

pub fn bin_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(
        |item| {
            let char = item.as_char();
            !(char == '0' || char == '1')
        },
        ErrorKind::Digit,
    )
}
