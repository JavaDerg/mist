use std::borrow::Cow;
use crate::parser::error::NumberSyntaxError;
use crate::parser::error::SyntaxError;
use crate::parser::span::StrSpan;
use crate::Token::String;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1, hex_digit1, oct_digit1, satisfy};
use nom::combinator::{opt, recognize};
use nom::error::{ErrorKind, ParseError};
use nom::multi::{many0, many0_count};
use nom::sequence::tuple;
use nom::Err::Error;
use nom::{AsChar, Err, IResult, InputLength, InputTakeAtPosition, Parser};
use std::rc::Weak;
use crate::parser::space_or_eof;
use crate::space0;

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    I64(i64),
    U64(u64),
    F64(f64),
}

#[derive(Copy, Clone, Debug)]
enum Prefix {
    Decimal,
    Hexadecimal,
    Octal,
    Binary,
    Unknown(char),
}

pub fn num(i_neg: StrSpan) -> IResult<StrSpan, Number, SyntaxError> {
    let (i_prefix, negative) = opt(tag("-"))(i_neg).map(|(i, n)| (i, n.is_some()))?;
    let (i_num, prefix) = prefix(i_prefix)?;

    let digit_fn = match prefix {
        Prefix::Decimal => digit1,
        Prefix::Hexadecimal => hex_digit1,
        Prefix::Octal => oct_digit1,
        Prefix::Binary => bin_digit1,
        Prefix::Unknown(p) => {
            return Err(Err::Failure(SyntaxError::Number {
                span: i_prefix,
                kind: NumberSyntaxError::InvalidPrefix(p),
            }))
        }
    };
    let is_floating = i_num.inner.bytes().any(|b| b == b'.');
    let (i_suffix, num_span) = recognize(tuple((
        digit_fn,
        many0_count(alt((tag("_"), digit_fn))),
        opt(tuple((tag("."), many0_count(alt((tag("_"), digit_fn)))))),
    )))(i_num)
    .map_err(|_: nom::Err<nom::error::Error<StrSpan>>| {
        nom::Err::Error(SyntaxError::Number {
            kind: NumberSyntaxError::UnparsableNumber,
            span: i_num,
        })
    })?;

    let (i, suffix) =
        opt(alt((char('u'), char('i'), char('f'))))(i_suffix)?;

    let radix = prefix.as_radix();

    if is_floating && matches!(&suffix, Some('u') | Some('i')) {
        return Err(Err::Failure(SyntaxError::Number {
            kind: NumberSyntaxError::InvalidSuffix(suffix.unwrap(), Cow::Borrowed("\n\tNote: A double can not be signed or unsigned")),
            span: i_suffix,
        }));
    }
    if is_floating && radix != 10 {
        return Err(Err::Failure(SyntaxError::Number {
            kind: NumberSyntaxError::FloatHasRadix,
            span: i_suffix,
        }));
    }
    if negative && matches!(&suffix, Some('u')) {
        return Err(Err::Failure(SyntaxError::Number {
            kind: NumberSyntaxError::NoSignOnUnsigned,
            span: i_suffix,
        }));
    }

    let mut num_s = std::string::String::with_capacity(num_span.inner.len() + 1);
    if negative {
        num_s.push('-');
    }
    num_span
        .inner
        .chars()
        .filter(|&c| c != '_')
        .for_each(|c| num_s.push(c));

    let number = if is_floating {
        Number::F64(num_s.parse().unwrap())
    } else {
        match suffix {
            Some('u') => Number::U64(u64::from_str_radix(&num_s, radix).unwrap()),
            Some('i') | None => Number::I64(i64::from_str_radix(&num_s, radix).unwrap()),
            _ => unreachable!(),
        }
    };
    
    if space_or_eof(i).is_err() {
        return Err(Err::Failure(SyntaxError::Number { kind: NumberSyntaxError::UnparsableNumber, span: i }));
    };

    Ok((i, number))
}

fn prefix(i: StrSpan) -> IResult<StrSpan, Prefix, SyntaxError> {
    let (ni, prefix) = opt(tuple((char('0'), satisfy(|x| !x.is_ascii_digit()))))(i)?;
    if let Some((_, prefix)) = prefix {
        Ok((
            ni,
            match prefix {
                'x' => Prefix::Hexadecimal,
                'o' => Prefix::Octal,
                'b' => Prefix::Binary,
                'u' | 'i' | 'f' | '.' => return Ok((i, Prefix::Decimal)),
                x => Prefix::Unknown(x),
            },
        ))
    } else {
        Ok((i, Prefix::Decimal))
    }
}

fn bin_digit1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
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

impl Prefix {
    pub fn as_radix(&self) -> u32 {
        match self {
            Prefix::Decimal => 10,
            Prefix::Hexadecimal => 16,
            Prefix::Octal => 8,
            Prefix::Binary => 2,
            Prefix::Unknown(_) => panic!("Not a radix"),
        }
    }
}
