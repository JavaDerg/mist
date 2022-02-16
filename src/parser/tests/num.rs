use crate::parser::error::{NumberSyntaxError, SyntaxError};
use crate::parser::span::StrSpan;
use crate::parser::value::num::{num, Number};

#[test]
fn num_basic() {
    assert_eq!(num(StrSpan::new("0", 0)).unwrap().1, Number::I64(0),);

    assert!(matches!(
        num(StrSpan::new("test", 0)),
        Err(nom::Err::Error(SyntaxError::Number {
            kind: NumberSyntaxError::UnparsableNumber,
            ..
        }))
    ));

    assert!(matches!(
        num(StrSpan::new("123*", 0)),
        Err(nom::Err::Failure(SyntaxError::Number {
            kind: NumberSyntaxError::UnparsableNumber,
            ..
        }))
    ));
}

#[test]
fn num_signed() {
    assert_eq!(num(StrSpan::new("1337", 0)).unwrap().1, Number::I64(1337),);
    assert_eq!(num(StrSpan::new("-1337", 0)).unwrap().1, Number::I64(-1337),);

    assert_eq!(num(StrSpan::new("1337i", 0)).unwrap().1, Number::I64(1337),);
    assert_eq!(num(StrSpan::new("-1337i", 0)).unwrap().1, Number::I64(-1337),);
}

#[test]
fn num_unsigned() {
    assert_eq!(num(StrSpan::new("1337u", 0)).unwrap().1, Number::U64(1337),);

    assert!(matches!(
        num(StrSpan::new("-1337u", 0)),
        Err(nom::Err::Failure(SyntaxError::Number {
            kind: NumberSyntaxError::NoSignOnUnsigned,
            ..
        }))
    ));
}

#[test]
fn num_prefix() {
    assert_eq!(num(StrSpan::new("0xFF", 0)).unwrap().1, Number::I64(255),);
    assert_eq!(num(StrSpan::new("0o10", 0)).unwrap().1, Number::I64(8),);
    assert_eq!(num(StrSpan::new("0b1111", 0)).unwrap().1, Number::I64(15),);
}

#[test]
fn num_f64() {
    assert_eq!(num(StrSpan::new("0.123", 0)).unwrap().1, Number::F64(0.123));
}
