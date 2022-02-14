use crate::parser::span::{LinePos, StrSpan};
use nom::error::Error;
use nom::{
    Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, UnspecializedInput,
};

#[test]
fn tag_test() {
    use nom::bytes::complete::tag;

    let span = StrSpan::new("Hello world!");

    let (i, o) = tag::<&str, StrSpan, Error<StrSpan>>("Hello")(span.clone()).unwrap();

    assert_eq!(o.inner.len(), 5);
    assert_eq!(
        &o.span.start,
        &LinePos {
            line: 0,
            position: 0,
        }
    );
    assert_eq!(
        &o.span.end,
        &LinePos {
            line: 0,
            position: 5,
        }
    );

    assert_eq!(i.inner.len(), 7);
    assert_eq!(
        &i.span.start,
        &LinePos {
            line: 0,
            position: 5,
        }
    );
    assert_eq!(
        &i.span.end,
        &LinePos {
            line: 0,
            position: 12,
        }
    );
}

#[test]
fn tag_nl_test() {
    use nom::bytes::complete::tag;

    let span = StrSpan::new("a\nb\r\nc\rdefg");

    let (i, o) = tag::<&str, StrSpan, Error<StrSpan>>("a\nb\r\nc\rd")(span.clone()).unwrap();

    assert_eq!(o.inner.len(), 8);
    assert_eq!(
        &o.span.start,
        &LinePos {
            line: 0,
            position: 0,
        }
    );
    assert_eq!(
        &o.span.end,
        &LinePos {
            line: 3,
            position: 1,
        }
    );

    assert_eq!(i.inner.len(), 3);
    assert_eq!(
        &i.span.start,
        &LinePos {
            line: 3,
            position: 1,
        }
    );
    assert_eq!(
        &i.span.end,
        &LinePos {
            line: 3,
            position: 4,
        }
    );
}
