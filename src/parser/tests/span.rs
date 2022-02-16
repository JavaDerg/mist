use crate::parser::span::{LinePos, StrSpan};
use nom::error::Error;
use nom::{
    Compare, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, UnspecializedInput,
};

#[test]
fn tag_test() {
    use nom::bytes::complete::tag;

    let span = StrSpan::new("Hello world!", 0);

    let (i, o) = tag::<&str, StrSpan, Error<StrSpan>>("Hello")(span).unwrap();

    assert_eq!(o.inner.len(), 5);
    assert_eq!(o.span.start, 0,);
    assert_eq!(o.span.end, 5,);

    assert_eq!(i.inner.len(), 7);
    assert_eq!(i.span.start, 5,);
    assert_eq!(i.span.end, 12,);
}
