use nom::combinator::iterator;
use nom::{Compare, Err, InputTake, InputTakeAtPosition, IResult};
use nom::error::{ErrorKind, ParseError};

#[derive(Clone)]
pub struct StrSpan<'a> {
    pub inner: &'a str,
    pub span: Span,
}

#[derive(Clone)]
pub struct Span {
    pub index: usize,
    pub start: LinePos,
    pub stop: LinePos,
}

#[derive(Copy, Clone)]
pub struct LinePos {
    pub line: usize,
    pub position: usize,
}

pub trait TokenSource {

}

impl LinePos {
    pub fn find(&self, str: &str) -> Self {
        let mut lines = 0;
        let mut pos = self.position;
        let mut r = false;
        for c in str.bytes() {
            match c {
                b'\r' => r = true,
                b'\n' if !r => lines += 1,
                _ if r => {
                    r = false;
                    lines += 1;
                }
                _ => {
                    pos += 1;
                }
            }
        }
        if r {
            lines += 1;
        }
        Self {
            line: self.line + lines,
            position: pos,
        }
    }
}

impl<'a> InputTake for StrSpan<'a> {
    fn take(&self, count: usize) -> Self {
        Self {
            inner: &self.inner[..count],
            span: Span {
                stop: self.span.start.find(&self.inner[..count]),
                ..self.span.clone()
            }
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let slice = &self.inner[..count];
        let np = self.span.start.find(slice);

        (Self {
            inner: slice,
            span: Span {
                stop: np.clone(),
                ..self.span.clone()
            }
        }, Self {
            inner: &self.inner[count..],
            span: Span {
                index: self.span.index + count,
                start: np,
                stop: self.span.stop,
            }
        })
    }
}

impl<'a> InputTakeAtPosition for StrSpan<'a> {
    type Item = char;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E> where P: Fn(Self::Item) -> bool {
        let (i, o) = self.inner.split_at_position(predicate).map_err(|err: Err<nom::error::Error<&str>>| match err {
            Err::Incomplete(n) => Err::Incomplete(n),
            _ => unreachable!(),
        })?;
        let taken = self.inner.len() - i.len();
        if taken == 0 {
            Ok((self.clone(), Self {
                inner: o,
                span: Span {
                    stop: self.span.start,
                    ..self.span.clone()
                },
            }))
        } else {
            Ok(self.take_split(taken))
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(&self, predicate: P, e: ErrorKind) -> IResult<Self, Self, E> where P: Fn(Self::Item) -> bool {
        todo!()
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E> where P: Fn(Self::Item) -> bool {
        todo!()
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(&self, predicate: P, e: ErrorKind) -> IResult<Self, Self, E> where P: Fn(Self::Item) -> bool {
        todo!()
    }
}
