use std::str::{CharIndices, Chars};
use nom::combinator::iterator;
use nom::error::{Error, ErrorKind, ParseError};
use nom::{Compare, Err, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Needed};

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

pub trait TokenSource {}

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
            },
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let slice = &self.inner[..count];
        let np = self.span.start.find(slice);

        (
            Self {
                inner: slice,
                span: Span {
                    stop: np.clone(),
                    ..self.span.clone()
                },
            },
            Self {
                inner: &self.inner[count..],
                span: Span {
                    index: self.span.index + count,
                    start: np,
                    stop: self.span.stop,
                },
            },
        )
    }
}

impl<'a> InputTakeAtPosition for StrSpan<'a> {
    type Item = char;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        let (i, o) = self.inner.split_at_position(predicate).map_err(
            |err: Err<nom::error::Error<&str>>| match err {
                Err::Incomplete(n) => Err::Incomplete(n),
                _ => unreachable!(),
            },
        )?;
        let taken = self.inner.len() - i.len();
        if taken == 0 {
            Ok((
                self.clone(),
                Self {
                    inner: o,
                    span: Span {
                        stop: self.span.start,
                        ..self.span.clone()
                    },
                },
            ))
        } else {
            Ok(self.take_split(taken))
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        let (i, _) = self.inner.split_at_position(predicate).map_err(
            |err: Err<nom::error::Error<&str>>| match err {
                Err::Incomplete(n) => Err::Incomplete(n),
                _ => unreachable!(),
            },
        )?;
        let taken = self.inner.len() - i.len();
        if taken == 0 {
            Err(Err::Error(E::from_error_kind(self.clone(), e)))
        } else {
            Ok(self.take_split(taken))
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        let (i, o) = self
            .inner
            .split_at_position_complete::<_, nom::error::Error<&str>>(predicate)
            .expect("This should not return any errors");
        let taken = self.inner.len() - i.len();
        if taken == 0 {
            Ok((
                self.clone(),
                Self {
                    inner: o,
                    span: Span {
                        stop: self.span.start,
                        ..self.span.clone()
                    },
                },
            ))
        } else {
            Ok(self.take_split(taken))
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.split_at_position1(predicate, e)
            .map_err(|err| match err {
                Err::Incomplete(_) => Err::Error(E::from_error_kind(self.clone(), e)),
                x => x,
            })
    }
}

impl<'a> InputLength for StrSpan<'a> {
    fn input_len(&self) -> usize {
        self.inner.len()
    }
}

impl<'a> InputIter for StrSpan<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;

    fn iter_indices(&self) -> Self::Iter {
        self.inner.char_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.inner.chars()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.inner
            .char_indices()
            .filter(|&(_, c)| predicate(c))
            .map(|(i, _)| i)
            .next()
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        self.inner
            .char_indices()
            .map(|(i, _)| i)
            .nth(count)
            .ok_or(Needed::Unknown)
    }
}
