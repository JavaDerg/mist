use std::collections::Bound;
use std::ops::{Deref, Index, Range, RangeBounds, RangeFrom, RangeTo};
use std::slice::SliceIndex;
use std::str::{CharIndices, Chars};

use crate::parser::source::Source;
use nom::character::complete::u64;
use nom::error::{ErrorKind, ParseError};
use nom::{
    Compare, CompareResult, Err, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition,
    Needed, Offset, Slice, UnspecializedInput,
};

#[derive(Copy, Clone, Debug)]
pub struct StrSpan<'a> {
    pub inner: &'a str,
    pub span: Span,
    pub source: Source,
}

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Copy, Clone, Default, Debug, Eq, PartialEq)]
pub struct LinePos {
    pub line: usize,
    pub position: usize,
}

impl<'a> StrSpan<'a> {
    pub fn new(str: &'a str, source: Source) -> Self {
        Self {
            inner: str,
            span: Span {
                start: 0,
                end: str.len(),
            },
            source,
        }
    }
}

pub trait TokenSource {
    fn document(&self) -> &str;
}

impl LinePos {
    pub fn find(&self, str: &str) -> Self {
        let mut lines = 0;
        let mut pos = self.position;
        let mut r = false;
        for c in str.bytes() {
            match c {
                b'\r' if !r => r = true,
                b'\r' if r => {
                    r = true;
                    lines += 1;
                }
                b'\n' if !r => {
                    lines += 1;
                    pos = 0;
                }
                _ if r => {
                    r = false;
                    lines += 1;
                    pos = 1;
                }
                _ => {
                    pos += 1;
                }
            }
        }
        if r {
            lines += 1;
            pos = 0;
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
                start: self.span.start,
                end: self.span.start + count,
            },
            source: self.source,
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.inner.split_at(count);

        (
            Self {
                inner: suffix,
                span: Span {
                    start: self.span.start + count,
                    end: self.span.end,
                },
                source: self.source,
            },
            Self {
                inner: prefix,
                span: Span {
                    start: self.span.start,
                    end: self.span.start + count,
                },
                source: self.source,
            },
        )
    }
}

impl<'a> UnspecializedInput for StrSpan<'a> {}

// impl<'a> InputTakeAtPosition for StrSpan<'a> {
//     type Item = char;
//
//     fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
//     where
//         P: Fn(Self::Item) -> bool,
//     {
//         let (i, o) = self.inner.split_at_position(predicate).map_err(
//             |err: Err<nom::error::Error<&str>>| match err {
//                 Err::Incomplete(n) => Err::Incomplete(n),
//                 _ => unreachable!(),
//             },
//         )?;
//         let taken = self.inner.len() - i.len();
//         if taken == 0 {
//             Ok((
//                 self.clone(),
//                 Self {
//                     inner: o,
//                     span: Span {
//                         end: self.span.start,
//                         ..self.span.clone()
//                     },
//                 },
//             ))
//         } else {
//             Ok(self.take_split(taken))
//         }
//     }
//
//     fn split_at_position1<P, E: ParseError<Self>>(
//         &self,
//         predicate: P,
//         e: ErrorKind,
//     ) -> IResult<Self, Self, E>
//     where
//         P: Fn(Self::Item) -> bool,
//     {
//         let (i, _) = self.inner.split_at_position(predicate).map_err(
//             |err: Err<nom::error::Error<&str>>| match err {
//                 Err::Incomplete(n) => Err::Incomplete(n),
//                 _ => unreachable!(),
//             },
//         )?;
//         let taken = self.inner.len() - i.len();
//         if taken == 0 {
//             Err(Err::Error(E::from_error_kind(self.clone(), e)))
//         } else {
//             Ok(self.take_split(taken))
//         }
//     }
//
//     fn split_at_position_complete<P, E: ParseError<Self>>(
//         &self,
//         predicate: P,
//     ) -> IResult<Self, Self, E>
//     where
//         P: Fn(Self::Item) -> bool,
//     {
//         let (i, o) = self
//             .inner
//             .split_at_position_complete::<_, nom::error::Error<&str>>(predicate)
//             .expect("This should not return any errors");
//         let taken = self.inner.len() - i.len();
//         if taken == 0 {
//             Ok((
//                 self.clone(),
//                 Self {
//                     inner: o,
//                     span: Span {
//                         end: self.span.start,
//                         ..self.span.clone()
//                     },
//                 },
//             ))
//         } else {
//             Ok(self.take_split(taken))
//         }
//     }
//
//     fn split_at_position1_complete<P, E: ParseError<Self>>(
//         &self,
//         predicate: P,
//         e: ErrorKind,
//     ) -> IResult<Self, Self, E>
//     where
//         P: Fn(Self::Item) -> bool,
//     {
//         self.split_at_position1(predicate, e)
//             .map_err(|err| match err {
//                 Err::Incomplete(_) => Err::Error(E::from_error_kind(self.clone(), e)),
//                 x => x,
//             })
//     }
// }

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

impl<'a> Compare<StrSpan<'a>> for StrSpan<'a> {
    fn compare(&self, t: StrSpan<'a>) -> CompareResult {
        self.inner.compare(t.inner)
    }

    fn compare_no_case(&self, t: StrSpan<'a>) -> CompareResult {
        self.inner.compare_no_case(t.inner)
    }
}

impl<'a> Compare<&'a str> for StrSpan<'a> {
    fn compare(&self, t: &'a str) -> CompareResult {
        self.inner.compare(t)
    }

    fn compare_no_case(&self, t: &'a str) -> CompareResult {
        self.inner.compare_no_case(t)
    }
}

impl<'a> Offset for StrSpan<'a> {
    fn offset(&self, second: &Self) -> usize {
        second.span.start - self.span.start
    }
}

impl<'a, R: RangeBounds<usize> + SliceIndex<str, Output = str>> Slice<R> for StrSpan<'a> {
    fn slice(&self, range: R) -> Self {
        let rel_start = match range.start_bound() {
            Bound::Included(&i) if i == usize::MAX => i,
            Bound::Included(&i) => i + 1,
            Bound::Excluded(&i) => i,
            Bound::Unbounded => 0,
        };
        let slice = self.inner.get(range).expect("Slice out of bounds");
        let len = slice.len();
        Self {
            inner: slice,
            span: Span {
                start: self.span.start + rel_start,
                end: self.span.start + rel_start + len,
            },
            source: self.source,
        }
    }
}

impl<'a> Deref for StrSpan<'a> {
    type Target = &'a str;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
