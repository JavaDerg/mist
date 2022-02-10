mod string;

use std::borrow::Cow;
use std::collections::HashMap;
use std::env::set_current_dir;
use std::mem::swap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::time::Instant;
use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag, take_until};
use nom::character::complete::{alpha1, alphanumeric0, alphanumeric1, char, digit1, one_of};
use nom::combinator::{cut, iterator, map, opt, recognize};
use nom::error::ErrorKind;
use nom::multi::{many0, many_till, separated_list0};
use nom::sequence::{delimited, preceded, tuple};
use nom::{IResult, InputTakeAtPosition, Parser};
use crate::string::parse_string;

#[derive(Debug, Clone, Eq, PartialEq)]
enum Token {
    Number(i64),
    Bool(bool),
    String(String),

    Local(String),
    Operation(Operation),
    Call(String),
    Def(String),

    Body(Vec<Token>),
    If(Vec<Token>, Option<Vec<Token>>),
    Loop(Vec<Token>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Operation {
    /// .
    Dup,
    /// @
    Seek,
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
    /// %
    Mod,
    /// :
    Set,
    /// =
    Eq,
    /// !=
    Neq,
    /// >
    Gt,
    /// <
    Lt,
    /// >=
    GtEq,
    /// <=
    LtEq,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Value {
    Number(i64),
    Bool(bool),
    String(String),
    Ref(String),
    Sequence(Vec<Token>),
}

enum Function {
    Interop(Rc<dyn Fn(&mut Vm)>),
    Normal(Vec<Token>),
}

enum MutCow<'a, T> {
    Owned(T),
    Borrowed(&'a mut T),
}

struct Vm<'a> {
    code: &'a [Token],
    stack: MutCow<'a, Stack>,
    locals: MutCow<'a, HashMap<String, Value>>,
    functions: MutCow<'a, HashMap<String, Function>>,
}

struct Stack {
    inner: Vec<Value>,
}

impl Stack {
    pub fn push(&mut self, v: Value) {
        self.inner.push(v);
    }

    pub fn dup(&mut self) -> Option<()> {
        match self.inner.last() {
            Some(v) => self.push(v.clone()),
            None => {
                eprintln!("can not dup on empty stack");
                None?;
            }
        }
        Some(())
    }

    pub fn do_seek(&mut self) -> Option<()> {
        match self.inner.pop() {
            Some(Value::Number(n)) => {
                if n.is_negative() {
                    eprintln!("can not seek into the future");
                    None
                } else {
                    let i = n as usize;
                    if i >= self.inner.len() {
                        eprintln!("can not seek into the abyss");
                        None
                    } else {
                        self.push(self.inner[self.inner.len() - i - 1].clone());
                        Some(())
                    }
                }
            },
            Some(_) => {
                eprintln!("can not seek to none-number");
                None
            }
            None => {
                eprintln!("stack empty, failed to pop 1");
                None
            }
        }
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.inner.pop()
    }

    pub fn pop_num(&mut self, locals: &HashMap<String, Value>) -> Option<i64> {
        match self.inner.pop() {
            Some(Value::Number(n)) => Some(n),
            Some(Value::Ref(r)) => match locals.get(&r)? {
                Value::Number(n) => Some(*n),
                _ => {
                    eprintln!("value not a number");
                    None
                }
            }
            Some(_) => {
                eprintln!("value not a number");
                None
            }
            None => {
                eprintln!("stack empty, failed to pop 1");
                None
            }
        }
    }
}

impl<'a> Vm<'a> {
    pub fn op(&mut self, op: Operation) -> Option<()> {
        match op {
            Operation::Dup => self.stack.dup()?,
            Operation::Seek => self.stack.do_seek()?,
            Operation::Add => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Number(
                    v1 + v2
                ));
            }
            Operation::Sub => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Number(
                    v1 - v2
                ));
            }
            Operation::Mul => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Number(
                    v1 * v2
                ));
            }
            Operation::Div => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Number(
                    v1 / v2
                ));
            }
            Operation::Mod => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Number(
                    v1 % v2
                ));
            }
            Operation::Set => {}
            Operation::Eq => {}
            Operation::Neq => {}
            Operation::Gt => {}
            Operation::Lt => {}
            Operation::GtEq => {}
            Operation::LtEq => {}
        }
        Some(())
    }

    pub fn call(&mut self, fun: &String) -> Option<()> {
        let fun = match self.functions.get(fun) {
            Some(f) => f,
            None => {
                eprintln!("function does not exist '{}'", fun);
                None?
            }
        };
        match fun {
            Function::Interop(f) => {
                let fun = f.clone();
                fun(self);
                return Some(());
            },
            Function::Normal(_) => {}
        }
        Some(())
    }
}

impl<'a> Iterator for Vm<'a> {
    type Item = ();

    fn next(&mut self) -> Option<Self::Item> {
        if self.code.is_empty() {
            return None;
        }
        match &self.code[0] {
            Token::Number(n) => self.stack.push(Value::Number(*n)),
            Token::Bool(b) => self.stack.push(Value::Bool(*b)),
            Token::String(s) => self.stack.push(Value::String(s.clone())),
            Token::Local(l) => self.stack.push(Value::Ref(l.clone())),
            Token::Operation(o) => self.op(*o)?,
            Token::Call(c) => self.call(c)?,
            Token::Def(d) => {todo!()}
            Token::Body(b) => self.stack.push(Value::Sequence(b.clone())),
            Token::If(t, f) => {todo!()}
            Token::Loop(l) => {todo!()}
        }

        self.code = &self.code[1..];
        Some(())
    }
}

fn main() {
    let st = Instant::now();
    let (i, tokens) = tokens(include_str!("test.mist")).unwrap();
    let took = st.elapsed();
    eprintln!("Parsing took {}µs\n\n", took.as_micros());

    if !i.trim().is_empty() {
        eprint!("invalid code");
        return;
    }

    let mut vm = Vm {
        code: tokens.as_ref(),
        stack: MutCow::Owned(Stack {
            inner: vec![]
        }),
        locals: MutCow::Owned(Default::default()),
        functions: MutCow::Owned(std())
    };
    let st = Instant::now();
    while let Some(()) = vm.next() {}
    let took = st.elapsed();
    eprintln!("\n\nExecution took {}s {}ms {}µs", took.as_secs(), took.as_millis() % 1000, took.as_micros() % 1000);
}

fn std() -> HashMap<String, Function> {
    [
        ("dbg".to_owned(), Function::Interop(Rc::new(|vm| println!("{:?}", vm.stack.pop())))),
    ]
        .into_iter()
        .collect()
}

fn tokens(i: &str) -> IResult<&str, Vec<Token>> {
    many0(token)(i)
}

fn token(i: &str) -> IResult<&str, Token> {
    // comments
    let (i, _) = opt(preceded(
        space0,
        alt((single_line_comment, multiline_comment)),
    ))(i)?;
    // remove prepended space characters is present
    let (i, _) = space0(i)?;

    alt((
        num,
        bool,
        variable,
        op,
        branch,
        loop_,
        map(body, |body| Token::Body(body)),
        map(parse_string, |str| Token::String(str)),
        map(preceded(char('$'), name), |fun| {
            Token::Def(fun.into())
        }),
        map(name, |fun| Token::Call(fun.into())),
    ))(i)
}

fn loop_(i: &str) -> IResult<&str, Token> {
    let (mut i, _) = tag("loop")(i)?;
    let mut body = vec![];
    loop {
        let (ni, token) = token(i)?;
        i = ni;
        match &token {
            Token::Call(ref s) if s == "end" => break,
            _ => body.push(token),
        }
    }
    Ok((i, Token::Loop(body)))
}

fn branch(i: &str) -> IResult<&str, Token> {
    let (mut i, _) = tag("if")(i)?;
    let mut c_branch = vec![];
    let mut b_true = None;
    loop {
        let (ni, token) = token(i)?;
        i = ni;
        match &token {
            Token::Call(ref s) if s == "else" => {
                let mut other = vec![];
                swap(&mut other, &mut c_branch);
                b_true = Some(other);
            },
            Token::Call(ref s) if s == "end" => break,
            _ => c_branch.push(token),
        }
    }
    Ok((i, if let Some(bt) = b_true {
        Token::If(bt, Some(c_branch))
    } else {
        Token::If(c_branch, None)
    }))
}

fn body(i: &str) -> IResult<&str, Vec<Token>> {
    delimited(char('('), tokens, preceded(space0, char(')')))(i)
}

fn name(i: &str) -> IResult<&str, &str> {
    recognize(tuple((alpha1, alphanumeric0)))(i)
}

fn single_line_comment(i: &str) -> IResult<&str, &str> {
    recognize(tuple((tag("//"), is_not("\r\n"))))(i)
}

fn multiline_comment(i: &str) -> IResult<&str, &str> {
    delimited(tag("/*"), take_until("*/"), cut(tag("*/")))(i)
}

fn op(i: &str) -> IResult<&str, Token> {
    let (i, o) = alt((
        tag("!="),
        tag("<="),
        tag(">="),
        recognize(one_of(".@+-*/%:=<>")),
    ))(i)?;

    Ok((
        i,
        Token::Operation(match o {
            "." => Operation::Dup,
            "@" => Operation::Seek,
            "+" => Operation::Add,
            "-" => Operation::Sub,
            "*" => Operation::Mul,
            "/" => Operation::Div,
            "%" => Operation::Mod,
            ":" => Operation::Set,
            "=" => Operation::Eq,
            "!=" => Operation::Neq,
            ">" => Operation::Gt,
            "<" => Operation::Lt,
            ">=" => Operation::GtEq,
            "<=" => Operation::LtEq,
            _ => unreachable!(),
        }),
    ))
}

fn variable(i: &str) -> IResult<&str, Token> {
    let (i, _) = tag("'")(i)?;
    let (i, var) = name(i)?;

    Ok((i, Token::Local(var.into())))
}

fn bool(i: &str) -> IResult<&str, Token> {
    alt((tag("true"), tag("false")))(i).map(|(i, b)| (i, Token::Bool(b == "true")))
}

fn num(i: &str) -> IResult<&str, Token> {
    recognize(tuple((opt(tag("-")), digit1)))(i)
        .map(|(i, n)| (i, Token::Number(n.parse::<i64>().unwrap())))
}

fn space0(i: &str) -> IResult<&str, &str> {
    opt(space)(i).map(|(i, s)| (i, s.unwrap_or("")))
}

fn space(i: &str) -> IResult<&str, &str> {
    i.split_at_position1_complete(|item| !item.is_whitespace(), ErrorKind::Space)
}

fn not_space(i: &str) -> IResult<&str, &str> {
    i.split_at_position_complete(|item| item.is_whitespace())
}

impl<'a, T> Deref for MutCow<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            MutCow::Owned(this) => this,
            MutCow::Borrowed(this) => &**this,
        }
    }
}

impl<'a, T> DerefMut for MutCow<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            MutCow::Owned(this) => this,
            MutCow::Borrowed(this) => *this,
        }
    }
}
