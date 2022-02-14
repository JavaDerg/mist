mod parser;
mod string;

use crate::string::parse_string;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until};
use nom::character::complete::{
    alpha1, char, digit1, one_of,
};
use nom::combinator::{cut, map, opt, recognize};
use nom::error::ErrorKind;
use nom::multi::{many0};
use nom::sequence::{delimited, preceded, tuple};
use nom::{IResult, InputTakeAtPosition};

use std::collections::HashMap;


use std::io::Write;
use std::mem::swap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::time::Instant;

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
    /// #
    Drop,
    /// ~
    Swap,
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
    /// Internal use only
    Undefined,
    Number(i64),
    Bool(bool),
    String(String),
    Ref(String),
    Sequence(Vec<Token>),
}

enum Function {
    Interop(Rc<dyn Fn(&mut Vm) -> Option<()>>),
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
        let val = match self.inner.last() {
            Some(v) => v.clone(),
            None => {
                eprintln!("can not dup on empty stack");
                None?
            }
        };
        self.push(val);
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
            }
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
            },
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

    pub fn pop_bool(&mut self, locals: &HashMap<String, Value>) -> Option<bool> {
        match self.inner.pop() {
            Some(Value::Bool(b)) => Some(b),
            Some(Value::Number(n)) => Some(n != 0),
            Some(Value::Ref(r)) => match locals.get(&r)? {
                Value::Bool(b) => Some(*b),
                Value::Number(n) => Some(*n != 0),
                _ => {
                    eprintln!("value not a bool or number");
                    None
                }
            },
            Some(_) => {
                eprintln!("value not a bool or number");
                None
            }
            None => {
                eprintln!("stack empty, failed to pop 1");
                None
            }
        }
    }

    pub fn pop_body(&mut self, locals: &HashMap<String, Value>) -> Option<Vec<Token>> {
        match self.inner.pop() {
            Some(Value::Sequence(ts)) => Some(ts),
            Some(Value::Ref(r)) => match locals.get(&r)? {
                Value::Sequence(ts) => Some(ts.clone()),
                _ => {
                    eprintln!("value not a sequence");
                    None
                }
            },
            Some(_) => {
                eprintln!("value not a sequence");
                None
            }
            None => {
                eprintln!("stack empty, failed to pop 1");
                None
            }
        }
    }

    pub fn pop_str(&mut self, locals: &HashMap<String, Value>) -> Option<String> {
        match self.inner.pop() {
            Some(Value::String(s)) => Some(s),
            Some(Value::Ref(r)) => match locals.get(&r)? {
                Value::String(s) => Some(s.clone()),
                _ => {
                    eprintln!("value not a string");
                    None
                }
            },
            Some(_) => {
                eprintln!("value not a string");
                None
            }
            None => {
                eprintln!("stack empty, failed to pop 1");
                None
            }
        }
    }

    pub fn pop_ref(&mut self, _locals: &HashMap<String, Value>) -> Option<String> {
        match self.inner.pop() {
            Some(Value::Ref(s)) => Some(s),
            Some(_) => {
                eprintln!("value not a local");
                None
            }
            None => {
                eprintln!("stack empty, failed to pop 1");
                None
            }
        }
    }

    pub fn pop_stringified(&mut self, locals: &HashMap<String, Value>) -> Option<String> {
        let val = self.pop()?;
        self.stringify(locals, val)
    }

    fn stringify(&mut self, locals: &HashMap<String, Value>, value: Value) -> Option<String> {
        Some(match value {
            Value::String(str) => str,
            Value::Number(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Sequence(_) => {
                eprintln!("Can not turn sequence into string");
                None?
            }
            Value::Ref(r) => {
                let r = locals.get(&r)?;
                return self.stringify(locals, r.clone());
            }
            Value::Undefined => "".to_string(),
        })
    }
}

impl<'a> Vm<'a> {
    pub fn op(&mut self, op: Operation) -> Option<()> {
        match op {
            Operation::Dup => self.stack.dup()?,
            Operation::Drop => drop(self.stack.pop()?),
            Operation::Swap => {
                let first = self.stack.pop()?;
                let second = self.stack.pop()?;
                self.stack.push(first);
                self.stack.push(second);
            }
            Operation::Seek => self.stack.do_seek()?,
            Operation::Add => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Number(v1 + v2));
            }
            Operation::Sub => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Number(v1 - v2));
            }
            Operation::Mul => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Number(v1 * v2));
            }
            Operation::Div => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Number(v1 / v2));
            }
            Operation::Mod => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Number(v1 % v2));
            }
            Operation::Set => {
                let r = self.stack.pop_ref(&self.locals)?;
                let val = self.stack.pop()?;
                self.locals.insert(r, val);
            }
            Operation::Eq => {
                let v2 = self.stack.pop()?;
                let v1 = self.stack.pop()?;
                self.stack.push(Value::Bool(v1.real_eq(&v2, &self.locals)));
            }
            Operation::Neq => {
                let v2 = self.stack.pop()?;
                let v1 = self.stack.pop()?;
                self.stack.push(Value::Bool(!v1.real_eq(&v2, &self.locals)));
            }
            Operation::Gt => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Bool(v1 > v2));
            }
            Operation::Lt => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Bool(v1 < v2));
            }
            Operation::GtEq => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Bool(v1 >= v2));
            }
            Operation::LtEq => {
                let v2 = self.stack.pop_num(&self.locals)?;
                let v1 = self.stack.pop_num(&self.locals)?;
                self.stack.push(Value::Bool(v1 <= v2));
            }
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
        let ts = match fun {
            Function::Interop(f) => {
                let fun = f.clone();
                fun(self);
                return Some(());
            }
            Function::Normal(ts) => ts.clone(),
        };
        self.eval(ts)
    }

    pub fn eval(&mut self, token: Vec<Token>) -> Option<()> {
        let Self {
            stack,
            locals,
            functions,
            ..
        } = self;
        let mut sub = Vm {
            code: &token,
            stack: MutCow::Borrowed(stack),
            locals: MutCow::Borrowed(locals),
            functions: MutCow::Borrowed(functions),
        };
        while let Some(()) = sub.next() {}
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
            Token::Def(d) => {
                let body = self.stack.pop_body(&self.locals)?;
                self.functions.insert(d.clone(), Function::Normal(body));
            }
            Token::Body(b) => self.stack.push(Value::Sequence(b.clone())),
            Token::If(t, f) => {
                let bool = self.stack.pop_bool(&self.locals)?;
                if bool {
                    self.eval(t.to_vec())?;
                } else if let Some(ts) = f {
                    self.eval(ts.to_vec())?;
                }
            }
            Token::Loop(l) => {
                let body = self.stack.pop_body(&self.locals)?;
                loop {
                    self.eval(body.clone())?;
                    if !self.stack.pop_bool(&self.locals)? {
                        break;
                    }
                    self.eval(l.clone())?;
                }
            }
        }

        self.code = &self.code[1..];
        Some(())
    }
}

fn main() {
    let file = match std::env::args().skip(1).next() {
        Some(f) => f,
        None => {
            eprintln!("no input file given");
            std::process::exit(1);
        }
    };
    let code = std::fs::read_to_string(file).unwrap();

    let st = Instant::now();
    let (i, tokens) = tokens(&code).unwrap();
    let took = st.elapsed();
    eprintln!("Parsing took {}µs\n\n", took.as_micros());

    if !i.trim().is_empty() && !i.trim_start().starts_with("//") {
        eprint!("invalid code {:?}", i.trim());
        return;
    }

    let mut vm = Vm {
        code: tokens.as_ref(),
        stack: MutCow::Owned(Stack { inner: vec![] }),
        locals: MutCow::Owned(Default::default()),
        functions: MutCow::Owned(std()),
    };
    let st = Instant::now();
    while let Some(()) = vm.next() {}
    std::io::stdout().flush().unwrap();
    let took = st.elapsed();
    eprintln!(
        "\n\nExecution took {}s {}ms {}µs",
        took.as_secs(),
        took.as_millis() % 1000,
        took.as_micros() % 1000
    );
}

macro_rules! interop {
    ($name:ident, $($body:tt)+) => {
        (stringify!($name).to_owned(), Function::Interop(Rc::new($($body)+)))
    };
}

fn std() -> HashMap<String, Function> {
    let map = [
        interop!(dbg, |vm| {
            println!("{:?}", vm.stack.pop());
            Some(())
        }),
        interop!(println, |vm| {
            let val = vm.stack.pop_str(&vm.locals)?;
            println!("{}", val);
            Some(())
        }),
        interop!(print, |vm| {
            let val = vm.stack.pop_str(&vm.locals)?;
            print!("{}", val);
            Some(())
        }),
        interop!(str, |vm| {
            let val = Value::String(vm.stack.pop_stringified(&vm.locals)?);
            vm.stack.push(val);
            Some(())
        }),
    ]
    .into_iter()
    .collect();
    map
}

fn tokens(i: &str) -> IResult<&str, Vec<Token>> {
    many0(token)(i)
}

fn token(mut i: &str) -> IResult<&str, Token> {
    // comments & spaces
    loop {
        let (ni, _) = space0(i)?;
        let (ni, _) = opt(alt((single_line_comment, multiline_comment)))(ni)?;
        if ni.len() != i.len() {
            i = ni;
        } else {
            break;
        }
    }
    // remove prepended space characters is present
    // let (i, _) = space0(i)?;

    alt((
        num,
        bool,
        variable,
        op,
        branch,
        loop_,
        map(body, |body| Token::Body(body)),
        map(parse_string, |str| Token::String(str)),
        map(preceded(char('$'), name), |fun| Token::Def(fun.into())),
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
            }
            Token::Call(ref s) if s == "end" => break,
            _ => c_branch.push(token),
        }
    }
    Ok((
        i,
        if let Some(bt) = b_true {
            Token::If(bt, Some(c_branch))
        } else {
            Token::If(c_branch, None)
        },
    ))
}

fn body(i: &str) -> IResult<&str, Vec<Token>> {
    delimited(char('('), tokens, preceded(space0, char(')')))(i)
}

fn name(i: &str) -> IResult<&str, &str> {
    recognize(tuple((alt((tag("_"), alpha1)), name_inner)))(i)
}

fn name_inner(i: &str) -> IResult<&str, &str> {
    i.split_at_position_complete(|x| !x.is_alphanumeric() && x != '_')
}

fn single_line_comment(i: &str) -> IResult<&str, &str> {
    recognize(tuple((tag("//"), opt(is_not("\r\n")))))(i)
}

fn multiline_comment(i: &str) -> IResult<&str, &str> {
    delimited(tag("/*"), take_until("*/"), cut(tag("*/")))(i)
}

fn op(i: &str) -> IResult<&str, Token> {
    let (i, o) = alt((
        tag("!="),
        tag("<="),
        tag(">="),
        recognize(one_of(".~#@+-*/%:=<>")),
    ))(i)?;

    Ok((
        i,
        Token::Operation(match o {
            "." => Operation::Dup,
            "#" => Operation::Drop,
            "~" => Operation::Swap,
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

impl Value {
    pub fn real_eq(&self, other: &Self, locals: &HashMap<String, Value>) -> bool {
        self.explode(locals) == other.explode(locals)
    }

    pub fn explode(&self, locals: &HashMap<String, Value>) -> Value {
        match self {
            Value::Ref(str) => locals.get(str).cloned().unwrap_or(Value::Undefined),
            x => x.clone(),
        }
    }
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
