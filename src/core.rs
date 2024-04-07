use std::{
    cell::{Ref, RefCell, RefMut},
    fmt::Debug,
    rc::Rc,
};

use crate::{define, evaluate, lexer::CodeLoc, Builtin, Env, LocToken, LumiExpr};

#[derive(Debug)]
pub enum ErrorLoc {
    Lexing(CodeLoc, CodeLoc),
    Token(LocToken),
    Expr(CodeLoc, CodeLoc),
    Internal,
}

#[derive(Debug)]
pub struct LErr(String, ErrorLoc);
impl LErr {
    pub fn lexing_error(message: String, start: CodeLoc, end: CodeLoc) -> Self {
        LErr(message, ErrorLoc::Lexing(start, end))
    }

    pub fn parsing_error(message: String, token: LocToken) -> Self {
        LErr(message, ErrorLoc::Token(token))
    }

    pub fn runtime_error(message: String, start: CodeLoc, end: CodeLoc) -> Self {
        LErr(message, ErrorLoc::Expr(start, end))
    }

    pub fn internal_error(message: String) -> Self {
        LErr(message, ErrorLoc::Internal)
    }

    pub fn render(self: LErr, src: &str) -> String {
        let LErr(message, loc) = self;
        use std::fmt::Write;
        let mut out = String::new();
        write!(out, "\x1b[31m").ok();
        writeln!(out, "{}", message).ok();
        write!(out, "\x1b[0m").ok();
        match loc {
            ErrorLoc::Lexing(start, end) => {
                write_source_error(&mut out, src, &start, &end);
                write!(
                    out,
                    "\n\t\t(at [line {} index {}])",
                    start.line, start.index
                )
                .ok();
            }
            ErrorLoc::Token(token) => {
                write_source_error(&mut out, src, &token.start, &token.end);
                write!(
                    out,
                    "\n\tat {:?}\t(at [line {} index {}])",
                    token, token.start.line, token.start.index
                )
                .ok();
            }
            ErrorLoc::Expr(start, end) => {
                write_source_error(&mut out, src, &start, &end);
                write!(out, "\n\t(expr: line {} index {})", start.line, start.index).ok();
            }
            ErrorLoc::Internal => {
                write!(out, "\n").ok();
            }
        }
        out
    }
}

pub fn write_source_error(out: &mut String, src: &str, start: &CodeLoc, end: &CodeLoc) {
    use std::fmt::Write;

    let mut line = 1;
    let mut ended = false;
    for (i, c) in src.chars().enumerate() {
        if c == '\n' {
            if line >= start.line {
                write!(out, "{}", c).ok();
            }
            line += 1;
            if line > end.line {
                break;
            }
        } else {
            if i == start.index {
                write!(out, "\x1b[33m").ok();
            }
            if i == end.index {
                write!(out, "\x1b[0m").ok();
                ended = true;
            }
            if line >= start.line && line <= end.line {
                write!(out, "{}", c).ok();
            }
        }
    }
    if !ended {
        write!(out, "\x1b[0m").ok();
    }
}

pub fn try_borrow<'a, T>(r: &'a RefCell<T>) -> LRes<Ref<'a, T>> {
    match r.try_borrow() {
        Ok(r) => Ok(r),
        Err(e) => Err(LErr::internal_error(format!(
            "internal borrow error: {}",
            e
        ))),
    }
}

pub fn try_borrow_mut<'a, T>(r: &'a RefCell<T>) -> LRes<RefMut<'a, T>> {
    match r.try_borrow_mut() {
        Ok(r) => Ok(r),
        Err(e) => Err(LErr::internal_error(format!(
            "internal borrow error: {}",
            e
        ))),
    }
}

#[derive(Debug, Clone)]
pub enum Obj {
    Null,
    Bool(bool),
    Num(LNum),
    Seq(Seq),
    Output(String),
    Func(Func),
}

#[derive(Debug, Clone)]
pub enum Seq {
    String(Rc<String>),
    List(Rc<Vec<Obj>>),
}

pub type LRes<T> = Result<T, LErr>;

#[derive(Debug, Clone, PartialEq)]
pub enum LNum {
    Int(i64),
    Float(f64),
}

#[allow(dead_code)]
pub enum CompareType {
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectType {
    Int,
    Float,
    String,
    Bool,
    List,
    Function,
    None,
}

#[derive(Debug, Clone)]
pub enum Func {
    Builtin(Rc<dyn Builtin>),
    Closure(Closure),
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub params: Rc<Vec<Box<String>>>,
    pub body: Rc<LumiExpr>,
    // pub env: Rc<RefCell<Env>>,
}

impl Closure {
    pub fn call(
        &self,
        args: Vec<Obj>,
        _closure: &Rc<RefCell<Env>>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> Result<Obj, LErr> {
        // FIXME: add top env (closure)
        // when var is not found in func's env we look to the inner env
        let env = Rc::new(RefCell::new(Env::new())); // atm new env has no knowledge of top env

        for (i, p) in self.params.iter().enumerate() {
            let obj = match args.get(i) {
                Some(o) => o,
                None => {
                    return Err(LErr::runtime_error(
                        "Did not find argument value.".to_string(),
                        start,
                        end,
                    ))
                }
            };
            define(&env, p.to_string(), ObjectType::None, obj.clone())?;
        }

        return Ok(evaluate(&env, &self.body)?);
    }
}

impl ObjectType {
    pub fn get_type_name(&self) -> &str {
        match self {
            ObjectType::Int => "int",
            ObjectType::Float => "float",
            ObjectType::String => "str",
            ObjectType::Bool => "bool",
            ObjectType::Function => "function",
            ObjectType::List => "list",
            ObjectType::None => "none",
        }
    }
}
impl Obj {
    pub fn is_same_type(&self, other: &Obj) -> bool {
        match (self, other) {
            (Obj::Null, Obj::Null)
            | (Obj::Bool(_), Obj::Bool(_))
            | (Obj::Num(_), Obj::Num(_))
            | (Obj::Seq(_), Obj::Seq(_)) => true,
            _ => false,
        }
    }

    pub fn is_type(&self, obj_type: &ObjectType) -> bool {
        match obj_type {
            ObjectType::Int => self.is_int(),
            ObjectType::Float => self.is_float(),
            ObjectType::String => self.is_string(),
            ObjectType::Bool => self.is_bool(),
            ObjectType::List => self.is_list(),
            ObjectType::Function => self.is_function(),
            ObjectType::None => true,
        }
    }

    pub fn is_same_value(&self, other: &Obj) -> bool {
        match (self, other) {
            (Obj::Bool(b1), Obj::Bool(b2)) => b1 == b2,
            (Obj::Num(n1), Obj::Num(n2)) => LNum::compare_lnums(n1, n2, CompareType::Equal),
            (Obj::Seq(seq1), Obj::Seq(seq2)) => match (seq1, seq2) {
                (Seq::String(s1), Seq::String(s2)) => s1 == s2,
                (Seq::List(_), Seq::List(_)) => todo!(),
                _ => false,
            },
            _ => false,
        }
    }

    pub fn get_type_name(&self) -> &str {
        match self {
            Obj::Null => "nill",
            Obj::Bool(_) => "bool",
            Obj::Num(n) => match n {
                LNum::Int(_) => "int",
                LNum::Float(_) => "float",
            },
            Obj::Seq(sq) => match sq {
                Seq::String(_) => "str",
                Seq::List(_) => "list",
            },
            Obj::Output(_) => "nil",
            Obj::Func(_) => "function",
        }
    }
    fn is_int(&self) -> bool {
        match self {
            Obj::Num(n) => match n {
                LNum::Int(_) => true,
                LNum::Float(_) => false,
            },
            _ => false,
        }
    }

    fn is_float(&self) -> bool {
        match self {
            Obj::Num(n) => match n {
                LNum::Int(_) => false,
                LNum::Float(_) => true,
            },
            _ => false,
        }
    }

    fn is_string(&self) -> bool {
        match self {
            Obj::Seq(sq) => match sq {
                Seq::String(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            Obj::Bool(_) => true,
            _ => false,
        }
    }

    fn is_function(&self) -> bool {
        match &self {
            Obj::Func(_) => true,
            _ => false,
        }
    }

    fn is_list(&self) -> bool {
        match &self {
            Obj::Seq(sq) => match sq {
                Seq::List(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn print_value(&self) {
        match &self {
            Obj::Null => {}
            Obj::Bool(v) => println!("{}", v),
            Obj::Num(v) => match v {
                LNum::Int(i) => println!("{}", i),
                LNum::Float(f) => println!("{}", f),
            },
            Obj::Seq(v) => match v {
                Seq::String(s) => println!("\"{}\"", s),
                Seq::List(objs) => {
                    for obj in objs.clone().iter() {
                        obj.print_value();
                    }
                }
            },
            Obj::Output(v) => println!("{}", v),
            Obj::Func(f) => match f {
                Func::Closure(_c) => {
                    // for p in c.params.iter() {
                    //     println!("param: {}", p);
                    // }
                    // print!("body {:?}", c.body);
                }
                Func::Builtin(_) => {}
            },
        }
    }

    pub fn i64(n: i64) -> Self {
        Obj::Num(LNum::Int(n))
    }

    pub fn f64(n: f64) -> Self {
        Obj::Num(LNum::Float(n))
    }
}

impl LNum {
    pub fn compare_lnums(num1: &LNum, num2: &LNum, compare_type: CompareType) -> bool {
        let f1 = match num1 {
            LNum::Float(f) => *f,
            LNum::Int(i) => *i as f64,
        };
        let f2 = match num2 {
            LNum::Float(f) => *f,
            LNum::Int(i) => *i as f64,
        };

        match compare_type {
            CompareType::Equal => f1 == f2,
            CompareType::Greater => f1 > f2,
            CompareType::GreaterEqual => f1 >= f2,
            CompareType::Less => f1 < f2,
            CompareType::LessEqual => f1 <= f2,
        }
    }

    pub fn get_num_val_usize(&self) -> usize {
        match &self {
            LNum::Int(i) => *i as usize,
            LNum::Float(f) => *f as usize,
        }
    }
}
