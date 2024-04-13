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
pub enum LErr {
    Throw(String, ErrorLoc),
    Return(Obj),
}

impl LErr {
    pub fn lexing_error(message: String, start: CodeLoc, end: CodeLoc) -> Self {
        LErr::Throw(message, ErrorLoc::Lexing(start, end))
    }

    pub fn parsing_error(message: String, token: LocToken) -> Self {
        LErr::Throw(message, ErrorLoc::Token(token))
    }

    pub fn runtime_error(message: String, start: CodeLoc, end: CodeLoc) -> Self {
        LErr::Throw(message, ErrorLoc::Expr(start, end))
    }

    pub fn internal_error(message: String) -> Self {
        LErr::Throw(message, ErrorLoc::Internal)
    }

    pub fn render(self: LErr, src: &str) -> String {
        match self {
            LErr::Throw(message, loc) => {
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
            LErr::Return(e) => format!("break {:?}", e),
        }
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
    Func(Box<Func>),
    Struct(Struct),
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
    Struct,
    None,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub env: Rc<RefCell<Env>>,
    pub params: Rc<Vec<Box<String>>>,
    pub body: Rc<Vec<Box<LumiExpr>>>,
}

#[derive(Debug, Clone)]
pub enum Func {
    Builtin(Rc<dyn Builtin>),
    Closure(Box<Closure>),
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub params: Rc<Vec<Box<String>>>,
    pub body: Rc<Vec<Box<LumiExpr>>>,
}

impl Closure {
    pub fn call(
        &mut self,
        args: Vec<Obj>,
        closure: &Rc<RefCell<Env>>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> Result<Obj, LErr> {
        let env = Rc::new(RefCell::new(Env::new(Some(closure.clone()))));

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

        for expr in self.body.iter() {
            match evaluate(&env, expr) {
                Ok(_) => {} // just continue evaluating
                Err(err) => match err {
                    LErr::Throw(_, _) => return Err(err),
                    LErr::Return(x) => {
                        return Ok(x);
                    }
                },
            }
        }

        Ok(Obj::Null)
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
            ObjectType::Struct => "struct",
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
            ObjectType::Struct => self.is_struct(),
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
            Obj::Struct(_) => "struct",
        }
    }

    pub fn get_int_val(&self) -> Result<i64, LErr> {
        match self {
            Obj::Num(lnum) => match lnum {
                LNum::Int(i) => Ok(*i),
                _ => Err(LErr::internal_error(
                    "Expected Num to be of type LNum::int".to_string(),
                )),
            },
            _ => Err(LErr::internal_error(
                "Expected obj to be of type Num".to_string(),
            )),
        }
    }

    pub fn get_str_value(&self) -> Result<String, LErr> {
        match self {
            Obj::Seq(seq) => match seq {
                Seq::String(s) => Ok(s.to_string()),
                _ => Err(LErr::internal_error(
                    "Expect Seq to be of type Str".to_string(),
                )),
            },
            _ => Err(LErr::internal_error(
                "Expect Seq to be of type Str".to_string(),
            )),
        }
    }

    pub fn get_default_value(object_type: &ObjectType) -> Result<Obj, LErr> {
        match object_type {
            ObjectType::Int => Ok(Obj::Num(LNum::default_int())),
            ObjectType::Float => Ok(Obj::Num(LNum::default_float())),
            ObjectType::String => Ok(Obj::Seq(Seq::String(Rc::new("".to_string())))),
            ObjectType::Bool => Ok(Obj::Bool(false)),
            ObjectType::List => Ok(Obj::Seq(Seq::List(Rc::new(Vec::new())))),
            _ => Err(LErr::internal_error(format!(
                "Object type {} does not have a defautl value.",
                object_type.get_type_name()
            ))),
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

    fn is_struct(&self) -> bool {
        match &self {
            Obj::Struct(_) => true,
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
            Obj::Func(f) => match &**f {
                Func::Closure(_c) => {
                    // for p in c.params.iter() {
                    //     println!("param: {}", p);
                    // }
                    // print!("body {:?}", c.body);
                }
                Func::Builtin(_) => {}
            },
            Obj::Struct(s) => println!("{:?}", s),
        }
    }

    pub fn i64(n: i64) -> Self {
        Obj::Num(LNum::Int(n))
    }

    pub fn f64(n: f64) -> Self {
        Obj::Num(LNum::Float(n))
    }
}

impl PartialEq for Obj {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Num(l0), Self::Num(r0)) => l0 == r0,
            (Self::Seq(l0), Self::Seq(r0)) => match (l0, r0) {
                (Seq::String(s1), Seq::String(s2)) => s1 == s2,
                (Seq::List(_), Seq::List(_)) => todo!(),
                _ => todo!(),
            },
            (Self::Output(l0), Self::Output(r0)) => l0 == r0,
            // (Self::Func(l0), Self::Func(r0)) => l0 == r0,
            // (Self::Struct(l0), Self::Struct(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
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

    pub fn default_int() -> LNum {
        LNum::Int(0)
    }

    pub fn default_float() -> LNum {
        LNum::Float(0.0)
    }
}
