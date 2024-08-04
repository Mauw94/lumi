use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt::Debug,
    rc::Rc,
};

use crate::{
    define_var, evaluate, lexer::CodeLoc, Builtin, Env, LNum, LocToken, LumiExpr, Namespace,
};

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
    Byte,
    String,
    Bool,
    List,
    Function,
    Struct,
    None,
    Namespace,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub env: Rc<RefCell<Env>>,
    pub params: Rc<Vec<Box<String>>>,
    pub functions: HashMap<String, LumiExpr>, // TODO: LumiExpr can be made Rc<LumiExpr> so we don't need to clone?
    pub properties: Vec<String>,
    // pub body: Rc<Vec<Box<LumiExpr>>>,
}

impl Struct {
    pub fn find_method(
        &mut self,
        method: &String,
        start: CodeLoc,
        end: CodeLoc,
    ) -> Result<LumiExpr, LErr> {
        match self.functions.get(method) {
            Some(m) => Ok(m.clone()),
            None => Err(LErr::runtime_error(
                format!("Did not find method '{}'", method),
                start,
                end,
            )),
        }
    }

    pub fn is_method(&self, value: &String) -> bool {
        return match self.functions.get(value) {
            Some(_) => true,
            None => false,
        };
    }

    pub fn is_property(&self, value: &String) -> bool {
        self.properties.contains(value)
    }
}

#[derive(Debug, Clone)]
pub enum Func {
    Builtin(Rc<dyn Builtin>),
    Closure(Box<Closure>),
    Namespace(Rc<dyn Namespace>),
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
        let env = Rc::clone(closure);

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
            define_var(&env, p.to_string(), ObjectType::None, obj.clone())?;
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
            ObjectType::Namespace => "namespace",
            ObjectType::Byte => "byte",
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
            ObjectType::Namespace => todo!(),
            ObjectType::Byte => todo!(),
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
                LNum::Int(lint) => match lint {
                    crate::LInt::Small(_) => "int (small)",
                    crate::LInt::Big(_) => "int (big)",
                    crate::LInt::Long(_) => "int (long)",
                },
                LNum::Float(_) => "float",
                LNum::Byte(_) => "byte",
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
                LNum::Int(lint) => match lint {
                    crate::LInt::Small(i) => Ok(*i as i64),
                    crate::LInt::Big(i) => Ok(*i as i64),
                    crate::LInt::Long(i) => Ok(*i as i64),
                },
                _ => Err(LErr::internal_error(
                    "Expected Num to be of type LNum::int".to_string(),
                )),
            },
            _ => Err(LErr::internal_error(
                "Expected obj to be of type Num".to_string(),
            )),
        }
    }

    pub fn get_float_val(&self) -> Result<f32, LErr> {
        match self {
            Obj::Num(lnum) => match lnum {
                LNum::Float(i) => Ok(*i),
                _ => Err(LErr::internal_error(
                    "Expected Num to be of type LNum::float".to_string(),
                )),
            },
            _ => Err(LErr::internal_error(
                "Expected obj to be of type Num".to_string(),
            )),
        }
    }

    pub fn get_byte_val(&self) -> Result<u8, LErr> {
        match self {
            Obj::Num(lnum) => match lnum {
                LNum::Byte(b) => Ok(*b),
                _ => Err(LErr::internal_error(
                    "Expected Num to be of type LNum::byte".to_string(),
                )),
            },
            _ => Err(LErr::internal_error(
                "Expected obj to be of type Num".to_string(),
            )),
        }
    }

    pub fn get_str_val(&self) -> Result<String, LErr> {
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

    pub fn get_num_value(&self, start: CodeLoc, end: CodeLoc) -> Result<f32, LErr> {
        match self {
            Obj::Num(LNum::Int(i)) => match i {
                crate::LInt::Small(i) => Ok(*i as f32),
                crate::LInt::Big(i) => Ok(*i as f32),
                crate::LInt::Long(i) => Ok(*i as f32),
            },
            Obj::Num(LNum::Float(f)) => Ok(*f),
            _ => Err(LErr::runtime_error(
                "A number was expected.".to_string(),
                start,
                end,
            )),
        }
    }

    pub fn get_list_val(&self) -> Result<Vec<Obj>, LErr> {
        match self {
            Obj::Seq(Seq::List(lst)) => Ok(lst.to_vec()),
            _ => Err(LErr::internal_error(
                "Expect Seq to be of type list".to_string(),
            )),
        }
    }

    pub fn get_object_type(&self) -> Result<ObjectType, LErr> {
        match self {
            Obj::Null => Ok(ObjectType::None),
            Obj::Bool(_) => Ok(ObjectType::Bool),
            Obj::Num(lnum) => match lnum {
                LNum::Int(_) => Ok(ObjectType::Int),
                LNum::Float(_) => Ok(ObjectType::Float),
                LNum::Byte(_) => Ok(ObjectType::Byte),
            },
            Obj::Seq(seq) => match seq {
                Seq::String(_) => Ok(ObjectType::String),
                Seq::List(_) => Ok(ObjectType::List),
            },
            Obj::Output(_) => Ok(ObjectType::None),
            Obj::Func(_) => Ok(ObjectType::Function),
            Obj::Struct(_) => Ok(ObjectType::Struct),
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
                _ => false,
            },
            _ => false,
        }
    }

    fn is_float(&self) -> bool {
        match self {
            Obj::Num(n) => match n {
                LNum::Float(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Obj::Seq(sq) => match sq {
                Seq::String(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Obj::Num(_) => true,
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

    pub fn is_list(&self) -> bool {
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
                LNum::Int(i) => match i {
                    crate::LInt::Small(i) => println!("{}", i),
                    crate::LInt::Big(i) => println!("{}", i),
                    crate::LInt::Long(i) => println!("{}", i),
                },
                LNum::Float(f) => println!("{}", f),
                LNum::Byte(b) => println!("{}", b),
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
                Func::Namespace(_) => {}
            },
            Obj::Struct(s) => println!("{:?}", s),
        }
    }

    pub fn format_value(&self) -> String {
        match &self {
            Obj::Null => {
                format!("null")
            }
            Obj::Bool(v) => format!("{}", v),
            Obj::Num(v) => match v {
                LNum::Int(i) => match i {
                    crate::LInt::Small(i) => format!("{}", i),
                    crate::LInt::Big(i) => format!("{}", i),
                    crate::LInt::Long(i) => format!("{}", i),
                },
                LNum::Float(f) => format!("{}", f),
                LNum::Byte(b) => format!("{}", b),
            },
            Obj::Seq(v) => match v {
                Seq::String(s) => format!("{}", s),
                Seq::List(objs) => {
                    let mut result = Vec::new();
                    for obj in objs.clone().iter() {
                        result.push(obj.format_value());
                    }

                    result.join(" ")
                }
            },
            Obj::Output(v) => format!("{}", v),
            Obj::Func(f) => match &**f {
                Func::Closure(_c) => {
                    // for p in c.params.iter() {
                    //     println!("param: {}", p);
                    // }
                    // print!("body {:?}", c.body);
                    format!("null")
                }
                Func::Builtin(_) => {
                    format!("null")
                }
                Func::Namespace(_) => {
                    format!("null")
                }
            },
            Obj::Struct(s) => format!("{:?}", s),
        }
    }

    pub fn i16(n: i16) -> Self {
        Obj::Num(LNum::Int(crate::LInt::Small(n)))
    }
    pub fn i32(n: i32) -> Self {
        Obj::Num(LNum::Int(crate::LInt::Big(n)))
    }
    pub fn i64(n: i64) -> Self {
        Obj::Num(LNum::Int(crate::LInt::Long(n)))
    }
    pub fn f32(n: f32) -> Self {
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

pub fn get_str_from_args_vec_obj(index: usize, args: &Vec<Obj>) -> Result<String, LErr> {
    match args.get(index) {
        Some(o) => {
            if o.is_type(&ObjectType::String) {
                Ok(o.get_str_val()?)
            } else {
                Err(LErr::internal_error(format!(
                    "Argument {:?} is not of type str.",
                    o
                )))
            }
        }
        None => Err(LErr::internal_error(format!("Did not find an argument."))),
    }
}

pub fn get_int_from_arg_obj(index: usize, args: &Vec<Obj>) -> Result<i64, LErr> {
    match args.get(index) {
        Some(o) => {
            if o.is_type(&ObjectType::Int) {
                Ok(o.get_int_val()?)
            } else {
                Err(LErr::internal_error(format!(
                    "Argument {:?} is not of type int.",
                    o
                )))
            }
        }
        None => Err(LErr::internal_error(format!("Did not find an argument."))),
    }
}

pub fn get_float_from_arg_obj(index: usize, args: &Vec<Obj>) -> Result<f32, LErr> {
    match args.get(index) {
        Some(o) => {
            if o.is_type(&ObjectType::Float) {
                Ok(o.get_float_val()?)
            } else {
                Err(LErr::internal_error(format!(
                    "Argument {:?} is not of type float.",
                    o
                )))
            }
        }
        None => Err(LErr::internal_error(format!("Did not find an argument."))),
    }
}

pub fn get_list_from_arg_obj(index: usize, args: &Vec<Obj>) -> Result<Vec<Obj>, LErr> {
    match args.get(index) {
        Some(o) => {
            if o.is_type(&ObjectType::List) {
                Ok(o.get_list_val()?)
            } else {
                Err(LErr::internal_error(format!(
                    "Argument {:?} is not of type list.",
                    o
                )))
            }
        }
        None => Err(LErr::internal_error(format!("Did not find an argument."))),
    }
}

pub fn get_str_from_arg_obj(index: usize, args: &Vec<Obj>) -> Result<String, LErr> {
    match args.get(index) {
        Some(o) => {
            if o.is_string() {
                Ok(o.get_str_val()?)
            } else {
                Err(LErr::internal_error(format!(
                    "Argument {:?} is not of type str.",
                    o
                )))
            }
        }
        None => Err(LErr::internal_error(format!("Did not find an argument."))),
    }
}
