use std::rc::Rc;

use crate::lexer::CodeLoc;

// TODO: extend this later to not only show on which line number, but also which position and which expression went wrong while parsing.
#[derive(Debug)]
pub struct LErr(String, CodeLoc);
impl LErr {
    pub fn lexing_error(message: String, code_loc: CodeLoc) -> Self {
        LErr(message, code_loc)
    }

    pub fn parsing_error(message: String, code_loc: CodeLoc) -> Self {
        LErr(message, code_loc)
    }

    pub fn runtime_error(message: String, code_loc: CodeLoc) -> Self {
        LErr(message, code_loc)
    }

    // TODO type of errors
    pub fn render(self: LErr) {
        let LErr(message, code_loc) = self;
        println!(
            "ERROR: at line [{}] index: [{}]: '{}'",
            code_loc.line, code_loc.index, message
        );
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    Null,
    Bool(bool),
    Num(LNum),
    Seq(Seq),
    Output(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Seq {
    String(Rc<String>),
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
    None,
}

impl ObjectType {
    pub fn get_type_name(&self) -> &str {
        match self {
            ObjectType::Int => "int",
            ObjectType::Float => "float",
            ObjectType::String => "str",
            ObjectType::Bool => "bool",
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
            ObjectType::List => todo!(),
            ObjectType::None => true,
        }
    }

    pub fn is_same_value(&self, other: &Obj) -> bool {
        match (self, other) {
            (Obj::Bool(b1), Obj::Bool(b2)) => b1 == b2,
            (Obj::Num(n1), Obj::Num(n2)) => LNum::compare_lnums(n1, n2, CompareType::Equal),
            (Obj::Seq(s1), Obj::Seq(s2)) => s1 == s2,
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
            },
            Obj::Output(_) => "nil",
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
            },
            Obj::Output(v) => println!("{}", v),
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
}
