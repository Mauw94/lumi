use std::rc::Rc;

use crate::lexer::{CodeLoc, Token};

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

#[derive(Debug, Clone)]
pub enum Obj {
    Null,
    Bool(bool),
    Num(LNum),
    Seq(Seq),
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

impl Obj {
    fn is_same_type(&self, other: &Obj) -> bool {
        match (self, other) {
            (Obj::Null, Obj::Null)
            | (Obj::Bool(_), Obj::Bool(_))
            | (Obj::Num(_), Obj::Num(_))
            | (Obj::Seq(_), Obj::Seq(_)) => true,
            _ => false,
        }
    }

    fn is_same_value(&self, other: &Obj) -> bool {
        match (self, other) {
            (Obj::Bool(b1), Obj::Bool(b2)) => b1 == b2,
            (Obj::Num(n1), Obj::Num(n2)) => n1 == n2,
            (Obj::Seq(s1), Obj::Seq(s2)) => s1 == s2,
            _ => false,
        }
    }
}

// TODO: this shouldn't be an implementing on LNum, move this
impl LNum {
    pub fn complete_binary_op(
        op: &Token,
        lhs: Obj,
        rhs: Obj,
        code_loc_l: CodeLoc,
        code_loc_r: CodeLoc,
    ) -> LRes<Obj> {
        match op {
            Token::Plus => {
                let lv = LNum::get_num_value(lhs, code_loc_l)?;
                let rv = LNum::get_num_value(rhs, code_loc_r)?;
                return Ok(Obj::Num(LNum::Float(lv + rv)));
            }
            Token::Minus => {
                let lv = LNum::get_num_value(lhs, code_loc_l)?;
                let rv = LNum::get_num_value(rhs, code_loc_r)?;
                return Ok(Obj::Num(LNum::Float(lv - rv)));
            }
            Token::Star => {
                let lv = LNum::get_num_value(lhs, code_loc_l)?;
                let rv = LNum::get_num_value(rhs, code_loc_r)?;
                return Ok(Obj::Num(LNum::Float(lv * rv)));
            }
            Token::Slash => {
                let lv = LNum::get_num_value(lhs, code_loc_l)?;
                let rv = LNum::get_num_value(rhs, code_loc_r)?;
                return Ok(Obj::Num(LNum::Float(lv / rv)));
            }
            Token::EqualEqual => {
                let same_type = lhs.is_same_type(&rhs);
                if !same_type {
                    return Ok(Obj::Bool(false));
                }

                return Ok(Obj::Bool(lhs.is_same_value(&rhs)));
            }
            _ => return Ok(Obj::Null),
        };
    }

    fn get_num_value(obj: Obj, code_loc: CodeLoc) -> Result<f64, LErr> {
        match obj {
            Obj::Num(LNum::Int(i)) => Ok(i as f64),
            Obj::Num(LNum::Float(f)) => Ok(f),
            _ => Err(LErr::runtime_error(
                "A number was expected.".to_string(),
                code_loc,
            )),
        }
    }
}
