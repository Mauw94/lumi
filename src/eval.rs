use std::rc::Rc;

use crate::{
    core::{CompareType, LErr, LNum, LRes, Obj},
    lexer::{CodeLoc, Token},
    Seq,
};

pub fn exec_binary_op(
    op: &Token,
    lhs: Obj,
    rhs: Obj,
    l_start: CodeLoc,
    l_end: CodeLoc,
    r_start: CodeLoc,
    r_end: CodeLoc,
) -> LRes<Obj> {
    match op {
        Token::Plus => {
            if lhs.is_string() && rhs.is_number() {
                let rv = rhs.get_num_value(r_start, l_start)?.to_string();
                let lv = lhs.get_str_val()?;
                return Ok(Obj::Seq(Seq::String(Rc::new(lv + &rv))));
            } else if lhs.is_number() && rhs.is_string() {
                let lv = lhs.get_num_value(l_start, l_end)?.to_string();
                let rv: String = rhs.get_str_val()?;
                return Ok(Obj::Seq(Seq::String(Rc::new(lv + &rv))));
            } else {
                let lv = lhs.get_num_value(l_start, l_end)?;
                let rv = rhs.get_num_value(r_start, r_end)?;
                let res = lv + rv;
                return return_correct_lnum_type(res);
            }
        }
        Token::Minus => {
            let lv = lhs.get_num_value(l_start, l_end)?;
            let rv = rhs.get_num_value(r_start, r_end)?;
            let res = lv - rv;
            return return_correct_lnum_type(res);
        }
        Token::Star => {
            let lv = lhs.get_num_value(l_start, l_end)?;
            let rv = rhs.get_num_value(r_start, r_end)?;
            let res = lv * rv;
            return return_correct_lnum_type(res);
        }
        Token::Slash => {
            let lv = lhs.get_num_value(l_start, l_end)?;
            let rv = rhs.get_num_value(r_start, r_end)?;
            let res = lv / rv;
            return return_correct_lnum_type(res);
        }
        Token::EqualEqual => {
            let same_type = lhs.is_same_type(&rhs);
            if !same_type {
                return Ok(Obj::Bool(false));
            }

            return Ok(Obj::Bool(lhs.is_same_value(&rhs)));
        }
        Token::Greater => match (lhs, rhs) {
            (Obj::Num(n1), Obj::Num(n2)) => {
                return Ok(Obj::Bool(LNum::compare_lnums(
                    &n1,
                    &n2,
                    CompareType::Greater,
                )))
            }
            _ => {
                return Err(LErr::runtime_error(
                    "Operands must be numbers.".to_string(),
                    l_start,
                    l_end,
                ))
            }
        },
        Token::GreaterEqual => match (lhs, rhs) {
            (Obj::Num(n1), Obj::Num(n2)) => {
                return Ok(Obj::Bool(LNum::compare_lnums(
                    &n1,
                    &n2,
                    CompareType::GreaterEqual,
                )))
            }
            _ => {
                return Err(LErr::runtime_error(
                    "Operands must be numbers.".to_string(),
                    l_start,
                    l_end,
                ))
            }
        },
        Token::Less => match (lhs, rhs) {
            (Obj::Num(n1), Obj::Num(n2)) => {
                return Ok(Obj::Bool(LNum::compare_lnums(&n1, &n2, CompareType::Less)))
            }
            _ => {
                return Err(LErr::runtime_error(
                    "Operands must be numbers.".to_string(),
                    l_start,
                    l_end,
                ))
            }
        },
        Token::LessEqual => match (lhs, rhs) {
            (Obj::Num(n1), Obj::Num(n2)) => {
                return Ok(Obj::Bool(LNum::compare_lnums(
                    &n1,
                    &n2,
                    CompareType::LessEqual,
                )))
            }
            _ => {
                return Err(LErr::runtime_error(
                    "Operands must be numbers.".to_string(),
                    l_start,
                    l_end,
                ))
            }
        },
        _ => return Ok(Obj::Null),
    };
}

fn return_correct_lnum_type(res: f32) -> LRes<Obj> {
    if res.fract() == 0.0 {
        Ok(Obj::Num(LNum::Int(res as i32)))
    } else {
        Ok(Obj::Num(LNum::Float(res)))
    }
}
