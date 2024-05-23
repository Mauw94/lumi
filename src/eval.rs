use crate::{
    core::{CompareType, LErr, LNum, LRes, Obj},
    lexer::{CodeLoc, Token},
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
        // TODO: fix this so a str and a number can be concatenated
        Token::Plus => {
            // TODO: move to separate func
            if lhs.is_string() || rhs.is_string() {

            }

            let lv = get_num_value(lhs, l_start, l_end)?;
            let rv = get_num_value(rhs, r_start, r_end)?;
            let res = lv + rv;
            if res.fract() == 0.0 {
                return Ok(Obj::Num(LNum::Int(res as i64)));
            } else {
                return Ok(Obj::Num(LNum::Float(res)));
            }
        }
        Token::Minus => {
            let lv = get_num_value(lhs, l_start, l_end)?;
            let rv = get_num_value(rhs, r_start, r_end)?;
            let res = lv - rv;
            if res.fract() == 0.0 {
                return Ok(Obj::Num(LNum::Int(res as i64)));
            } else {
                return Ok(Obj::Num(LNum::Float(res)));
            }
        }
        Token::Star => {
            let lv = get_num_value(lhs, l_start, l_end)?;
            let rv = get_num_value(rhs, r_start, r_end)?;
            let res = lv * rv;
            if res.fract() == 0.0 {
                return Ok(Obj::Num(LNum::Int(res as i64)));
            } else {
                return Ok(Obj::Num(LNum::Float(res)));
            }
        }
        Token::Slash => {
            let lv = get_num_value(lhs, l_start, l_end)?;
            let rv = get_num_value(rhs, r_start, r_end)?;
            let res = lv / rv;
            if res.fract() == 0.0 {
                return Ok(Obj::Num(LNum::Int(res as i64)));
            } else {
                return Ok(Obj::Num(LNum::Float(res)));
            }
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

fn get_num_value(obj: Obj, start: CodeLoc, end: CodeLoc) -> Result<f64, LErr> {
    match obj {
        Obj::Num(LNum::Int(i)) => Ok(i as f64),
        Obj::Num(LNum::Float(f)) => Ok(f),
        _ => Err(LErr::runtime_error(
            "A number was expected.".to_string(),
            start,
            end,
        )),
    }
}
