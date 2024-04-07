use crate::{
    core::{CompareType, LErr, LNum, LRes, Obj},
    lexer::{CodeLoc, Token},
};

pub fn exec_binary_op(
    op: &Token,
    lhs: Obj,
    rhs: Obj,
    code_loc_l: CodeLoc,
    code_loc_r: CodeLoc,
) -> LRes<Obj> {
    match op {
        // FIXME start and end loc
        Token::Plus => {
            let lv = get_num_value(lhs, code_loc_l, code_loc_r)?;
            let rv = get_num_value(rhs, code_loc_r, code_loc_r)?;
            return Ok(Obj::Num(LNum::Float(lv + rv)));
        }
        Token::Minus => {
            let lv = get_num_value(lhs, code_loc_l, code_loc_r)?;
            let rv = get_num_value(rhs, code_loc_r, code_loc_r)?;
            return Ok(Obj::Num(LNum::Float(lv - rv)));
        }
        Token::Star => {
            let lv = get_num_value(lhs, code_loc_l, code_loc_r)?;
            let rv = get_num_value(rhs, code_loc_r, code_loc_r)?;
            return Ok(Obj::Num(LNum::Float(lv * rv)));
        }
        Token::Slash => {
            let lv = get_num_value(lhs, code_loc_l, code_loc_r)?;
            let rv = get_num_value(rhs, code_loc_r, code_loc_r)?;
            return Ok(Obj::Num(LNum::Float(lv / rv)));
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
                    code_loc_l,
                    code_loc_r,
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
                    code_loc_l,
                    code_loc_r,
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
                    code_loc_l,
                    code_loc_r,
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
                    code_loc_l,
                    code_loc_r,
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
