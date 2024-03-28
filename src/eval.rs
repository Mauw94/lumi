use crate::{
    core::{LErr, LNum, LRes, Obj},
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
        Token::Plus => {
            let lv = get_num_value(lhs, code_loc_l)?;
            let rv = get_num_value(rhs, code_loc_r)?;
            return Ok(Obj::Num(LNum::Float(lv + rv)));
        }
        Token::Minus => {
            let lv = get_num_value(lhs, code_loc_l)?;
            let rv = get_num_value(rhs, code_loc_r)?;
            return Ok(Obj::Num(LNum::Float(lv - rv)));
        }
        Token::Star => {
            let lv = get_num_value(lhs, code_loc_l)?;
            let rv = get_num_value(rhs, code_loc_r)?;
            return Ok(Obj::Num(LNum::Float(lv * rv)));
        }
        Token::Slash => {
            let lv = get_num_value(lhs, code_loc_l)?;
            let rv = get_num_value(rhs, code_loc_r)?;
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
