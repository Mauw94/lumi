use crate::{
    interpreter::{LRes, Obj},
    lexer::{CodeLoc, Token},
};

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
pub enum LNum {
    Int(i64),
    Float(f64),
}

impl LNum {
    pub fn complete_binary_op(op: &Token, lv: f64, rv: f64) -> LRes<Obj> {
        match op {
            Token::Plus => return Ok(Obj::Num(LNum::Float(lv + rv))),
            Token::Minus => return Ok(Obj::Num(LNum::Float(lv - rv))),
            Token::Star => return Ok(Obj::Num(LNum::Float(lv * rv))),
            Token::Slash => return Ok(Obj::Num(LNum::Float(lv / rv))),
            _ => return Ok(Obj::Null),
        };
    }

    pub fn get_real_value(obj: Obj, code_loc: CodeLoc) -> Result<f64, LErr> {
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
