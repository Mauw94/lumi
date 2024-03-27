use crate::{core::LErr, lexer::Token, parser::Expr};

#[derive(Debug, Clone)]
pub enum Obj {
    Null,
    Bool(bool),
    Num(LNum),
    Seq(Seq),
}

#[derive(Debug, Clone)]
pub enum LNum {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone)]
pub enum Seq {
    String(String),
}

pub type LRes<T> = Result<T, LErr>;

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&self, expr: &Expr) -> LRes<Obj> {
        match &expr {
            Expr::Sequence(xs) => {
                // let mut ret = Obj::Null;
                // for (i, x) in xs[..xs.len() - 1].iter().enumerate() {
                //     ret = self.eval(x)?;
                // }
                // Ok(ret)

                // sequence is nested
                self.eval(xs.last().unwrap())
                //Ok(Obj::Null)
            }
            Expr::Int(v) => Ok(Obj::Num(LNum::Int(*v))),
            Expr::Float(v) => Ok(Obj::Num(LNum::Float(*v))),
            Expr::Identifier(v) => Ok(Obj::Seq(Seq::String(v.to_string()))),
            Expr::Unary(t, v) => {
                let rvalue = self.eval(v)?;

                match t {
                    Token::Bang => {
                        return Ok(Obj::Bool(self.is_truthy(rvalue)));
                    }
                    Token::Minus => match rvalue {
                        Obj::Num(lnum) => match lnum {
                            LNum::Int(v) => return Ok(Obj::Num(LNum::Int(-v))),
                            LNum::Float(v) => return Ok(Obj::Num(LNum::Float(-v))),
                        },
                        _ => {
                            return Err(LErr::runtime_error(
                                "Operand must be a number".to_string(),
                                1,
                            ))
                        }
                    },
                    _ => Ok(Obj::Null),
                }
            }
            Expr::Logical(_, _, _) => todo!(),
            Expr::Binary(lv, op, rv) => {
                let lvalue = self.eval(lv)?;
                let rvalue = self.eval(rv)?;

                // TODO: make some function in Num that can combined multiple numbers into 1 Obj::Num
                let real_l_v = match lvalue {
                    Obj::Num(value) => match value {
                        LNum::Int(i) => i as f64,
                        LNum::Float(f) => f,
                    },
                    _ => 0 as f64, // should be error
                };
                let real_r_v = match rvalue {
                    Obj::Num(value) => match value {
                        LNum::Int(i) => i as f64,
                        LNum::Float(f) => f,
                    },
                    _ => 0 as f64, // should be error
                };

                match op {
                    Token::Plus => return Ok(Obj::Num(LNum::Float(real_l_v + real_r_v))),
                    Token::Minus => return Ok(Obj::Num(LNum::Float(real_l_v - real_r_v))),
                    Token::Star => return Ok(Obj::Num(LNum::Float(real_l_v * real_r_v))),
                    Token::Slash => return Ok(Obj::Num(LNum::Float(real_l_v / real_r_v))),
                    _ => return Ok(Obj::Null),
                };
            }
            Expr::Var(_) => todo!(),
            Expr::Assign(_, _) => todo!(),
        }
    }

    fn is_truthy(&self, obj: Obj) -> bool {
        match obj {
            Obj::Bool(b) => b,
            Obj::Null => false,
            _ => false,
        }
    }
}
