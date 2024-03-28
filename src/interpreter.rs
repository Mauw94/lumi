use crate::{
    core::{LErr, LNum},
    lexer::Token,
    parser::{Expr, LiteralValue, LumiExpr},
};

#[derive(Debug, Clone)]
pub enum Obj {
    Null,
    Bool(bool),
    Num(LNum),
    Seq(Seq),
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

    pub fn eval(&self, expr: &LumiExpr) -> LRes<Obj> {
        match &expr.expr {
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
            Expr::Literal(literal) => match literal {
                LiteralValue::True => Ok(Obj::Bool(true)),
                LiteralValue::False => Ok(Obj::Bool(false)),
                LiteralValue::Nil => Ok(Obj::Null),
            },
            Expr::Unary(t, expr) => {
                let rvalue = self.eval(expr)?;

                match t {
                    Token::Bang => {
                        return Ok(Obj::Bool(!self.is_truthy(rvalue)));
                    }
                    Token::Minus => match rvalue {
                        Obj::Num(lnum) => match lnum {
                            LNum::Int(v) => return Ok(Obj::Num(LNum::Int(-v))),
                            LNum::Float(v) => return Ok(Obj::Num(LNum::Float(-v))),
                        },
                        _ => {
                            return Err(LErr::runtime_error(
                                "Operand must be a number".to_string(),
                                expr.start,
                            ))
                        }
                    },
                    _ => Ok(Obj::Null),
                }
            }
            Expr::Logical(l_expr, op, r_expr) => {
                let lhs = self.eval(l_expr)?;

                if op == &Token::Or {
                    if self.is_truthy(lhs) {
                        return Ok(Obj::Bool(true));
                    }
                } else {
                    if !self.is_truthy(lhs.clone()) {
                        return Ok(lhs);
                    }
                }

                return self.eval(&r_expr);
            }
            Expr::Binary(lv, op, rv) => {
                let lvalue = self.eval(lv)?;
                let rvalue = self.eval(rv)?;

                let real_l_v = match LNum::get_real_value(lvalue, lv.start) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

                let real_r_v = match LNum::get_real_value(rvalue, rv.start) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };

                LNum::complete_binary_op(op, real_l_v, real_r_v)
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
