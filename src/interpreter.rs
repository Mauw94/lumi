use crate::{core::LErr, lexer::Token, parser::Expr};

pub enum Obj {
    Null,
    Num(LNum),
}

pub enum LNum {
    Int(i64),
    Float(f64)
}

// TODO implement stuff

pub type LRes<T> = Result<T, LErr>;

struct Interpreter {
        expr: Expr
}

impl Interpreter {
    pub fn eval(&self, expr: &Expr) -> LRes<Obj> {
        match &self.expr {
            Expr::Sequence(xs) => {
                // for (i, x) in xs[..xs.len() - 1].iter().enumerate() {
                //     self.eval(x);
                // }
                Ok(Obj::Null)
            },
            Expr::Int(v) => Ok(Obj::Num(LNum::Int(*v))),
            Expr::Float(_) => todo!(),
            Expr::Identifier(_) => todo!(),
            Expr::Unary(_, _) => todo!(),
            Expr::Logical(_, _, _) => todo!(),
            Expr::Binary(lv, op, rv) => {
                // let lvalue = self.eval(lv)?;
                // let rvalue = self.eval(rv)?;
                // match op {
                //     Token::Plus => {

                //     }
                //     Token::Minus {

                //     }
                //     _ => Ok(Obj::Null)
                // }
                Ok(Obj::Null)
            },
            Expr::Var(_) => todo!(),
            Expr::Assign(_, _) => todo!(),
        }
    }
}