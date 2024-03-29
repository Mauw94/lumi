use std::rc::Rc;

use crate::{
    core::{LErr, LNum, LRes, Obj, Seq},
    eval,
    lexer::Token,
    parser::{Expr, LiteralValue, LumiExpr},
};

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&self, expr: &LumiExpr) -> LRes<Obj> {
        match &expr.expr {
            Expr::Sequence(xs) => {
                // Sequence is nested
                self.eval(xs.last().unwrap())
            }
            Expr::Int(v) => Ok(Obj::Num(LNum::Int(*v))),
            Expr::Float(v) => Ok(Obj::Num(LNum::Float(*v))),
            Expr::String(v) => Ok(Obj::Seq(Seq::String(Rc::new(v.to_string())))),
            Expr::Identifier(v) => Ok(Obj::Seq(Seq::String(Rc::new(v.to_string())))),
            Expr::Literal(literal) => match literal {
                LiteralValue::True => Ok(Obj::Bool(true)),
                LiteralValue::False => Ok(Obj::Bool(false)),
                LiteralValue::Nil => Ok(Obj::Null),
            },
            Expr::Unary(t, expr) => {
                let rhs = self.eval(expr)?;

                match t {
                    Token::Bang => {
                        return Ok(Obj::Bool(!self.is_truthy(rhs)));
                    }
                    Token::Minus => match rhs {
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
                    if self.is_truthy(lhs.clone()) {
                        return Ok(lhs);
                    }
                } else if op == &Token::And {
                    let rhs = self.eval(&r_expr)?;
                    return Ok(Obj::Bool(
                        self.is_truthy(lhs.clone()) && self.is_truthy(rhs.clone()),
                    ));
                } else {
                    if !self.is_truthy(lhs.clone()) {
                        return Ok(lhs);
                    }
                }

                return self.eval(&r_expr);
            }
            Expr::Binary(lv, op, rv) => {
                let lhs = self.eval(lv)?;
                let rhs = self.eval(rv)?;

                eval::exec_binary_op(op, lhs, rhs, lv.start, rv.start)
            }
            Expr::Var(_) => todo!(),
            Expr::Assign(_, _) => todo!(),
            Expr::Print(expr) => Ok(self.eval(expr)?),
        }
    }

    fn is_truthy(&self, obj: Obj) -> bool {
        match obj {
            Obj::Bool(b) => b,
            Obj::Null => false,
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Interpreter, LumiExpr};
    use crate::{core::Obj, lexer::Lexer, parser::Parser};

    fn n(n: f64) -> Obj {
        Obj::f64(n)
    }

    #[test]
    fn make_binary_expr() {
        let interpreter = Interpreter::new();
        let expr = setup(&"2 + 2\n".to_string());

        let result = interpreter.eval(&expr).unwrap();

        assert_eq!(result, n(4.0));
    }

    fn setup(code: &str) -> LumiExpr {
        let mut lexer: Lexer = Lexer::new(code);
        let tokens = lexer.lex().unwrap();

        let mut parser: Parser = Parser::new(tokens);
        return parser.parse().unwrap();
    }
}
