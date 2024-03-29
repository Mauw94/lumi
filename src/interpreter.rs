use std::rc::Rc;

use crate::{
    core::{LErr, LNum, LRes, Obj, Seq},
    eval,
    lexer::Token,
    parser::{Expr, LiteralValue, LumiExpr},
    Env,
};

pub fn evaluate(env: &mut Env, expr: &LumiExpr) -> LRes<Obj> {
    match &expr.expr {
        Expr::Sequence(xs) => {
            // Sequence is nested
            evaluate(env, xs.last().unwrap())
        }
        Expr::Int(v) => Ok(Obj::Num(LNum::Int(*v))),
        Expr::Float(v) => Ok(Obj::Num(LNum::Float(*v))),
        Expr::String(v) => Ok(Obj::Seq(Seq::String(Rc::new(v.to_string())))),
        Expr::Identifier(v) => {
            env.lookup_variable(v, expr.start)
            // Ok(Obj::Seq(Seq::String(Rc::new(v.to_string()))))
        }
        Expr::Literal(literal) => match literal {
            LiteralValue::True => Ok(Obj::Bool(true)),
            LiteralValue::False => Ok(Obj::Bool(false)),
            LiteralValue::Nil => Ok(Obj::Null),
        },
        Expr::Unary(t, expr) => {
            let rhs = evaluate(env, expr)?;

            match t {
                Token::Bang => {
                    return Ok(Obj::Bool(!is_truthy(rhs)));
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
            let lhs = evaluate(env, l_expr)?;

            if op == &Token::Or {
                if is_truthy(lhs.clone()) {
                    return Ok(lhs);
                }
            } else if op == &Token::And {
                let rhs = evaluate(env, &r_expr)?;
                return Ok(Obj::Bool(is_truthy(lhs.clone()) && is_truthy(rhs.clone())));
            } else {
                if !is_truthy(lhs.clone()) {
                    return Ok(lhs);
                }
            }

            return evaluate(env, &r_expr);
        }
        Expr::Binary(lv, op, rv) => {
            let lhs = evaluate(env, lv)?;
            let rhs = evaluate(env, rv)?;

            eval::exec_binary_op(op, lhs, rhs, lv.start, rv.start)
        }
        Expr::Assign(l_expr, r_expr) => match &l_expr.expr {
            Expr::Identifier(var_name) => {
                let rhs = evaluate(env, r_expr)?;
                env.define(var_name.to_string(), rhs);
                return Ok(Obj::Null);
            }
            _ => {
                return Err(LErr::runtime_error(
                    "Cannot assign to non-identifier.".to_string(),
                    l_expr.start,
                ))
            }
        },
        Expr::Print(expr) => Ok(evaluate(env, expr)?),
        Expr::Declare(var_name, expr) => {
            let value = evaluate(env, expr)?;
            env.define(var_name.to_string(), value);
            Ok(Obj::Null)
        }
    }
}

fn is_truthy(obj: Obj) -> bool {
    match obj {
        Obj::Bool(b) => b,
        Obj::Null => false,
        _ => true,
    }
}
