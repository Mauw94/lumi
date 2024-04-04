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
        // TODO: add all results of expressions to a Vec trace
        // and print this in main
        Expr::Sequence(xs) => {
            // this if is a hack, needs fixing
            // FIXME
            if xs.len() == 1 {
                return evaluate(env, xs.last().unwrap());
            } else {
                for expr in xs {
                    let o = evaluate(env, expr)?;
                    o.print_value();
                }
            }

            Ok(Obj::Null)
        }
        Expr::Int(v) => Ok(Obj::Num(LNum::Int(*v))),
        Expr::Float(v) => Ok(Obj::Num(LNum::Float(*v))),
        Expr::String(v) => Ok(Obj::Seq(Seq::String(Rc::new(v.to_string())))),
        Expr::Identifier(v) => match env.lookup_variable(v, expr.start) {
            Ok(v) => Ok(v.1),
            Err(e) => Err(e),
        },
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
        Expr::Declare(var_name, obj_type, expr) => {
            let value = evaluate(env, expr)?;
            if !value.is_type(obj_type) {
                return Err(LErr::runtime_error(
                    // TODO: move common error messages to global file
                    format!(
                        "Type mismatch. Tried to assign a {} value to {}",
                        value.get_type_name(),
                        obj_type.get_type_name()
                    ),
                    expr.start,
                ));
            } else {
                env.define(var_name.to_string(), obj_type.to_owned(), value);
            }
            Ok(Obj::Null)
        }
        Expr::Assign(l_expr, r_expr) => match &l_expr.expr {
            // When declaring a variable without a type it is always re-assignable.
            // e.g.
            // a: int -> 2
            // a = "test" => will fail with a type mismatch.
            // a -> 5
            // a = "abc" => will NOT fail because variable 'a' never got a specific type.
            Expr::Identifier(var_name) => {
                match env.lookup_variable(var_name, l_expr.start) {
                    Ok(o) => {
                        let rhs = evaluate(env, &r_expr)?;
                        if rhs.is_type(&o.0) {
                            env.define(var_name.to_string(), o.0, rhs);
                        } else {
                            return Err(LErr::parsing_error(
                                format!(
                                    "Type mismatch. Tried to assign a {} value to {}",
                                    rhs.get_type_name(),
                                    o.0.get_type_name()
                                ),
                                l_expr.start,
                            ));
                        }
                    }
                    Err(e) => return Err(e),
                }
                return Ok(Obj::Null);
            }
            _ => {
                return Err(LErr::runtime_error(
                    "Cannot assign to non-identifier.".to_string(),
                    l_expr.start,
                ))
            }
        },
        Expr::List(exprs) => {
            let mut objs: Vec<Obj> = Vec::new();
            for expr in exprs {
                objs.push(evaluate(env, expr)?);
            }

            Ok(Obj::Seq(Seq::List(Rc::new(objs))))
        }
        Expr::Print(expr) => {
            return Ok(evaluate(env, expr)?);
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
