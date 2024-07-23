use std::{cell::RefCell, rc::Rc};

use crate::{
    define_var, interpreter, lookup, CodeLoc, Env, Expr, LErr, LNum, LookupType, LumiExpr, Obj,
    Seq, Token,
};

pub fn unary_exp(env: &Rc<RefCell<Env>>, token: &Token, expr: &LumiExpr) -> Result<Obj, LErr> {
    let rhs = interpreter::evaluate(env, expr)?;

    match token {
        Token::Bang => {
            return Ok(Obj::Bool(!is_truthy(rhs)));
        }
        Token::Minus => match rhs {
            Obj::Num(lnum) => match lnum {
                LNum::Int(v) => return Ok(Obj::Num(LNum::Int(-v))),
                LNum::Float(v) => return Ok(Obj::Num(LNum::Float(-v))),
                LNum::Byte(b) => return Ok(Obj::Num(LNum::Byte(b))), // Cannot negate unsigned
            },
            _ => {
                return Err(LErr::runtime_error(
                    "Operand must be a number".to_string(),
                    expr.start,
                    expr.end,
                ))
            }
        },
        _ => Ok(Obj::Null),
    }
}

pub fn logical_expr(
    env: &Rc<RefCell<Env>>,
    operator_token: &Token,
    l_expr: &LumiExpr,
    r_expr: &LumiExpr,
) -> Result<Obj, LErr> {
    let lhs = interpreter::evaluate(env, l_expr)?;

    if operator_token == &Token::Or {
        if is_truthy(lhs.clone()) {
            return Ok(lhs);
        }
    } else if operator_token == &Token::And {
        let rhs = interpreter::evaluate(env, &r_expr)?;
        return Ok(Obj::Bool(is_truthy(lhs.clone()) && is_truthy(rhs.clone())));
    } else {
        if !is_truthy(lhs.clone()) {
            return Ok(lhs);
        }
    }

    return interpreter::evaluate(env, &r_expr);
}

// When declaring a variable without a type it is always re-assignable.
// e.g.
// a: int -> 2
// a = "test" => will fail with a type mismatch.
// a -> 5
// a = "abc" => will NOT fail because variable 'a' never got a specific type.
pub fn assign_exp(
    env: &Rc<RefCell<Env>>,
    l_expr: &LumiExpr,
    r_expr: &LumiExpr,
) -> Result<Obj, LErr> {
    match &l_expr.expr {
        Expr::Identifier(var_name) => {
            match lookup(env, var_name, l_expr.start, l_expr.end, LookupType::Var) {
                Ok(o) => match &r_expr.expr {
                    Expr::Index(var, i) => {
                        let index_obj = interpreter::evaluate(env, i)?;
                        let index: usize =
                            get_real_index_num_from_object(index_obj, r_expr.start, r_expr.end)?;
                        return get_value_by_index_from_list(
                            env,
                            var,
                            index,
                            r_expr.start,
                            r_expr.end,
                        );
                    }
                    _ => {
                        let rhs = interpreter::evaluate(env, &r_expr)?;
                        if rhs.is_type(&o.0) {
                            define_var(env, var_name.to_string(), o.0, rhs)?;
                        } else {
                            return Err(LErr::runtime_error(
                                format!(
                                    "Type mismatch. Tried to assign a {} value to {}",
                                    rhs.get_type_name(),
                                    o.0.get_type_name()
                                ),
                                r_expr.start,
                                r_expr.end,
                            ));
                        }
                    }
                },
                Err(e) => return Err(e),
            }
            return Ok(Obj::Null);
        }
        _ => {
            return Err(LErr::runtime_error(
                "Cannot assign to non-identifier.".to_string(),
                l_expr.start,
                l_expr.end,
            ))
        }
    }
}

pub fn list_expr(env: &Rc<RefCell<Env>>, exprs: &Vec<Box<LumiExpr>>) -> Result<Obj, LErr> {
    let objs: Vec<Obj> = exprs
        .into_iter()
        .map(|e| interpreter::evaluate(env, &e))
        .collect::<Result<Vec<Obj>, LErr>>()?;

    if objs.is_empty() {
        return Ok(Obj::Seq(Seq::List(Rc::new(objs))));
    } else {
        let first_type = objs.first().unwrap(); // We expect the first value to be available
        for (i, o) in objs.iter().enumerate() {
            if !o.is_same_type(first_type) {
                return Err(LErr::runtime_error(
                    format!(
                        "Value in list is not of the same type ({}), expected type {}",
                        o.get_type_name(),
                        first_type.get_type_name()
                    ),
                    exprs.get(i).unwrap().start,
                    exprs.get(i).unwrap().end,
                ));
            }
        }
    }
    Ok(Obj::Seq(Seq::List(Rc::new(objs))))
}

fn get_real_index_num_from_object(
    index_obj: Obj,
    start: CodeLoc,
    end: CodeLoc,
) -> Result<usize, LErr> {
    match index_obj {
        Obj::Num(lnum) => return Ok(lnum.get_num_val_usize()),
        _ => {
            return Err(LErr::runtime_error(
                "Index is not a numeric value.".to_string(),
                start,
                end,
            ))
        }
    };
}

fn get_value_by_index_from_list(
    env: &Rc<RefCell<Env>>,
    var: &String,
    index: usize,
    start: CodeLoc,
    end: CodeLoc,
) -> Result<Obj, LErr> {
    match lookup(env, var, start, end, LookupType::Var) {
        Ok(o) => match o.1 {
            Obj::Seq(sq) => match sq {
                Seq::List(list) => {
                    if list.len() > index {
                        let cloned_rc_list = Rc::clone(&list);
                        if let Some(obj) = cloned_rc_list.get(index) {
                            return Ok(obj.clone());
                        } else {
                            return Err(LErr::runtime_error(
                                format!("Did not find an object with index {}", index),
                                start,
                                end,
                            ));
                        }
                    } else {
                        return Err(LErr::runtime_error(
                            "Index out of bounds".to_string(),
                            start,
                            end,
                        ));
                    }
                }
                _ => {
                    return Err(LErr::runtime_error(
                        "Can not find index in none list type.".to_string(),
                        start,
                        end,
                    ))
                }
            },
            _ => {
                return Err(LErr::runtime_error(
                    "Can not find index in none list type.".to_string(),
                    start,
                    end,
                ))
            }
        },
        Err(e) => return Err(e),
    }
}

fn is_truthy(obj: Obj) -> bool {
    match obj {
        Obj::Bool(b) => b,
        Obj::Null => false,
        _ => true,
    }
}
