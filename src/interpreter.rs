use std::{cell::RefCell, rc::Rc};

use crate::{
    core::{LErr, LNum, LRes, Obj, Seq},
    define, eval,
    lexer::Token,
    lookup_variable,
    parser::{Expr, LiteralValue, LumiExpr},
    Closure, CodeLoc, Env, Func, ObjectType, Struct,
};

pub fn evaluate(env: &Rc<RefCell<Env>>, expr: &LumiExpr) -> LRes<Obj> {
    match &expr.expr {
        // TODO: add all results of expressions to a Vec trace
        // and print this in main
        Expr::Sequence(xs) => {
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
        Expr::Identifier(v) => match lookup_variable(env, v, expr.start, expr.end) {
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
                            expr.end,
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
        Expr::If(condition, body, else_branch) => {
            if is_truthy(evaluate(env, condition)?) {
                return Ok(evaluate(env, &body)?);
            }
            return match else_branch {
                Some(end) => Ok(evaluate(env, end)?),
                None => Ok(Obj::Null),
            };
        }
        Expr::Binary(lv, op, rv) => {
            let lhs = evaluate(env, lv)?;
            let rhs = evaluate(env, rv)?;

            eval::exec_binary_op(op, lhs, rhs, lv.start, lv.end, rv.start, rv.end)
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
                    expr.end,
                ));
            } else {
                define(env, var_name.to_string(), obj_type.to_owned(), value)?;
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
                match lookup_variable(env, var_name, l_expr.start, l_expr.end) {
                    Ok(o) => match &r_expr.expr {
                        Expr::Index(var, i) => {
                            let index_obj = evaluate(env, i)?;
                            let index: usize = get_real_index_num_from_object(
                                index_obj,
                                r_expr.start,
                                r_expr.end,
                            )?;
                            return get_value_by_index_from_list(
                                env, var, index, expr.start, r_expr.end,
                            );
                        }
                        _ => {
                            let rhs = evaluate(env, &r_expr)?;
                            if rhs.is_type(&o.0) {
                                define(env, var_name.to_string(), o.0, rhs)?;
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
        },
        Expr::List(exprs) => {
            let objs: Vec<Obj> = exprs
                .into_iter()
                .map(|e| evaluate(env, &e))
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
        Expr::Index(var, expr) => {
            let index_obj = evaluate(env, expr)?;
            let index: usize = get_real_index_num_from_object(index_obj, expr.start, expr.end)?;
            get_value_by_index_from_list(env, var, index, expr.start, expr.end)
        }
        Expr::Print(expr) => Ok(evaluate(env, expr)?),
        Expr::Return(expr) => Ok(evaluate(env, expr)?),
        Expr::Struct(s_name, parameters, body) => {
            // TODO add fields on struct object containing vars
            // methods are defined in the body
            // FIXME
            println!("{:?}", s_name);
            println!("{:?}", parameters);
            println!("{:?}", body);
            let strct = Obj::Struct(Struct {
                params: Rc::clone(parameters),
                body: Rc::clone(body),
                env: Rc::new(RefCell::new(Env::new(None))),
            });
            define(env, s_name.to_string(), ObjectType::Struct, strct.clone())?;
            Ok(strct)
        }
        Expr::Fn(fn_name, parameters, expressions) => {
            // TODO: add types to parameters and check if argument has correct type
            let func = Obj::Func(Func::Closure(Closure {
                body: Rc::clone(expressions),
                params: Rc::clone(parameters),
            }));
            define(env, fn_name.to_string(), ObjectType::Function, func.clone())?;
            return Ok(func);
        }
        Expr::Call(callee, args) => {
            let func = evaluate(env, callee)?;
            match func {
                Obj::Func(f) => match f {
                    Func::Closure(c) => {
                        let arguments = args
                            .into_iter()
                            .map(|a| evaluate(env, &a))
                            .collect::<Result<Vec<Obj>, LErr>>()?;
                        if arguments.len() != c.params.len() {
                            return Err(LErr::runtime_error(
                                format!(
                                    "Expected {} arguments, but got {}.",
                                    c.params.len(),
                                    arguments.len()
                                ),
                                callee.start,
                                callee.end,
                            ));
                        }
                        return Ok(c.call(arguments, env, callee.start, callee.end)?);
                    }
                    Func::Builtin(b) => {
                        if args.len() == 0 {
                            return b.run(env, Vec::new(), callee.start, callee.end);
                        } else {
                            let arguments = args
                                .into_iter()
                                .map(|a| evaluate(env, &a))
                                .collect::<Result<Vec<Obj>, LErr>>()?;
                            return b.run(env, arguments, callee.start, callee.end);
                        }
                    }
                },
                _ => {
                    return Err(LErr::runtime_error(
                        "Callee is not a function.".to_string(),
                        callee.start,
                        callee.end,
                    ))
                }
            };
        }
        Expr::For(to_expr, from_expr, step_expr, body) => {
            let mut objects: Vec<Obj> = Vec::new();

            let mut to = match evaluate(env, to_expr) {
                Ok(o) => match o.get_int_val() {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                },
                Err(e) => return Err(e),
            };
            let from = match evaluate(env, from_expr) {
                Ok(o) => match o.get_int_val() {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                },
                Err(e) => return Err(e),
            };
            let step = match evaluate(env, step_expr) {
                Ok(o) => match o.get_int_val() {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                },
                Err(e) => return Err(e),
            };

            while to <= from {
                objects.push(evaluate(env, body)?);
                to += step;
            }

            Ok(Obj::Seq(Seq::List(Rc::new(objects))))
        }
    }
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
    match lookup_variable(env, var, start, end) {
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
