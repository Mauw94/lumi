use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    core::{LErr, LNum, Obj, Seq},
    define, eval,
    lexer::Token,
    lookup_variable,
    parser::{Expr, LiteralValue, LumiExpr},
    undefine, Closure, CodeLoc, Env, Func, ObjectType, Struct,
};

pub fn evaluate(env: &Rc<RefCell<Env>>, expr: &LumiExpr) -> Result<Obj, LErr> {
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
        Expr::Block(xs) => {
            for expr in xs {
                let o = evaluate(env, expr)?;
                o.print_value();
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
            match expr {
                Some(e) => {
                    let value = evaluate(env, e)?;
                    if !value.is_type(obj_type) {
                        return Err(LErr::runtime_error(
                            format!(
                                "Type mismatch. Tried to assign a {} value to {}",
                                value.get_type_name(),
                                obj_type.get_type_name()
                            ),
                            e.start,
                            e.end,
                        ));
                    } else {
                        define(env, var_name.to_string(), obj_type.to_owned(), value)?;
                    }
                }
                None => define(
                    env,
                    var_name.to_string(),
                    obj_type.to_owned(),
                    Obj::get_default_value(&obj_type)?,
                )?,
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
        Expr::Print(expr) => {
            let prt = evaluate(env, expr)?;
            prt.print_value();
            Ok(Obj::Null)
        }
        Expr::Return(Some(expr)) => Err(LErr::Return(evaluate(env, expr)?)),
        Expr::Return(None) => Err(LErr::Return(Obj::Null)),
        Expr::Struct(s_name, parameters, body) => {
            let mut methods: HashMap<String, LumiExpr> = HashMap::new();
            for m in body.iter() {
                match &m.expr {
                    Expr::Fn(n, _p, _e) => {
                        methods.insert(n.to_string(), *m.clone());
                    }
                    // TODO: add properties as well
                    _ => todo!(), // _ => Err(LErr::runtime_error(
                                  //     "Expected a function".to_string(),
                                  //     m.start,
                                  //     m.end,
                                  // )),
                }
            }

            let s = Struct {
                params: Rc::clone(parameters),
                env: Rc::clone(env),
                methods,
            };

            // for expr in body.iter() {
            //     evaluate(&s.env, expr)?;
            // }

            let strct = Obj::Struct(s);
            define(env, s_name.to_string(), ObjectType::Struct, strct.clone())?;
            Ok(Obj::Null)
        }
        Expr::Fn(fn_name, parameters, expressions) => {
            // TODO: add types to parameters and check if argument has correct type
            let func = Obj::Func(Box::new(Func::Closure(Box::new(Closure {
                body: Rc::clone(expressions),
                params: Rc::clone(parameters),
            }))));
            define(env, fn_name.to_string(), ObjectType::Function, func.clone())?;
            Ok(func)
        }
        Expr::Get(strct, method, args) => {
            let res = evaluate(env, strct)?;
            match res {
                Obj::Struct(mut s) => {
                    // FIXME check type still maybe?
                    match s.find_method(method, strct.start, strct.end) {
                        Ok(m) => {
                            match evaluate(&s.env, &m)? {
                                Obj::Func(f) => match *f {
                                    Func::Closure(mut c) => {
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
                                                strct.start,
                                                strct.end,
                                            ));
                                        }

                                        // FIXME
                                        // call stops evaluating after RETURN but still emits some NULL values..?
                                        match c.call(arguments, env, strct.start, strct.end) {
                                            Ok(o) => {
                                                return Ok(o);
                                            }
                                            Err(e) => return Err(e),
                                        }
                                    }
                                    _ => {
                                        return Err(LErr::runtime_error(
                                            "Expect closure".to_string(),
                                            strct.start,
                                            strct.end,
                                        ))
                                    }
                                },
                                _ => {
                                    return Err(LErr::runtime_error(
                                        "blehg".to_string(),
                                        strct.start,
                                        strct.start,
                                    ))
                                }
                            }
                            // println!("eval result {:?}", res);

                            // Ok(Obj::Null)
                        }
                        Err(err) => return Err(err),
                    }
                }
                _ => {
                    return Err(LErr::runtime_error(
                        "Expected a struct here.".to_string(),
                        strct.start,
                        strct.end,
                    ));
                }
            }
        }
        Expr::Call(callee, args) => {
            let func = evaluate(env, callee)?;
            // seems like func(closures) are being put on the stack (LIFO)
            // and thus when we evaluate and reach a return value it still evaluates "older" calls afterwards
            match func {
                Obj::Func(f) => match *f {
                    Func::Closure(mut c) => {
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

                        // FIXME
                        // call stops evaluating after RETURN but still emits some NULL values..?
                        match c.call(arguments, env, callee.start, callee.end) {
                            Ok(o) => {
                                return Ok(o);
                            }
                            Err(e) => return Err(e),
                        }
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
        Expr::For(index, to_expr, from_expr, step_expr, body) => {
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

            define(
                env,
                index.to_string(),
                ObjectType::Int,
                Obj::Num(LNum::Int(to)),
            )?;
            while to <= from {
                for expr in body {
                    objects.push(evaluate(env, expr)?);
                }
                to += step;
                define(
                    env,
                    index.to_string(),
                    ObjectType::Int,
                    Obj::Num(LNum::Int(to)),
                )?;
            }

            undefine(env, &index)?; // Remove index var

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
