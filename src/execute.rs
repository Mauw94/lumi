use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    define_function, define_struct, define_var, interpreter, lookup, undefine_var, vectors, Builtin, Closure, CodeLoc, Env, Expr, Func, GetType, LErr, LInt, LNum, LRes, LookupType, LumiExpr, Obj, ObjectType, Seq, Struct, Token, EVAL
};

pub fn sequence_expr(env: &Rc<RefCell<Env>>, exprs: &Vec<Box<LumiExpr>>) -> LRes<Obj> {
    let mut results: Vec<String> = Vec::new();
    if exprs.len() == 1 {
        let res = interpreter::evaluate(env, exprs.last().unwrap())?;
        results.push(res.format_value());

        let mut eval = EVAL.lock().unwrap();
        eval.res = results;
        return Ok(res);
    } else {
        let mut evals: Vec<Obj> = Vec::new();
        for expr in exprs {
            let res = interpreter::evaluate(env, expr)?;
            results.push(res.format_value());
            evals.push(res);
        }

        let mut eval = EVAL.lock().unwrap();
        eval.res = results;

        return Ok(evals.last().unwrap().clone());
    }
}

pub fn block_expr(env: &Rc<RefCell<Env>>, exprs: &Vec<Box<LumiExpr>>) -> Result<Obj, LErr> {
    for expr in exprs {
        let o = interpreter::evaluate(env, expr)?;
        o.print_value();
    }

    Ok(Obj::Null)
}

pub fn unary_exp(env: &Rc<RefCell<Env>>, token: &Token, expr: &LumiExpr) -> Result<Obj, LErr> {
    let rhs = interpreter::evaluate(env, expr)?;

    match token {
        Token::Bang => {
            return Ok(Obj::Bool(!is_truthy(rhs)));
        }
        Token::Minus => match rhs {
            Obj::Num(lnum) => match lnum {
                LNum::Int(v) => match v {
                    crate::LInt::Small(i) => Ok(Obj::Num(LNum::Int(crate::LInt::Small(-i)))),
                    crate::LInt::Big(i) => Ok(Obj::Num(LNum::Int(crate::LInt::Big(-i)))),
                    crate::LInt::Long(i) => Ok(Obj::Num(LNum::Int(crate::LInt::Long(-i)))),
                },
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

pub fn index_expr(env: &Rc<RefCell<Env>>, var: &String, expr: &Box<LumiExpr>) -> Result<Obj, LErr> {
    let index_obj = interpreter::evaluate(env, expr)?;
    let index: usize = get_real_index_num_from_object(index_obj, expr.start, expr.end)?;
    get_value_by_index_from_list(env, var, index, expr.start, expr.end)
}

pub fn assign_exp(
    env: &Rc<RefCell<Env>>,
    l_expr: &LumiExpr,
    r_expr: &LumiExpr,
) -> Result<Obj, LErr> {
    match &l_expr.expr {
        Expr::Identifier(var_name) => {
            match lookup(env, var_name, l_expr.start, l_expr.end, LookupType::Var) {
                Ok(lh_obj) => match &r_expr.expr {
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
                        if rhs.is_type(&lh_obj.0) {
                            define_var(env, var_name.to_string(), lh_obj.0, rhs)?;
                        } else {
                            return Err(LErr::runtime_error(
                                format!(
                                    "Type mismatch. Tried to assign a {} value to {}",
                                    rhs.get_type_name(),
                                    lh_obj.0.get_type_name()
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
        return Ok(Obj::Seq(Seq::List(Rc::new(RefCell::new(Vec::new())))));
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
    Ok(Obj::Seq(Seq::List(Rc::new(RefCell::new(objs)))))
}

pub fn struct_expr(
    env: &Rc<RefCell<Env>>,
    s_name: &String,
    parameters: &Rc<Vec<Box<String>>>,
    body: &Rc<Vec<Box<LumiExpr>>>,
) -> Result<Obj, LErr> {
    let mut new_struct = Struct {
        params: Rc::clone(parameters),
        env: Rc::clone(env),
        functions: HashMap::new(),
        properties: Vec::new(),
    };

    for m in body.iter() {
        match &m.expr {
            Expr::Fn(n, _p, _e) => {
                new_struct.functions.insert(n.to_string(), *m.clone());
            }
            Expr::Declare(var_name, obj_type, expr) => {
                new_struct.properties.push(var_name.to_string());
                declare_expr(&new_struct.env, expr, var_name, obj_type)?;
            }
            // NOTE: Expr::Call shouldn't be possible here. Struct only contains properties or functions
            _ => {
                return Err(LErr::runtime_error(
                    "Unexpected expression.".to_string(),
                    m.start,
                    m.end,
                ))
            }
        }
    }

    define_struct(env, s_name.to_string(), Obj::Struct(new_struct.clone()))?;
    Ok(Obj::Null)
}

pub fn call_expr(
    env: &Rc<RefCell<Env>>,
    callee: &Box<LumiExpr>,
    args: &Option<Vec<Box<LumiExpr>>>,
) -> Result<Obj, LErr> {
    let func = interpreter::evaluate(env, callee)?;
    // seems like func(closures) are being put on the stack (LIFO)
    // and thus when we evaluate and reach a return value it still evaluates "older" calls afterwards
    match func.clone() {
        Obj::Func(f) => match *f {
            Func::Closure(closure) => {
                return execute_closure_func_call(callee, closure, args, env);
            }
            Func::Builtin(b) => {
                if args.is_none() {
                    return evaluate_lresult(b, env, Vec::new(), callee.start, callee.end);
                } else {
                    let args_unwrapped = args.clone().unwrap();
 
                    let arguments = args_unwrapped
                        .into_iter()
                        .map(|a| interpreter::evaluate(env, &a))
                        .collect::<Result<Vec<Obj>, LErr>>()?;

                    return evaluate_lresult(b, env, arguments, callee.start, callee.end);
                }
            }
            _ => {
                return Err(LErr::runtime_error(
                    format!(
                        "Function type {:?} is not callable like this. Consider calling it on the appropriate property or variable.",
                        func.get_type_name()
                    ),
                    callee.start,
                    callee.end,
                ))
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

fn evaluate_lresult(built_in: Rc<dyn Builtin>, env: &Rc<RefCell<Env>>, arguments: Vec<Obj> , start: CodeLoc, end: CodeLoc) -> Result<Obj, LErr> {
    match built_in.run(env, arguments, start, end) {
        Ok(obj) => match obj {
            
            Obj::LResult(lresult) => match lresult {
                crate::LResult::Res(obj) => { 
                    return Ok(*obj);
                },
                crate::LResult::Error(e) => { 
                    eprintln!("{:?}", e);
                    return Ok(Obj::Null);
                },
            },
            _ => return Ok(obj)
        },
        Err(e) => return Err(e),
    }
}

pub fn function_expr(
    env: &Rc<RefCell<Env>>,
    fn_name: &String,
    parameters: &Rc<Vec<Box<String>>>,
    expressions: &Rc<Vec<Box<LumiExpr>>>,
) -> Result<Obj, LErr> {
    // TODO: add types to parameters and check if argument has correct type
    let func = Obj::Func(Box::new(Func::Closure(Box::new(Closure {
        body: Rc::clone(expressions),
        params: Rc::clone(parameters),
    }))));
    define_function(env, fn_name.to_string(), ObjectType::Function, func.clone())?;
    Ok(func)
}

pub fn for_expr(
    env: &Rc<RefCell<Env>>,
    index: &String,
    to_expr: &Box<LumiExpr>,
    from_expr: &Box<LumiExpr>,
    step_expr: &Box<LumiExpr>,
    body: &Vec<Box<LumiExpr>>,
) -> Result<Obj, LErr> {
    let mut objects: Vec<Obj> = Vec::new();

    let mut to = match interpreter::evaluate(env, to_expr) {
        Ok(o) => match o.get_int_val() {
            Ok(v) => v,
            Err(e) => return Err(e),
        },
        Err(e) => return Err(e),
    };
    let from = match interpreter::evaluate(env, from_expr) {
        Ok(o) => match o.get_int_val() {
            Ok(v) => v,
            Err(e) => return Err(e),
        },
        Err(e) => return Err(e),
    };
    let step = match interpreter::evaluate(env, step_expr) {
        Ok(o) => match o.get_int_val() {
            Ok(v) => v,
            Err(e) => return Err(e),
        },
        Err(e) => return Err(e),
    };

    define_var(
        env,
        index.to_string(),
        ObjectType::Int,
        Obj::Num(LNum::Int(LInt::new(to))),
    )?;
    while to <= from {
        for expr in body {
            objects.push(interpreter::evaluate(env, expr)?);
        }
        to += step;
        define_var(
            env,
            index.to_string(),
            ObjectType::Int,
            Obj::Num(LNum::Int(LInt::new(to))),
        )?;
    }

    undefine_var(env, &index)?; // Remove index var

    Ok(Obj::Seq(Seq::List(Rc::new(RefCell::new(objects)))))
}

pub fn foreach_expr(
    env: &Rc<RefCell<Env>>,
    iterable: &String,
    _token: &Token,
    identifier: &String,
    index_identifier: &Option<String>,
    body: &Vec<Box<LumiExpr>>,
    start: &CodeLoc,
    end: &CodeLoc
) -> Result<Obj, LErr> {
    let mut objects: Vec<Obj> = Vec::new();
    let iter = lookup(&env, iterable, *start, *end, LookupType::Var)?;

    if !iter.1.is_string() && !iter.1.is_list() {
        return Err(LErr::runtime_error("Expected a string or list in foreach statement.".to_string(), *start, *end));
    }

    // Helper function to handle iteration and evaluation logic
    let mut execute_body = |index: usize, current_value: Obj| -> Result<(), LErr> {
        if let Some(index_str) = index_identifier {
            define_var(env, index_str.to_string(), ObjectType::String, Obj::new_number_obj(index))?;
        }
        define_var(env, identifier.to_string(), ObjectType::String, current_value)?;
        for expr in body {
            objects.push(interpreter::evaluate(env, expr)?);
        }
        Ok(())
    };

    if iter.1.is_string() {
        let iter_str = iter.1.get_str_val()?;
        for (index, current_value) in iter_str.chars().enumerate() {
            execute_body(index, Obj::new_str_obj(current_value))?;
        }
    } else if iter.1.is_list() {
        let iter_list = iter.1.get_list_val()?;
        let list = iter_list.borrow();
        for (index, current_value) in list.iter().enumerate() {
            execute_body(index, current_value.clone())?;
        }
    }

    // TODO: add dictionary

    if let Some(index_str) = index_identifier {
        undefine_var(env, index_str)?;
    }

    Ok(Obj::Seq(Seq::List(Rc::new(RefCell::new(objects)))))
}

pub fn namespace_expr(
    env: &Rc<RefCell<Env>>,
    name: &String,
    start: &CodeLoc,
    end: &CodeLoc,
    is_include: &bool,
) -> Result<Obj, LErr> {
    let func = lookup(&env, name, *start, *end, LookupType::Namespace)?;
    match func.1 {
        Obj::Func(f) => match *f {
            Func::Namespace(n) => {
                if *is_include {
                    n.load_functions(env)?;
                } else {
                    n.unload_functions(env)?;
                }
                Ok(Obj::Null)
            }
            _ => {
                return Err(LErr::runtime_error(
                    format!("Expected a namespace here, found {:?}", func.0),
                    *start,
                    *end,
                ))
            }
        },
        _ => {
            return Err(LErr::runtime_error(
                format!("Expected a namespace here, found {:?}", func.0),
                *start,
                *end,
            ))
        }
    }
}

pub fn declare_expr(
    env: &Rc<RefCell<Env>>,
    expr: &Option<Box<LumiExpr>>,
    var_name: &String,
    obj_type: &ObjectType,
) -> Result<(), LErr> {
    match expr {
        Some(e) => {
            let value = interpreter::evaluate(env, e)?;
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
                Ok(define_var(
                    env,
                    var_name.to_string(),
                    obj_type.to_owned(),
                    value,
                )?)
            }
        }
        None => Ok(define_var(
            env,
            var_name.to_string(),
            obj_type.to_owned(),
            Obj::get_default_value(&obj_type)?,
        )?),
    }
}

pub fn every_expr(env: &Rc<RefCell<Env>>, callee: &Box<LumiExpr>, token: &Token, term: &Box<LumiExpr>) -> Result<Obj, LErr> {
    let evaluated_list = interpreter::evaluate(env, &callee)?;
    if !evaluated_list.is_list() {
        return Err(LErr::runtime_error("The 'every' keyword can only be used after a list expression.".to_string(), callee.start, callee.end));
    }

    let list_val = evaluated_list.get_list_val()?;
    let list = list_val.borrow();
    let rust_list = vectors::parse_lumi_list_to_rust_vec::<f32>(&list)?;

    let evaluated_term = interpreter::evaluate(env, &term)?;
    if !evaluated_term.is_number() {
        return Err(LErr::runtime_error("Expected a number after the 'every' keyword.".to_string(), term.start, term.end));
    }
    let term_in_number = evaluated_term.get_num_value(term.start, term.end)?;

    match token {
        Token::Star => {
                let modified_vec: Vec<f32> = rust_list.iter().map(|&x| x * term_in_number).collect();
                return Ok(Obj::new_list_obj(modified_vec));
        },
        Token::Minus => {
            let modified_vec: Vec<f32> = rust_list.iter().map(|&x| x - term_in_number).collect();
                return Ok(Obj::new_list_obj(modified_vec));
        },
        Token::Plus =>  {
            let modified_vec: Vec<f32> = rust_list.iter().map(|&x| x + term_in_number).collect();
                return Ok(Obj::new_list_obj(modified_vec));
        },
        Token::Slash => {
            let modified_vec: Vec<f32> = rust_list.iter().map(|&x| x / term_in_number).collect();
                return Ok(Obj::new_list_obj(modified_vec));
        },
        _ => return Err(LErr::runtime_error(format!("Unexpected token. Found {:?}, but expected either '-, +, * or /'.", token), callee.end, term.start))
    }
}

pub fn get_expr(
    env: &Rc<RefCell<Env>>,
    callee: &Box<LumiExpr>,
    func_name: &String,
    args: &Option<Vec<Box<LumiExpr>>>,
    get_type: &GetType,
) -> Result<Obj, LErr> {
    let eval_res = interpreter::evaluate(env, callee)?;
    match eval_res.clone() {
        Obj::Struct(mut s) => match get_type {
            GetType::Property => {
                let var = lookup(&s.env, func_name, callee.start, callee.end, LookupType::Var)?;
                return Ok(var.1);
            }
            GetType::Function => match s.find_method(func_name, callee.start, callee.end) {
                Ok(m) => match interpreter::evaluate(&s.env, &m)? {
                    Obj::Func(f) => match *f {
                        Func::Closure(closure) => {
                            return execute_closure_func_call(callee, closure, args, &s.env)
                        }
                        _ => {
                            return Err(LErr::runtime_error(
                                "Expect closure".to_string(),
                                callee.start,
                                callee.end,
                            ))
                        }
                    },
                    _ => {
                        return Err(LErr::runtime_error(
                            "Expected a function object".to_string(),
                            callee.start,
                            callee.start,
                        ))
                    }
                },
                Err(err) => return Err(err),
            },
        },
        Obj::Seq(Seq::List(_lst)) => execute_function(
            &env,
            &eval_res,
            get_callee_var_name(callee)?,
            func_name,
            callee.start,
            callee.end,
            &args.clone().unwrap(),
        ),
        Obj::Seq(Seq::Dict(_dict)) => execute_function(
            &env,
            &eval_res,
            get_callee_var_name(callee)?,
            func_name,
            callee.start,
            callee.end,
            &args.clone().unwrap(),
        ),
        Obj::Seq(Seq::String(_s)) =>
            execute_function(
                &env,
                &eval_res,
                get_callee_var_name(callee)?,
                func_name,
                callee.start,
                callee.end,
                &args.clone().unwrap(),
            ),
        _ => {
            return Err(LErr::runtime_error(
                "Expected a struct here.".to_string(), // FIXME can also be a list or another var
                callee.start,
                callee.end,
            ));
        }
    }
}

fn get_callee_var_name(callee: &Box<LumiExpr>) -> LRes<&str> {
    match &callee.expr {
        Expr::Identifier(identifier) => Ok(&identifier),
        _ => Err(LErr::internal_error(format!(
            "Expected a var name here. Found {}",
            callee.expr
        ))),
    }
}

fn execute_function(
    env: &Rc<RefCell<Env>>,
    eval_res: &Obj,
    var_name: &str,
    func_name: &String,
    start: CodeLoc,
    end: CodeLoc,
    args: &Vec<Box<LumiExpr>>,
) -> Result<Obj, LErr> {
    let function = lookup(&env, func_name, start, end, LookupType::Function)?;
    match function.1 {
        Obj::Func(f) => match *f {
            Func::Builtin(b) => {
                if args.is_empty() {
                    return b.run(env, vec![eval_res.clone()], start, end);
                } else {
                    let mut arguments = args
                        .into_iter()
                        .map(|a| interpreter::evaluate(env, &a))
                        .collect::<Result<Vec<Obj>, LErr>>()?;
                    // Add the object we call this function on as the first argument in the vec.
                    arguments.insert(0, eval_res.clone());
                    return b.run(env, arguments, start, end);
                }
            }
            Func::Extension(e) => {
                let arguments = args
                    .into_iter()
                    .map(|a| interpreter::evaluate(env, &a))
                    .collect::<Result<Vec<Obj>, LErr>>()?;
                return e.run(env, var_name, eval_res.clone(), arguments, start, end);
            }
            _ => {
                return Err(LErr::runtime_error(
                    "Expected a built_in function call here.".to_string(),
                    start,
                    end,
                ))
            }
        },
        _ => {
            return Err(LErr::runtime_error(
                format!(
                    "Object of type {:?} cannot be used as a function.",
                    function.1.get_type_name()
                ),
                start,
                end,
            ))
        }
    };
}

fn execute_closure_func_call(
    callee: &Box<LumiExpr>,
    mut closure: Box<Closure>,
    args: &Option<Vec<Box<LumiExpr>>>,
    env: &Rc<RefCell<Env>>,
) -> Result<Obj, LErr> {
    let mut arguments = Vec::new();
    match args {
        Some(args) => {
            arguments = args
                .into_iter()
                .map(|a| interpreter::evaluate(env, &a))
                .collect::<Result<Vec<Obj>, LErr>>()?;
            if arguments.len() != closure.params.len() {
                return Err(LErr::runtime_error(
                    format!(
                        "Expected {} arguments, but got {}.",
                        closure.params.len(),
                        arguments.len()
                    ),
                    callee.start,
                    callee.end,
                ));
            }
        }
        None => {}
    }
    // FIXME
    // call stops evaluating after RETURN but still emits some NULL values..?
    match closure.call(arguments, env, callee.start, callee.end) {
        Ok(o) => {
            return Ok(o);
        }
        Err(e) => return Err(e),
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
    match lookup(env, var, start, end, LookupType::Var) {
        Ok(o) => match o.1 {
            Obj::Seq(sq) => match sq {
                Seq::List(list_val) => {
                    let list = list_val.borrow();
                    if list.len() > index {
                        if let Some(x) = list.get(index) {
                            return Ok(x.clone());
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
