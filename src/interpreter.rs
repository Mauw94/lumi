use std::{cell::RefCell, rc::Rc};

use crate::{
    core::{LErr, Obj, Seq},
    eval, execute, lookup,
    parser::{Expr, LiteralValue, LumiExpr},
    Env, LNum, LookupType,
};

pub fn evaluate(env: &Rc<RefCell<Env>>, expr: &LumiExpr) -> Result<Obj, LErr> {
    match &expr.expr {
        Expr::Sequence(exprs) => execute::sequence_expr(env, exprs),
        Expr::Block(exprs) => execute::block_expr(env, exprs),
        Expr::Int(v) => Ok(Obj::Num(LNum::Int(v.clone()))),
        Expr::Float(v) => Ok(Obj::Num(LNum::Float(*v))),
        Expr::String(v) => Ok(Obj::Seq(Seq::String(Rc::new(v.to_string())))),
        Expr::Identifier(v) => match lookup(env, v, expr.start, expr.end, LookupType::Unknown) {
            Ok(v) => Ok(v.1),
            Err(e) => Err(e),
        },
        Expr::Literal(literal) => match literal {
            LiteralValue::True => Ok(Obj::Bool(true)),
            LiteralValue::False => Ok(Obj::Bool(false)),
            LiteralValue::Nil => Ok(Obj::Null),
        },
        Expr::Unary(token, expr) => execute::unary_exp(env, token, expr),
        Expr::Logical(l_expr, operator_token, r_expr) => {
            execute::logical_expr(env, operator_token, &l_expr, &r_expr)
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
            execute::declare_expr(env, expr, var_name, obj_type)?;

            Ok(Obj::Null)
        }
        Expr::Assign(l_expr, r_expr) => execute::assign_exp(env, l_expr, r_expr),
        Expr::List(exprs) => execute::list_expr(env, exprs),
        Expr::Index(var, expr) => execute::index_expr(env, var, expr),
        Expr::Print(expr) => {
            let prt = evaluate(env, expr)?;
            prt.print_value();

            Ok(prt)
        }
        Expr::Return(Some(expr)) => Err(LErr::Return(evaluate(env, expr)?)),
        Expr::Return(None) => Err(LErr::Return(Obj::Null)),
        Expr::Struct(s_name, parameters, body) => {
            execute::struct_expr(env, s_name, parameters, body)
        }
        Expr::Fn(fn_name, parameters, expressions) => {
            execute::function_expr(env, fn_name, parameters, expressions)
        }
        Expr::Get(strct, value, args, get_type) => {
            execute::get_expr(env, strct, value, args, &get_type)
        }
        Expr::Call(callee, args) => execute::call_expr(env, callee, args),
        Expr::For(index, to_expr, from_expr, step_expr, body) => {
            execute::for_expr(env, index, to_expr, from_expr, step_expr, body)
        }
        Expr::Namespace(name, start, end, is_include) => {
            execute::namespace_expr(env, name, start, end, is_include)
        }
        Expr::Every(list, operator, term) => execute::every_expr(env, list, operator, term),
    }
}

fn is_truthy(obj: Obj) -> bool {
    match obj {
        Obj::Bool(b) => b,
        Obj::Null => false,
        _ => true,
    }
}
