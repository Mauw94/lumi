use crate::{Expr, Obj};

pub fn debug_print_obj(obj: &Obj) {
    println!("{:?}", obj);
}

pub fn debug_print_expr(expr: &Expr) {
    println!("{:?}", expr);
}
