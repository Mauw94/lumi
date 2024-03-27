use crate::{lexer::LocToken, parser::LumiExpr};

pub fn debug_print_expressions(expr: LumiExpr) {
    println!("Expressions: \n{}", expr);
}

pub fn debug_print_tokens(tokens: Vec<LocToken>) {
    println!("Tokens:");
    for loc_token in &tokens {
        print!("{:?} ", loc_token.token);
    }
    println!("\n");
}
