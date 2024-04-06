use crate::{core::Obj, lexer::LocToken, parser::LumiExpr, AppConfig};

pub struct LDebug<'a> {
    config: &'a AppConfig,
    tokens: Option<Vec<LocToken>>,
    expr: Option<LumiExpr>,
    eval: Option<Obj>,
}

impl<'a> LDebug<'a> {
    pub fn new(config: &'a AppConfig) -> Self {
        Self {
            config,
            tokens: None,
            expr: None,
            eval: None,
        }
    }

    pub fn set_tokens(&mut self, tokens: Vec<LocToken>) {
        self.tokens = Some(tokens);
    }

    pub fn set_expr(&mut self, expr: LumiExpr) {
        self.expr = Some(expr);
    }

    pub fn set_eval(&mut self, obj: Obj) {
        self.eval = Some(obj);
    }

    pub fn debug_print(&self) {
        if self.config.is_debug_print_enabled() {
            self.print_tokens();
            self.print_expressions();
            self.print_eval_results();
        }
    }

    fn print_tokens(&self) {
        match &self.tokens {
            Some(tokens) => {
                println!("Tokens:");
                for loc_token in tokens {
                    print!("{:?} ", loc_token.token);
                }
                println!("\n");
            }
            None => {}
        }
    }

    fn print_expressions(&self) {
        match &self.expr {
            Some(expr) => println!("Expressions: \n{}", expr),
            None => {}
        }
    }

    fn print_eval_results(&self) {
        match &self.eval {
            Some(eval) => println!("\nResult after eval: {:?}\n", eval),
            None => {}
        }
    }
}
