use std::cell::RefCell;
use std::rc::Rc;

use chrono::DateTime;
use chrono::Local;

pub use crate::core::*;
pub use crate::debug::*;
pub use crate::env::*;
pub use crate::eval::*;
pub use crate::interpreter::*;
pub use crate::lexer::*;
pub use crate::parser::*;

mod core;
mod debug;
mod env;
mod eval;
mod interpreter;
mod lexer;
mod parser;

pub struct AppConfig {
    debug_print_enabled: bool,
}

impl AppConfig {
    pub fn new(debug_print_enabled: bool) -> Self {
        Self {
            debug_print_enabled,
        }
    }

    pub fn is_debug_print_enabled(&self) -> bool {
        self.debug_print_enabled
    }
}

pub fn quick_eval(code: &str) -> Obj {
    let env = Rc::new(RefCell::new(Env::new()));
    let mut lexer = Lexer::new(code);
    let tokens = lexer.lex().unwrap();
    println!("{:?}", tokens);
    let mut parser = Parser::new(lexer.lex().unwrap());

    evaluate(&env, &parser.parse().unwrap()).unwrap()
}

#[derive(Debug)]
struct Time;

impl Builtin for Time {
    fn run(&self, _env: &Rc<RefCell<Env>>, _args: Vec<Obj>) -> LRes<Obj> {
        let local: DateTime<Local> = Local::now();
        Ok(Obj::Output(format!(
            "Current time is {}",
            local.format("%Y-%m-%d %H:%M:%S")
        )))
    }

    fn builtin_name(&self) -> &str {
        "time"
    }
}
