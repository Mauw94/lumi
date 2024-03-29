pub use crate::core::*;
pub use crate::debug::*;
pub use crate::eval::*;
pub use crate::interpreter::*;
pub use crate::lexer::*;
pub use crate::parser::*;

mod core;
mod debug;
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
    let mut lexer = Lexer::new(code);
    let tokens = lexer.lex().unwrap();
    println!("{:?}", tokens);
    let mut parser = Parser::new(lexer.lex().unwrap());
    let interpret = Interpreter::new();

    interpret.eval(&parser.parse().unwrap()).unwrap()
}
