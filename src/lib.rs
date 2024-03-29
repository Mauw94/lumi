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

// pub fn simple_eval(code: &str) -> Obj {
//     let mut parser: Parser = Parser::new();
// }

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
