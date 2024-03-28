use std::{
    env,
    io::{stdin, stdout, Write},
};

use debug::Debug;

use crate::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

pub mod core;
pub mod debug;
pub mod eval;
pub mod interpreter;
pub mod lexer;
pub mod parser;

struct AppConfig {
    debug_print_enabled: bool,
}

impl AppConfig {
    fn new(debug_print_enabled: bool) -> Self {
        Self {
            debug_print_enabled,
        }
    }

    fn is_debug_print_enabled(&self) -> bool {
        self.debug_print_enabled
    }
}

fn prompt(input: &mut String) -> bool {
    input.clear();
    print!("lumi> ");
    if stdout().flush().is_err() {
        return false;
    }

    match stdin().read_line(input) {
        Err(_) => false,
        Ok(_) => true,
    }
}

fn repl(config: &AppConfig) {
    let mut debugger = Debug::new(config);
    let mut input = String::new();
    while prompt(&mut input) {
        let mut lexer = Lexer::new(&input);
        match lexer.lex() {
            Ok(tokens) => {
                debugger.set_tokens(tokens.clone());
                let mut p = Parser::new(tokens);
                match p.expression() {
                    Ok(expr) => {
                        debugger.set_expr(expr.clone());
                        let interpreter = Interpreter::new();
                        match interpreter.eval(&expr) {
                            Ok(x) => {
                                debugger.set_eval(x.clone());
                                debugger.debug_print();
                                x.print_value();
                            }
                            Err(e) => e.render(),
                        }
                    }
                    Err(e) => e.render(),
                }
            }
            Err(e) => e.render(),
        };
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let config = AppConfig::new(false);

    if args.len() <= 1 {
        repl(&config);
    }
}
