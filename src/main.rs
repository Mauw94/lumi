use std::{
    env,
    io::{stdin, stdout, Write},
};

use crate::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

pub mod core;
pub mod interpreter;
pub mod lexer;
pub mod parser;

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

fn repl() {
    let mut input = String::new();
    while prompt(&mut input) {
        let mut lexer = Lexer::new(&input);
        match lexer.lex() {
            Ok(tokens) => {
                println!("tokens: {:?}", tokens);
                let mut p = Parser::new(tokens);
                match p.expression() {
                    Ok(expr) => {
                        // TODO: function to print expressions without all the CodeLoc overhead
                        println!("expressions: {:?}", expr);
                        let interpreter = Interpreter::new();
                        match interpreter.eval(&expr) {
                            Ok(x) => {
                                println!("result after eval: {:?}", x);
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

    if args.len() <= 1 {
        repl();
    }
}
