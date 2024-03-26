use std::{
    env,
    io::{stdin, stdout, Write},
};

use crate::{lexer::Lexer, parser::Parser};

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
                println!("{:?}", tokens);
                let mut p = Parser::new(tokens);
                match p.expression() {
                    Ok(expr) => println!("{:?}", expr),
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
