use std::{
    env,
    io::{stdin, stdout, Write},
};

use crate::lexer::Lexer;

pub mod interpreter;
pub mod lexer;

fn prompt(input: &mut String) -> bool {
    input.clear();
    print!("> ");
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
        lexer.lex();
        println!("{:?}", lexer.tokens);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() <= 1 {
        repl();
    }
}
