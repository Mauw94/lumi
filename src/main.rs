use std::{
    env, fs,
    io::{stdin, stdout, Write},
    path::Path,
};

use lumi::{evaluate, AppConfig, Debug, Env, Lexer, Parser};

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
    let mut env = Env::new();
    let mut debugger = Debug::new(config);
    let mut input = String::new();
    while prompt(&mut input) {
        let mut lexer = Lexer::new(&input);
        match lexer.lex() {
            Ok(tokens) => {
                debugger.set_tokens(tokens.clone());
                debugger.debug_print();
                let mut p = Parser::new(tokens);
                match p.parse() {
                    Ok(expr) => {
                        debugger.set_expr(expr.clone());
                        match evaluate(&mut env, &expr) {
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

fn run_code(config: &AppConfig, code: &str) {
    let mut debugger = Debug::new(config);
    let mut env = Env::new();
    let mut lexer = Lexer::new(code);
    match lexer.lex() {
        Ok(tokens) => {
            debugger.set_tokens(tokens.clone());
            let mut p = Parser::new(tokens);
            match p.parse() {
                Ok(expr) => {
                    debugger.set_expr(expr.clone());
                    match evaluate(&mut env, &expr) {
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

fn main() {
    let args: Vec<String> = env::args().collect();

    let config = AppConfig::new(false);

    if args.len() <= 1 {
        repl(&config);
    } else {
        let filename = &args[1];
        let input_folder = Path::new("examples");
        let file_path = input_folder.join(filename);

        match fs::read_to_string(&file_path) {
            Ok(content) => run_code(&config, &content),
            Err(err) => eprintln!("Error reading file: {}", err),
        }
    }
}
