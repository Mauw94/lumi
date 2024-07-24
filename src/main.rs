use std::{
    cell::RefCell,
    env, fs,
    io::{stdin, stdout, Write},
    path::Path,
    rc::Rc,
    time::Instant,
};

use lumi::{evaluate, initialize, AppConfig, Env, LDebug, LErr, Lexer, Namespace, Parser, StdLib};

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
    let env = setup_env();

    let mut debugger = LDebug::new(config);
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
                        debugger.debug_print();
                        match evaluate(&env, &expr) {
                            Ok(x) => {
                                debugger.set_eval(x.clone());
                                debugger.debug_print();
                                x.print_value();
                            }
                            Err(e) => println!("{}", e.render(&input)),
                        }
                    }
                    Err(e) => println!("{}", e.render(&input)),
                }
            }
            Err(e) => println!("{}", e.render(&input)),
        };
    }
}

fn run_code(config: &AppConfig, code: &str) {
    let env = setup_env();
    let mut debugger = LDebug::new(config);
    let mut lexer = Lexer::new(code);
    match lexer.lex() {
        Ok(tokens) => {
            debugger.set_tokens(tokens.clone());
            debugger.debug_print();
            let mut p = Parser::new(tokens);
            match p.parse() {
                Ok(expr) => {
                    debugger.set_expr(expr.clone());
                    debugger.debug_print();
                    match evaluate(&env, &expr) {
                        Ok(x) | Err(LErr::Return(x)) => {
                            debugger.set_eval(x.clone());
                            debugger.debug_print();
                        }
                        Err(e) => {
                            println!("{}", e.render(&code));
                        }
                    }
                }
                Err(e) => println!("{}", e.render(&code)),
            }
        }
        Err(e) => println!("{}", e.render(&code)),
    }
}

fn setup_env() -> Rc<RefCell<Env>> {
    let mut e = Env::new(None);
    initialize(&mut e);
    let ref_e = Rc::new(RefCell::new(e));
    StdLib.load_functions(&ref_e).unwrap();
    ref_e
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let config = AppConfig::new(false);

    if args.len() <= 1 {
        repl(&config);
    } else {
        let start = Instant::now();

        let filename = &args[1];
        let input_folder = Path::new("examples");
        let file_path = input_folder.join(filename);

        match fs::read_to_string(&file_path) {
            Ok(content) => run_code(&config, &content),
            Err(err) => eprintln!("Error reading file: {}", err),
        }

        let duration = start.elapsed();
        print!(
            "                                    Execution time: {:?}",
            duration
        );
    }
}
