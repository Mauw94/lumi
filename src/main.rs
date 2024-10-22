use std::{
    cell::RefCell,
    env, fs,
    io::{stdin, stdout, Write},
    path::Path,
    rc::Rc,
    time::Instant,
};

use lumi_lib::{
    evaluate, initialize, Dictionary, Env, LErr, LResult, Lexer, Namespace, Obj, Parser, StdLib,
    Str, Vector,
};

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
    let env = setup_env();

    let mut input = String::new();
    while prompt(&mut input) {
        let mut lexer = Lexer::new(&input);
        match lexer.lex() {
            Ok(tokens) => {
                let mut p = Parser::new(tokens, &env);
                match p.parse() {
                    Ok(expr) => match evaluate(&env, &expr) {
                        Ok(_) | Err(LErr::Return(_)) => {}
                        Err(e) => println!("{}", e.render(&input)),
                    },
                    Err(e) => println!("{}", e.render(&input)),
                }
            }
            Err(e) => println!("{}", e.render(&input)),
        };
    }
}

fn run_code(code: &str) {
    let env = setup_env();
    let mut lexer = Lexer::new(code);
    match lexer.lex() {
        Ok(tokens) => {
            let mut p = Parser::new(tokens, &env);
            match p.parse() {
                Ok(expr) => match evaluate(&env, &expr) {
                    Ok(Obj::LResult(LResult::Error(e))) => {
                        println!("are we printing here?");
                        eprintln!("{:?}", e);
                    }
                    Ok(_) | Err(LErr::Return(_)) => {}
                    Err(e) => {
                        println!("{}", e.render(&code));
                    }
                },
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
    Vector.load_functions(&ref_e).unwrap();
    Str.load_functions(&ref_e).unwrap();
    Dictionary.load_functions(&ref_e).unwrap();

    ref_e
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() <= 1 {
        repl();
    } else {
        let start = Instant::now();

        let filename = &args[1];
        let input_folder = Path::new("runnables");
        let file_path = input_folder.join(filename);

        match fs::read_to_string(&file_path) {
            Ok(content) => run_code(&content),
            Err(err) => eprintln!("Error reading file: {}", err),
        };

        let duration = start.elapsed();
        print!(
            "                                    Execution time: {:?}",
            duration
        );
    }
}
