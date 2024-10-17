#[macro_use]
extern crate lazy_static;

use std::cell::RefCell;
use std::fmt::Debug;
use std::fs;
use std::path::Path;
use std::rc::Rc;
use std::sync::Mutex;

use wasm_bindgen::prelude::wasm_bindgen;

pub use crate::core::*;
pub use crate::env::*;
pub use crate::eval::*;
pub use crate::execute::*;
pub use crate::fileio::*;
pub use crate::helper::*;
pub use crate::interpreter::*;
pub use crate::lexer::*;
pub use crate::lnum::*;
pub use crate::parser::*;
pub use crate::stdlib::*;
pub use crate::str::*;
pub use crate::vectors::*;

mod core;
mod env;
mod eval;
mod execute;
mod fileio;
mod helper;
mod interpreter;
mod lexer;
mod lnum;
mod parser;
mod stdlib;
mod str;
mod vectors;

pub struct Eval {
    res: Vec<String>,
}

lazy_static! {
    static ref EVAL: Mutex<Eval> = Mutex::new(Eval { res: Vec::new() });
}

pub fn print_eval() {
    let eval = EVAL.lock().unwrap();
    for item in &eval.res {
        println!("{}", item);
    }
}

pub fn get_eval() -> Vec<String> {
    let eval = EVAL.lock().unwrap();

    eval.res.clone()
}

pub fn quick_eval(code: &str) -> Result<Obj, LErr> {
    let env = setup_env();
    let mut lexer = Lexer::new(code);
    let mut parser = Parser::new(lexer.lex().unwrap());

    match evaluate(&env, &parser.parse().unwrap()) {
        Ok(obj) => Ok(obj),
        Err(e) => match e {
            LErr::Throw(s, _) => {
                println!("{:?}", code);
                Err(LErr::internal_error(s))
            }
            LErr::Return(_) => Ok(Obj::Null),
        },
    }
}

fn setup_env() -> Rc<RefCell<Env>> {
    let mut e = Env::new(None);
    initialize(&mut e);
    let ref_e = Rc::new(RefCell::new(e));
    StdLib.load_functions(&ref_e).unwrap();
    Vector.load_functions(&ref_e).unwrap();
    Str.load_functions(&ref_e).unwrap();
    ref_e
}

#[wasm_bindgen]
pub fn run_code(code: &str) -> Vec<String> {
    let env = setup_env();
    let mut lexer = Lexer::new(code);
    match lexer.lex() {
        Ok(tokens) => {
            let mut p = Parser::new(tokens);
            match p.parse() {
                Ok(expr) => match evaluate(&env, &expr) {
                    Ok(_) | Err(LErr::Return(_)) => {}
                    Err(e) => {
                        add_err_res_to_eval(e.render(&code));
                    }
                },
                Err(e) => {
                    add_err_res_to_eval(e.render(&code));
                }
            }
        }
        Err(e) => {
            add_err_res_to_eval(e.render(&code));
        }
    }

    print_eval();
    get_eval()
}

fn add_err_res_to_eval(res: String) {
    let mut eval = EVAL.lock().unwrap();
    eval.res = vec![res];
}

pub fn execute_examples() -> Result<Vec<Obj>, LErr> {
    let mut results: Vec<Obj> = Vec::new();
    let input_folder = Path::new("examples");
    if let Ok(entries) = fs::read_dir(input_folder) {
        println!("{:?}", entries);
        for entry in entries {
            if let Ok(entry) = entry {
                if let Some(file_name) = entry.file_name().to_str() {
                    let file_path = input_folder.join(file_name);
                    match fs::read_to_string(&file_path) {
                        Ok(content) => match quick_eval(&content) {
                            Ok(result) => results.push(result),
                            Err(e) => match e {
                                LErr::Throw(_, _) => return Err(e),
                                LErr::Return(_) => println!("{:?}", e),
                            },
                        },
                        Err(err) => {
                            return Err(LErr::internal_error(format!(
                                "Error reading file: {}. File name: {}",
                                err, &file_name
                            )));
                        }
                    }
                }
            }
        }
    } else {
        println!("Failed to read folder contents.");
    }

    Ok(results)
}

pub trait Namespace: Debug {
    fn load_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()>;
    fn unload_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()>;
    fn get_function_names(&self) -> Vec<String>;
    fn namespace_name(&self) -> &str;
}

pub trait Builtin: Debug {
    fn run(
        &self,
        env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj>;

    fn builtin_name(&self) -> &str;
}

pub trait Extension: Debug {
    fn run(
        &self,
        env: &Rc<RefCell<Env>>,
        var_name: &str,
        obj: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj>;
    fn extension_name(&self) -> &str;
}

fn check_args(
    min: usize,
    max: usize,
    args: &Vec<Obj>,
    start: CodeLoc,
    end: CodeLoc,
) -> Result<(), LErr> {
    if args.len() > max {
        return Err(LErr::runtime_error(
            format!("Expected only 1 argument, got {}.", args.len()),
            start,
            end,
        ));
    } else if args.len() < min {
        return Err(LErr::runtime_error(
            format!("Expected at least {} arguments.", min),
            start,
            end,
        ));
    }

    Ok(())
}
