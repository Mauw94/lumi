use std::cell::RefCell;
use std::fmt::Debug;
use std::fs;
use std::path::Path;
use std::rc::Rc;

use chrono::DateTime;
use chrono::Local;

pub use crate::core::*;
pub use crate::debug::*;
pub use crate::env::*;
pub use crate::eval::*;
pub use crate::interpreter::*;
pub use crate::lexer::*;
pub use crate::parser::*;

mod core;
mod debug;
mod env;
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
    let env = Rc::new(RefCell::new(Env::new(None)));
    let mut lexer = Lexer::new(code);
    let mut parser = Parser::new(lexer.lex().unwrap());

    match evaluate(&env, &parser.parse().unwrap()) {
        Ok(obj) => obj,
        Err(e) => match e {
            LErr::Throw(_, _) => todo!(),
            LErr::Return(o) => o,
        },
    }
}

pub fn execute_examples() -> Result<Vec<Obj>, LErr> {
    let mut results: Vec<Obj> = Vec::new();
    let input_folder = Path::new("examples/tests");
    if let Ok(entries) = fs::read_dir(input_folder) {
        for entry in entries {
            if let Ok(entry) = entry {
                if let Some(file_name) = entry.file_name().to_str() {
                    let file_path = input_folder.join(file_name);
                    match fs::read_to_string(&file_path) {
                        Ok(content) => results.push(quick_eval(&content)),
                        Err(err) => {
                            return Err(LErr::internal_error(format!(
                                "Error reading file: {}",
                                err
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

#[derive(Debug)]
struct Time;

impl Builtin for Time {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        _args: Vec<Obj>,
        _start: CodeLoc,
        _end: CodeLoc,
    ) -> LRes<Obj> {
        let local: DateTime<Local> = Local::now();
        Ok(Obj::Output(format!(
            "Current time is {}",
            local.format("%Y-%m-%d %H:%M:%S")
        )))
    }

    fn builtin_name(&self) -> &str {
        "time"
    }
}

#[derive(Debug)]
struct Stringify {
    name: String,
}

impl Builtin for Stringify {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(1, 1, &args, start, end)?;
        let a = args.first().unwrap();
        match self.stringify_obj(a, start, end) {
            Ok(s) => Ok(Obj::Seq(Seq::String(Rc::new(s)))),
            Err(e) => Err(e),
        }
    }

    fn builtin_name(&self) -> &str {
        &self.name
    }
}

impl Stringify {
    fn stringify_obj(&self, obj: &Obj, start: CodeLoc, end: CodeLoc) -> Result<String, LErr> {
        match obj {
            Obj::Num(n) => match n {
                LNum::Int(i) => Ok(i.to_string()),
                LNum::Float(f) => Ok(f.to_string()),
            },
            Obj::Bool(b) => Ok(b.to_string()),
            Obj::Seq(seq) => match seq {
                Seq::String(s) => Ok(s.to_string()),
                Seq::List(lst) => {
                    let res = lst
                        .iter()
                        .map(|o| self.stringify_obj(o, start, end))
                        .collect::<Result<Vec<String>, LErr>>()?;
                    let mut list_res = String::new();
                    list_res.push_str("[");
                    for (i, s) in res.iter().enumerate() {
                        list_res.push_str(&s);
                        if i != res.len() - 1 {
                            list_res.push_str(", ");
                        }
                    }
                    list_res.push_str("]");

                    Ok(list_res)
                }
            },
            _ => Err(LErr::runtime_error(
                format!("Cannot stringify type {}", obj.get_type_name()),
                start,
                end,
            )),
        }
    }
}

#[derive(Debug)]
struct Vars;

impl Builtin for Vars {
    fn run(
        &self,
        env: &Rc<RefCell<Env>>,
        _args: Vec<Obj>,
        _start: CodeLoc,
        _end: CodeLoc,
    ) -> LRes<Obj> {
        use std::fmt::Write;

        let e = try_borrow(env)?;

        let mut out = String::new();
        write!(out, "\x1b[33m").ok();

        for v in e.vars.iter() {
            writeln!(out, "{} ({:?})", v.0, v.1 .0).ok();
        }
        write!(out, "\x1b[0m").ok();

        println!("{}", out);

        Ok(Obj::Null)
    }

    fn builtin_name(&self) -> &str {
        "vars"
    }
}

#[derive(Debug)]
struct Typeof;

impl Builtin for Typeof {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(1, 1, &args, start, end)?;

        use std::fmt::Write;

        let mut out = String::new();
        write!(out, "\x1b[33m").ok();

        let var = args.first().unwrap();
        write!(out, "{}", var.get_type_name()).ok();
        write!(out, "\x1b[0m").ok();

        println!("{}", out);

        Ok(Obj::Null)
    }

    fn builtin_name(&self) -> &str {
        "typeof"
    }
}

fn check_args(
    min: usize,
    max: usize,
    args: &Vec<Obj>,
    start: CodeLoc,
    end: CodeLoc,
) -> Result<(), LErr> {
    // FIXME
    // a: int
    // typeof(a) returns nill
    // should return int

    if args.len() > max {
        return Err(LErr::runtime_error(
            format!("Expected only 1 argument, got {}.", args.len()),
            start,
            end,
        ));
    } else if args.len() < min {
        return Err(LErr::runtime_error(
            format!("Expected at least 1 argument."),
            start,
            end,
        ));
    }

    Ok(())
}
