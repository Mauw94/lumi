use std::cell::RefCell;
use std::fmt::Debug;
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
    let tokens = lexer.lex().unwrap();
    println!("{:?}", tokens);
    let mut parser = Parser::new(lexer.lex().unwrap());

    evaluate(&env, &parser.parse().unwrap()).unwrap()
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
        if args.len() > 1 {
            return Err(LErr::runtime_error(
                format!("Expected only 1 argument, got {}.", args.len()),
                start,
                end,
            ));
        } else if args.len() == 0 {
            return Err(LErr::runtime_error(
                format!("Expected at least 1 argument."),
                start,
                end,
            ));
        }
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
