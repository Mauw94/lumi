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
pub use crate::helper::*;
pub use crate::interpreter::*;
pub use crate::lexer::*;
pub use crate::parser::*;

mod core;
mod debug;
mod env;
mod eval;
mod helper;
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
    let env = setup_env();
    let mut lexer = Lexer::new(code);
    let mut parser = Parser::new(lexer.lex().unwrap());

    match evaluate(&env, &parser.parse().unwrap()) {
        Ok(obj) => obj,
        Err(e) => match e {
            LErr::Throw(s, _) => {
                eprintln!("Something went wrong.");
                println!("{}", s);
                Obj::Null
            }
            LErr::Return(o) => o,
        },
    }
}

fn setup_env() -> Rc<RefCell<Env>> {
    let mut e = Env::new(None);
    initialize(&mut e);
    Rc::new(RefCell::new(e))
}

pub fn execute_examples() -> Result<Vec<Obj>, LErr> {
    let mut results: Vec<Obj> = Vec::new();
    let input_folder = Path::new("examples");
    if let Ok(entries) = fs::read_dir(input_folder) {
        for entry in entries {
            if let Ok(entry) = entry {
                if let Some(file_name) = entry.file_name().to_str() {
                    let file_path = input_folder.join(file_name);
                    match fs::read_to_string(&file_path) {
                        Ok(content) => results.push(quick_eval(&content)),
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

#[derive(Debug)]
struct ConcatStr;

impl Builtin for ConcatStr {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(2, 2, &args, start, end)?;

        let mut first = get_str_from_args_vec_obj(0, &args)?;
        let second = get_str_from_args_vec_obj(1, &args)?;

        first.push_str(&second);
        Ok(Obj::Seq(Seq::String(Rc::new(first))))
    }

    fn builtin_name(&self) -> &str {
        "concat_str"
    }
}

#[derive(Debug)]
struct Substr;

// Start and end index are both inclusive, with starting position 1 being the first character.
impl Builtin for Substr {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(3, 3, &args, start, end)?;

        let str_o = get_str_from_args_vec_obj(0, &args)?;
        let start_index = get_int_from_arg_obj(1, &args)? as usize;
        let end_index = get_int_from_arg_obj(2, &args)? as usize;

        if start_index > str_o.len() || start_index < 1 {
            return Err(LErr::runtime_error(
                format!("First argument is out of bounds. {}", start_index),
                start,
                end,
            ));
        }

        if end_index > str_o.len() {
            return Err(LErr::runtime_error(
                format!("Second argument is out of bounds. {}", end_index),
                start,
                end,
            ));
        }

        let new_str = &str_o[start_index..end_index];

        Ok(Obj::Seq(Seq::String(Rc::new(new_str.to_string()))))
    }

    fn builtin_name(&self) -> &str {
        "substr"
    }
}

#[derive(Debug)]
struct Len;

impl Builtin for Len {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(1, 1, &args, start, end)?;
        let obj = args.get(0).unwrap();

        if obj.is_string() {
            let res = get_str_from_args_vec_obj(0, &args)?;
            Ok(Obj::Num(LNum::Int(res.len() as i64)))
        } else if obj.is_list() {
            let res: Vec<Obj> = get_list_from_arg_obj(0, &args)?;
            Ok(Obj::Num(LNum::Int(res.len() as i64)))
        } else {
            Err(LErr::runtime_error(
                format!(
                    "Argument needs to be of type list or str. Found {}",
                    obj.get_type_name()
                ),
                start,
                end,
            ))
        }
    }

    fn builtin_name(&self) -> &str {
        "len"
    }
}

#[derive(Debug)]
struct ContainsStr;

// First argument the string that will be searched in.
// Second argument the search value.
impl Builtin for ContainsStr {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(2, 2, &args, start, end)?;

        let str_to_search_obj = args.get(0).unwrap();
        let search_val_obj = args.get(1).unwrap();

        let str_to_search = str_to_search_obj.get_str_val()?;
        let search_val = search_val_obj.get_str_val()?;

        Ok(Obj::Bool(str_to_search.contains(&search_val)))
    }

    fn builtin_name(&self) -> &str {
        "contains_str"
    }
}

#[derive(Debug)]
struct ReplaceStr;

impl Builtin for ReplaceStr {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(3, 3, &args, start, end)?;

        let str_to_replace_in_obj = args.get(0).unwrap();
        let part_to_replace_obj = args.get(1).unwrap();
        let replace_value_obj = args.get(2).unwrap();

        let str_to_replace_in = str_to_replace_in_obj.get_str_val()?;
        let part_to_replace = part_to_replace_obj.get_str_val()?;
        let replace_value = replace_value_obj.get_str_val()?;

        let modified_str = str_to_replace_in.replace(&part_to_replace, &replace_value);

        Ok(Obj::Seq(Seq::String(Rc::new(modified_str))))
    }

    fn builtin_name(&self) -> &str {
        "replace_str"
    }
}
