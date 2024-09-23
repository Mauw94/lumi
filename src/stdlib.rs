use std::{cell::RefCell, rc::Rc};

use chrono::{DateTime, Local};

use crate::{
    check_args, get_all_builtin_functions, get_all_builtin_functions_for_namespace,
    get_all_extension_functions, get_all_namespaces, get_int_from_arg_obj, get_str_from_arg_obj,
    get_str_from_args_vec_obj, try_borrow, vectors, Builtin, CodeLoc, Env, LErr, LInt, LNum, LRes,
    LibType, Namespace, NamespaceType, Obj, ObjectType, Seq,
};

#[derive(Debug)]
pub struct StdLib;

impl Namespace for StdLib {
    fn load_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        let mut e = env.borrow_mut();

        // LibType::Std
        e.insert_builtin(Time, NamespaceType::StdLib(LibType::Std));
        e.insert_builtin(Stringify, NamespaceType::StdLib(LibType::Std));
        e.insert_builtin(Vars, NamespaceType::StdLib(LibType::Std));
        e.insert_builtin(BuiltIn, NamespaceType::StdLib(LibType::Std));
        e.insert_builtin(Namespaces, NamespaceType::StdLib(LibType::Std));
        e.insert_builtin(Typeof, NamespaceType::StdLib(LibType::Std));
        e.insert_builtin(Extension, NamespaceType::StdLib(LibType::Std));

        // LibType::Str
        e.insert_builtin(ConcatStr, NamespaceType::StdLib(LibType::Str));
        e.insert_builtin(Substr, NamespaceType::StdLib(LibType::Str));
        e.insert_builtin(ContainsStr, NamespaceType::StdLib(LibType::Str));
        e.insert_builtin(ReplaceStr, NamespaceType::StdLib(LibType::Str));
        e.insert_builtin(Slice, NamespaceType::StdLib(LibType::Str));

        Ok(())
    }

    fn unload_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        let mut e = env.borrow_mut();
        e.remove_function(Time.builtin_name())?;
        e.remove_function(Stringify.builtin_name())?;
        e.remove_function(Vars.builtin_name())?;
        e.remove_function(BuiltIn.builtin_name())?;
        e.remove_function(Namespaces.builtin_name())?;
        e.remove_function(Typeof.builtin_name())?;
        e.remove_function(ConcatStr.builtin_name())?;
        e.remove_function(Substr.builtin_name())?;
        e.remove_function(ContainsStr.builtin_name())?;
        e.remove_function(ReplaceStr.builtin_name())?;
        e.remove_function(Slice.builtin_name())?;
        e.remove_function(Extension.builtin_name())?;

        Ok(())
    }

    fn get_function_names(&self) -> Vec<String> {
        vec![
            Time.builtin_name().to_string(),
            Stringify.builtin_name().to_string(),
            Vars.builtin_name().to_string(),
            BuiltIn.builtin_name().to_string(),
            Namespaces.builtin_name().to_string(),
            Typeof.builtin_name().to_string(),
            ConcatStr.builtin_name().to_string(),
            Substr.builtin_name().to_string(),
            ContainsStr.builtin_name().to_string(),
            ReplaceStr.builtin_name().to_string(),
        ]
    }

    fn namespace_name(&self) -> &str {
        "stdlib"
    }
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
struct Stringify;

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
        "string"
    }
}

impl Stringify {
    fn stringify_obj(&self, obj: &Obj, start: CodeLoc, end: CodeLoc) -> Result<String, LErr> {
        match obj {
            Obj::Num(n) => match n {
                LNum::Int(i) => match i {
                    crate::LInt::Small(i) => Ok(i.to_string()),
                    crate::LInt::Big(i) => Ok(i.to_string()),
                    crate::LInt::Long(i) => Ok(i.to_string()),
                },
                LNum::Float(f) => Ok(f.to_string()),
                LNum::Byte(b) => Ok(b.to_string()),
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

        let mut res: Vec<String> = Vec::new();
        let mut out = String::new();
        write!(out, "\x1b[33m").ok();

        for v in e.vars.iter() {
            writeln!(out, "{} ({:?})", v.0, v.1 .0).ok();
            res.push(format!("{} ({:?})", v.0, v.1 .0));
        }
        write!(out, "\x1b[0m").ok();

        println!("{}", out);

        Ok(Obj::Seq(Seq::String(Rc::new(res.join(", ")))))
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

#[derive(Debug)]
struct Sum;

impl Builtin for Sum {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(1, 1, &args, start, end)?;

        let obj = args.get(0).unwrap();
        if obj.is_list() {
            let list = obj.get_list_val()?;
            let obj_type = list[0].get_object_type()?;
            match obj_type {
                ObjectType::Int => {
                    let vec = vectors::parse_lumi_list_to_rust_vec::<i32>(&list)?;
                    let sum: i32 = vec.iter().sum();

                    return Ok(Obj::Num(LNum::Int(LInt::new(sum as i64))));
                }
                ObjectType::Float => {
                    let vec = vectors::parse_lumi_list_to_rust_vec::<f32>(&list)?;
                    let sum: f32 = vec.iter().sum();

                    return Ok(Obj::Num(LNum::Float(sum)));
                }
                ObjectType::String => {
                    let vec = vectors::parse_lumi_list_to_rust_vec::<String>(&list)?;
                    let concatenated: String = vec.iter().fold(String::new(), |mut acc, s| {
                        acc.push_str(&s);
                        acc
                    });

                    return Ok(Obj::Seq(Seq::String(Rc::new(concatenated))));
                }
                ObjectType::List => {
                    let mut res: Vec<i64> = Vec::new();
                    for o in list.iter() {
                        let val = self.run(_env, vec![o.clone()], start, end)?;
                        res.push(val.get_int_val()?);
                    }

                    Ok(Obj::Num(LNum::Int(LInt::new(res.iter().sum()))))
                }
                _ => Err(LErr::internal_error(format!(
                    "Sum on object of type {} is not possible.",
                    obj_type.get_type_name()
                ))),
            }
        } else {
            Err(LErr::internal_error("Expecting a list.".to_string()))
        }
    }

    fn builtin_name(&self) -> &str {
        "sum"
    }
}

// Slice takes 3 arguments
// The list, start index, end index (not incl)
// Returns a list.
#[derive(Debug)]
struct Slice;

impl Builtin for Slice {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(3, 3, &args, start, end)?;

        let obj = args.get(0).unwrap();
        let from_obj = args.get(1).unwrap();
        let to_obj = args.get(2).unwrap();

        if obj.is_list() {
            let list = obj.get_list_val()?;
            let from = from_obj.get_int_val()? as usize;
            let to = to_obj.get_int_val()? as usize;

            if to > list.len() {
                return Err(LErr::runtime_error(
                    format!("Index out of bounds. Index {to}."),
                    start,
                    end,
                ));
            }

            let res = &list[from..to];

            return Ok(Obj::Seq(Seq::List(Rc::new(res.to_vec()))));
        } else {
            return Err(LErr::internal_error(format!(
                "Expected a list, found {}",
                obj.get_type_name()
            )));
        }
    }

    fn builtin_name(&self) -> &str {
        "slice"
    }
}

#[derive(Debug)]
struct BuiltIn;

impl Builtin for BuiltIn {
    fn run(
        &self,
        env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        _start: CodeLoc,
        _end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(0, 1, &args, _start, _end)?;

        match args.len() {
            0 => get_all_builtin_functions(env),
            1 => {
                let namespace_name = get_str_from_arg_obj(0, &args)?;
                get_all_builtin_functions_for_namespace(env, namespace_name)
            }
            _ => Ok(Obj::Null),
        }
    }

    fn builtin_name(&self) -> &str {
        "built_in"
    }
}

#[derive(Debug)]
struct Namespaces;

impl Builtin for Namespaces {
    fn run(
        &self,
        env: &Rc<RefCell<Env>>,
        _args: Vec<Obj>,
        _start: CodeLoc,
        _end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(0, 0, &_args, _start, _end)?;
        get_all_namespaces(env)?;
        Ok(Obj::Null)
    }

    fn builtin_name(&self) -> &str {
        "namespaces"
    }
}

#[derive(Debug)]
struct Extension;

// We can pass 'vec' or 'str' here (as a string) to show all extension functions for Vectors or Strings.
impl Builtin for Extension {
    fn run(
        &self,
        env: &Rc<RefCell<Env>>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(0, 1, &args, start, end)?;

        if args.len() == 0 {
            get_all_extension_functions(env, &None)?;
        } else {
            let extensions_type = &get_str_from_arg_obj(0, &args)? as &str;
            let lib_type = match extensions_type.to_lowercase().as_str() {
                "vec" => NamespaceType::StdLib(LibType::Vec),
                "str" => NamespaceType::StdLib(LibType::Str),
                _ => NamespaceType::StdLib(LibType::Std),
            };
            get_all_extension_functions(env, &Some(lib_type))?;
        }
        Ok(Obj::Null)
    }

    fn builtin_name(&self) -> &str {
        "extensions"
    }
}
