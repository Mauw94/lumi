use std::{cell::RefCell, rc::Rc};

use crate::{
    check_args, try_borrow_mut, CodeLoc, Env, Extension, LRes, LibType, Namespace, NamespaceType, Obj, Seq
};

#[derive(Debug)]
pub struct Str;

impl Namespace for Str {
    fn load_functions(&self, env: &std::rc::Rc<std::cell::RefCell<Env>>) -> LRes<()> {
        let mut e = try_borrow_mut(env)?;

        e.insert_extension(Split, NamespaceType::StdLib(LibType::Str));
        e.insert_extension(Trim, NamespaceType::StdLib(LibType::Str));

        Ok(())
    }

    fn unload_functions(&self, env: &std::rc::Rc<std::cell::RefCell<Env>>) -> LRes<()> {
        let mut e = try_borrow_mut(env)?;

        e.remove_function(Split.extension_name())?;
        e.remove_function(Trim.extension_name())?;

        Ok(())
    }

    fn get_function_names(&self) -> Vec<String> {
        vec![Split.extension_name().to_string()]
    }

    fn namespace_name(&self) -> &str {
        "str"
    }
}

#[derive(Debug)]
struct Split;

impl Extension for Split {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        _var_name: &str,
        obj: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(1, 1, &args, start, end)?;

        let str_value = obj.get_str_val()?;
        let split_value = args.get(0).unwrap().get_str_val()?;

        let split_res: Vec<&str> = str_value.split(&split_value).collect();
        let res:Vec<Obj> = split_res
            .iter()
            .map(|s| Obj::Seq(Seq::String(Rc::new(s.to_string()))))
            .collect();

        Ok(Obj::Seq(Seq::List(Rc::new(RefCell::new(res)))))
    }

    fn extension_name(&self) -> &str {
        "split"
    }
}

#[derive(Debug)]
struct Trim;

impl Extension for Trim {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        _var_name: &str,
        obj: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(0, 0, &args, start, end)?;

        let str_value = obj.get_str_val()?;

        Ok(Obj::Seq(Seq::String(Rc::new(str_value.trim().to_string()))))
    }

    fn extension_name(&self) -> &str {
        "trim"
    }
}
