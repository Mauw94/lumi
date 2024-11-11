use std::{cell::RefCell, rc::Rc};

use crate::{
    check_args, try_borrow_mut, CodeLoc, Env, Extension, LErr, LRes, LibType, Namespace,
    NamespaceType, Obj,
};

#[derive(Debug)]
pub struct Dictionary;

impl Namespace for Dictionary {
    fn load_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        let mut e = try_borrow_mut(env)?;

        e.insert_extension(Get, NamespaceType::StdLib(LibType::Dict));
        e.insert_extension(Insert, NamespaceType::StdLib(LibType::Dict));

        Ok(())
    }

    fn unload_functions(&self, env: &Rc<RefCell<Env>>) -> LRes<()> {
        let mut e = try_borrow_mut(env)?;

        e.remove_function(Get.extension_name())?;
        e.remove_function(Insert.extension_name())?;

        Ok(())
    }

    fn get_function_names(&self) -> Vec<String> {
        vec![Get.extension_name().to_string()]
    }

    fn namespace_name(&self) -> &str {
        "dictionary"
    }
}

#[derive(Debug)]
struct Get;

impl Extension for Get {
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
        check_is_dict(&obj, start, end)?;

        let key = args.get(0).unwrap();
        let dict = obj.get_dict_val()?;
        let dict_mut = try_borrow_mut(dict)?;

        let v = match dict_mut.get(key) {
            Some(v) => v,
            None => {
                return Err(LErr::internal_error(format!(
                    "Did not find value for key {:?} in dictionary.",
                    key
                )))
            }
        };

        Ok(v.clone())
    }

    fn extension_name(&self) -> &str {
        "get"
    }
}

#[derive(Debug)]
struct Insert;

impl Extension for Insert {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        _var_name: &str,
        obj: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(2, 2, &args, start, end)?;
        check_is_dict(&obj, start, end)?;

        let key = args.get(0).unwrap();
        let val = args.get(1).unwrap();
        let dict = obj.get_dict_val()?;
        let mut dict_mut = try_borrow_mut(dict)?;
        dict_mut.insert(key.clone(), val.clone());

        Ok(Obj::Null)
    }

    fn extension_name(&self) -> &str {
        "insert"
    }
}

fn check_is_dict(obj: &Obj, start: CodeLoc, end: CodeLoc) -> Result<(), LErr> {
    if !obj.is_dict() {
        return Err(LErr::runtime_error(
            format!("Expected a dictionary here."),
            start,
            end,
        ));
    }

    Ok(())
}
