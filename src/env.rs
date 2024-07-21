use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    try_borrow, try_borrow_mut, BuiltIn, Builtin, CodeLoc, ConcatStr, ContainsStr, FileIO, Func,
    LErr, LRes, Len, Namespace, Namespaces, Obj, ObjectType, ReplaceStr, Slice, Stringify, Substr,
    Sum, Time, Typeof, Vars,
};

#[derive(Debug)]
pub struct Env {
    pub vars: HashMap<String, (ObjectType, Box<RefCell<Obj>>)>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new(closure: Option<Rc<RefCell<Env>>>) -> Env {
        Env {
            vars: HashMap::new(),
            parent: closure,
        }
    }

    pub fn insert_builtin(&mut self, b: impl Builtin + 'static) {
        self.insert(
            b.builtin_name().to_string(),
            ObjectType::Function,
            Obj::Func(Box::new(Func::Builtin(Rc::new(b)))),
        )
        .unwrap();
    }

    pub fn insert_namespace(&mut self, n: impl Namespace + 'static) {
        self.insert(
            n.namespace_name().to_string(),
            ObjectType::Namespace,
            Obj::Func(Box::new(Func::Namespace(Rc::new(n)))),
        )
        .unwrap();
    }

    pub fn insert(&mut self, key: String, obj_type: ObjectType, obj: Obj) -> LRes<()> {
        self.vars
            .insert(key, (obj_type, Box::new(RefCell::new(obj))));
        Ok(())
    }

    pub fn add_closure(&mut self, env: Rc<RefCell<Env>>) {
        self.parent = Some(env);
    }

    pub fn remove_builtin(&mut self, key: &str) -> Result<(), LErr> {
        match self.vars.get(key) {
            Some(built_in) => match built_in.0 {
                ObjectType::Function => {
                    self.vars.remove(key);
                    return Ok(());
                }
                _ => {
                    return Err(LErr::internal_error(format!(
                        "Expected to be removing a built-in function. Found {} instead",
                        built_in.0.get_type_name()
                    )))
                }
            },
            None => {
                return Err(LErr::internal_error(
                    "Tried removing a non-existing built-in funciton.".to_string(),
                ))
            }
        }
    }
}

pub fn initialize(env: &mut Env) {
    env.insert_builtin(Time);
    env.insert_builtin(Stringify {
        name: "string".to_string(),
    });
    env.insert_builtin(Vars);
    env.insert_builtin(BuiltIn);
    env.insert_builtin(Namespaces);
    env.insert_builtin(Typeof);
    env.insert_builtin(ConcatStr);
    env.insert_builtin(Substr);
    env.insert_builtin(Len);
    env.insert_builtin(ContainsStr);
    env.insert_builtin(ReplaceStr);
    env.insert_builtin(Sum);
    env.insert_builtin(Slice);

    env.insert_namespace(FileIO);
}

pub fn define(
    env: &Rc<RefCell<Env>>,
    var_name: String,
    obj_type: ObjectType,
    obj: Obj,
) -> Result<(), LErr> {
    let mut r = try_borrow_mut(env)?;
    r.vars
        .insert(var_name, (obj_type, Box::new(RefCell::new(obj))));
    Ok(())
}

pub fn undefine(env: &Rc<RefCell<Env>>, key: &str) -> Result<(), LErr> {
    let mut r = try_borrow_mut(env)?;
    r.vars.remove(key);

    Ok(())
}

// TODO: expand this so it knows what it is looking for (variable, function, namespace, struct, ..)
pub fn lookup_variable(
    env: &Rc<RefCell<Env>>,
    var_name: &String,
    start: CodeLoc,
    end: CodeLoc,
) -> LRes<(ObjectType, Obj)> {
    let cur_env = try_borrow(env)?;
    match cur_env.vars.get(var_name) {
        Some(obj) => {
            let object_type = obj.0.clone();
            let object = obj.1.borrow().clone();
            return Ok((object_type, object));
        }
        None => match &cur_env.parent {
            Some(p) => return lookup_variable(&p, var_name, start, end),
            None => {
                let s_key = find_key_containing_var(cur_env, var_name);
                let f = match s_key {
                    Some(k) => format!(
                        "Did not find variable with name: '{}'. Did you mean '{}'?",
                        var_name, k
                    ),
                    None => format!("Did not find variable with name: '{}'.", var_name,),
                };
                return Err(LErr::runtime_error(f, start, end));
            }
        },
    }
}

pub fn get_all_builtin_functions(env: &Rc<RefCell<Env>>) -> LRes<Obj> {
    let cur_env = try_borrow(&env)?;
    let mut built_in_function_names: Vec<String> = Vec::new();
    // TOOD: also check parent env for built_in function names. Here we could be nested in a closure
    for (_key, (obj_type, obj)) in &cur_env.vars {
        if let ObjectType::Function = obj_type {
            let obj_borrow = obj.borrow();
            match obj_borrow.clone() {
                Obj::Func(f) => match *f {
                    Func::Builtin(b) => built_in_function_names.push(b.builtin_name().to_string()),
                    _ => return Err(LErr::internal_error("Not a function.".to_string())),
                },
                _ => return Err(LErr::internal_error("Not a function.".to_string())),
            }
        }
    }

    for name in &built_in_function_names {
        println!("{}", name);
    }

    Ok(Obj::Null)
}

pub fn get_all_namespaces(env: &Rc<RefCell<Env>>) -> LRes<Obj> {
    let cur_env = try_borrow(env)?;
    let mut namespace_names: Vec<String> = Vec::new();
    // TOOD: also check parent env for built_in function names. Here we could be nested in a closure
    for (_key, (obj_type, obj)) in &cur_env.vars {
        if let ObjectType::Namespace = obj_type {
            let obj_borrow = obj.borrow();
            match obj_borrow.clone() {
                Obj::Func(f) => match *f {
                    Func::Namespace(n) => namespace_names.push(n.namespace_name().to_string()),
                    _ => return Err(LErr::internal_error("Not a namespace.".to_string())),
                },
                _ => return Err(LErr::internal_error("Not a namespace.".to_string())),
            }
        }
    }

    for name in &namespace_names {
        println!("{}", name);
    }

    Ok(Obj::Null)
}

fn find_key_containing_var(env: Ref<'_, Env>, var_name: &String) -> Option<String> {
    for (key, _) in env.vars.iter() {
        if key.contains(var_name) {
            return Some(key.to_string());
        }
    }
    None
}
