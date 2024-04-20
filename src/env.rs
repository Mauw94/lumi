use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    try_borrow, try_borrow_mut, Builtin, CodeLoc, ConcatStr, Func, LErr, LRes, Len, Obj,
    ObjectType, Stringify, Substr, Time, Typeof, Vars,
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

    pub fn insert_builtint(&mut self, b: impl Builtin + 'static) {
        self.insert(
            b.builtin_name().to_string(),
            ObjectType::Function,
            Obj::Func(Box::new(Func::Builtin(Rc::new(b)))),
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
}

pub fn initialize(env: &mut Env) {
    env.insert_builtint(Time);
    env.insert_builtint(Stringify {
        name: "string".to_string(),
    });
    env.insert_builtint(Vars);
    env.insert_builtint(Typeof);
    env.insert_builtint(ConcatStr);
    env.insert_builtint(Substr);
    env.insert_builtint(Len);
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

fn find_key_containing_var(env: Ref<'_, Env>, var_name: &String) -> Option<String> {
    for (key, _) in env.vars.iter() {
        if key.contains(var_name) {
            return Some(key.to_string());
        }
    }
    None
}
