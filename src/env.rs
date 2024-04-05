use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{try_borrow, try_borrow_mut, Builtin, Func, LErr, LRes, Obj, ObjectType, Time};

#[derive(Debug)]
pub struct Env {
    pub vars: HashMap<String, (ObjectType, Box<RefCell<Obj>>)>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn insert_builtint(&mut self, b: impl Builtin + 'static) {
        self.insert(
            b.builtin_name().to_string(),
            ObjectType::Function,
            Obj::Func(Func::Builtin(Rc::new(b))),
        )
        .unwrap();
    }

    pub fn insert(&mut self, key: String, obj_type: ObjectType, obj: Obj) -> LRes<()> {
        self.vars
            .insert(key, (obj_type, Box::new(RefCell::new(obj))));
        Ok(())
    }
}

pub fn initialize(env: &mut Env) {
    env.insert_builtint(Time);
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

pub fn lookup_variable(
    env: &Rc<RefCell<Env>>,
    var_name: &String,
    code_loc: crate::CodeLoc,
) -> LRes<(ObjectType, Obj)> {
    let r = try_borrow(env)?;
    match r.vars.get(var_name) {
        Some(obj) => {
            let object_type = obj.0.clone();
            let object = obj.1.borrow().clone();
            return Ok((object_type, object));
        }
        None => Err(LErr::runtime_error(
            format!("Did not find variable with name: '{}'", var_name),
            code_loc,
        )),
    }
}
