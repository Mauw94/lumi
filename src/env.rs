use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{try_borrow, try_borrow_mut, LErr, LRes, Obj, ObjectType};

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
            let object_type = obj.0.to_owned();
            let object = obj.1.borrow_mut().to_owned();
            return Ok((object_type, object));
        }
        None => Err(LErr::runtime_error(
            format!("Did not find variable with name: '{}'", var_name),
            code_loc,
        )),
    }
}
