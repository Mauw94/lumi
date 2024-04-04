use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{LErr, LRes, Obj, ObjectType};

#[derive(Debug)]
pub struct Env {
    pub vars: HashMap<String, (ObjectType, Box<RefCell<Obj>>)>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn define(&mut self, var_name: String, obj_type: ObjectType, obj: Obj) {
        println!("defining: {}", var_name);
        self.vars
            .insert(var_name, (obj_type, Box::new(RefCell::new(obj))));
    }

    pub fn lookup_variable(
        &mut self,
        var_name: &String,
        code_loc: crate::CodeLoc,
    ) -> LRes<(ObjectType, Obj)> {
        match self.vars.get(var_name) {
            Some(obj) => {
                println!("retrieving value for: {}", var_name);
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
}
