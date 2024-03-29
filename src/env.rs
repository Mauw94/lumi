use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{LErr, LRes, Obj};

#[derive(Debug)]
pub struct Env {
    pub vars: HashMap<String, Box<RefCell<Obj>>>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn define(&mut self, var_name: String, obj: Obj) {
        self.vars.insert(var_name, Box::new(RefCell::new(obj)));
    }

    pub fn lookup_variable(&mut self, var_name: &String, code_loc: crate::CodeLoc) -> LRes<Obj> {
        match self.vars.get(var_name) {
            Some(obj) => Ok(obj.borrow_mut().to_owned()),
            None => Err(LErr::runtime_error(
                format!("Did not find variable with name: {}", var_name),
                code_loc,
            )),
        }
    }
}
