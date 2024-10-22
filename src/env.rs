use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    try_borrow, try_borrow_mut, Builtin, CodeLoc, Extension, FileIO, Func, LErr, LRes, Namespace,
    Obj, ObjectType, Seq, StdLib, Str, Vector,
};

#[derive(Debug, PartialEq)]
pub enum NamespaceType {
    StdLib(LibType),
    External(String),
}

#[derive(Debug, PartialEq)]
pub enum LibType {
    Std,
    Str,
    Vec,
    Dict,
}

impl NamespaceType {
    fn get_name(&self) -> String {
        match self {
            NamespaceType::StdLib(lib_type) => match lib_type {
                LibType::Std => format!("stdlib"),
                LibType::Str => format!("stdlib (str)"),
                LibType::Vec => format!("stdlib (vec)"),
                LibType::Dict => format!("stdlib (dict)"),
            },
            NamespaceType::External(n) => format!("{}", n),
        }
    }
}

#[derive(Debug)]
pub struct Env {
    pub vars: HashMap<String, (ObjectType, Box<RefCell<Obj>>)>,
    pub functions: HashMap<String, (ObjectType, Box<RefCell<Obj>>, NamespaceType)>,
    pub namespaces: HashMap<String, Box<RefCell<Obj>>>,
    pub structs: HashMap<String, Box<RefCell<Obj>>>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new(closure: Option<Rc<RefCell<Env>>>) -> Env {
        Env {
            vars: HashMap::new(),
            functions: HashMap::new(),
            namespaces: HashMap::new(),
            structs: HashMap::new(),
            parent: closure,
        }
    }

    pub fn insert_builtin(&mut self, b: impl Builtin + 'static, namespace_type: NamespaceType) {
        self.insert_function(
            b.builtin_name().to_string(),
            Obj::Func(Box::new(Func::Builtin(Rc::new(b)))),
            namespace_type,
        )
        .unwrap();
    }

    pub fn insert_extension(&mut self, e: impl Extension + 'static, namespace_type: NamespaceType) {
        self.insert_function(
            e.extension_name().to_string(),
            Obj::Func(Box::new(Func::Extension(Rc::new(e)))),
            namespace_type,
        )
        .unwrap();
    }

    pub fn insert_var(&mut self, key: String, obj_type: ObjectType, obj: Obj) -> LRes<()> {
        self.vars
            .insert(key, (obj_type, Box::new(RefCell::new(obj))));
        Ok(())
    }

    pub fn insert_function(
        &mut self,
        key: String,
        obj: Obj,
        namespace_type: NamespaceType,
    ) -> LRes<()> {
        self.functions.insert(
            key,
            (
                ObjectType::Function,
                Box::new(RefCell::new(obj)),
                namespace_type,
            ),
        );
        Ok(())
    }

    pub fn insert_struct(&mut self, key: String, obj: Obj) -> LRes<()> {
        self.structs.insert(key, Box::new(RefCell::new(obj)));
        Ok(())
    }

    pub fn insert_namespace(&mut self, key: String, n: impl Namespace + 'static) {
        self.namespaces.insert(
            key,
            Box::new(RefCell::new(Obj::Func(Box::new(Func::Namespace(Rc::new(
                n,
            )))))),
        );
    }

    pub fn add_closure(&mut self, env: Rc<RefCell<Env>>) {
        self.parent = Some(env);
    }

    pub fn remove_function(&mut self, key: &str) -> Result<(), LErr> {
        match self.functions.get(key) {
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
    env.insert_namespace(StdLib.namespace_name().to_string(), StdLib);
    env.insert_namespace(StdLib.namespace_name().to_string(), Vector);
    env.insert_namespace(FileIO.namespace_name().to_string(), FileIO);
    env.insert_namespace(Str.namespace_name().to_string(), Str);
}

pub fn define_var(
    env: &Rc<RefCell<Env>>,
    var_name: String,
    obj_type: ObjectType,
    obj: Obj,
) -> Result<(), LErr> {
    let mut cur_env = try_borrow_mut(env)?;
    cur_env
        .vars
        .insert(var_name, (obj_type, Box::new(RefCell::new(obj))));
    Ok(())
}

pub fn define_function(
    env: &Rc<RefCell<Env>>,
    var_name: String,
    obj_type: ObjectType,
    obj: Obj,
) -> Result<(), LErr> {
    let mut cur_env = try_borrow_mut(env)?;
    cur_env.functions.insert(
        var_name,
        (
            obj_type,
            Box::new(RefCell::new(obj)),
            NamespaceType::StdLib(LibType::Std),
        ),
    );
    Ok(())
}

pub fn define_struct(env: &Rc<RefCell<Env>>, strct_name: String, obj: Obj) -> Result<(), LErr> {
    let mut cur_env = try_borrow_mut(env)?;
    cur_env.insert_struct(strct_name, obj).unwrap();
    Ok(())
}

pub fn undefine_var(env: &Rc<RefCell<Env>>, key: &str) -> Result<(), LErr> {
    let mut r = try_borrow_mut(env)?;
    r.vars.remove(key);

    Ok(())
}

#[derive(Debug)]
pub enum LookupType {
    Var,
    Function,
    Namespace,
    Struct,
    Unknown,
}

fn lookup_in_parent_env(
    cur_env: &Ref<Env>,
    identifier: &String,
    start: CodeLoc,
    end: CodeLoc,
    lookup_type: LookupType,
) -> LRes<(ObjectType, Obj)> {
    let type_name: &str = match lookup_type {
        LookupType::Var => "variable",
        LookupType::Function => "function",
        LookupType::Namespace => "namespace",
        LookupType::Struct => "struct",
        LookupType::Unknown => "unkown",
    };

    match &cur_env.parent {
        Some(p) => return lookup(&p, identifier, start, end, lookup_type),
        None => {
            let s_key = find_key_containing_identifier(cur_env, identifier, lookup_type);
            let f = match s_key {
                Some(k) => format!(
                    "Did not find {type_name} with name: '{}'. Did you mean '{}'?",
                    identifier, k
                ),
                None => format!("Did not find {type_name} with name: '{}'.", identifier,),
            };
            return Err(LErr::runtime_error(f, start, end));
        }
    }
}

pub fn lookup(
    env: &Rc<RefCell<Env>>,
    identifier: &String,
    start: CodeLoc,
    end: CodeLoc,
    lookup_type: LookupType,
) -> LRes<(ObjectType, Obj)> {
    let cur_env = try_borrow(env)?;
    match lookup_type {
        LookupType::Var => match cur_env.vars.get(identifier) {
            Some(obj) => {
                let object_type = obj.0.clone();
                let object = obj.1.borrow().clone();
                return Ok((object_type, object));
            }
            None => lookup_in_parent_env(&cur_env, identifier, start, end, lookup_type),
        },
        LookupType::Function => match cur_env.functions.get(identifier) {
            Some(obj) => {
                let object_type = obj.0.clone();
                let object = obj.1.borrow().clone();
                return Ok((object_type, object));
            }
            None => lookup_in_parent_env(&cur_env, identifier, start, end, lookup_type),
        },
        LookupType::Namespace => match cur_env.namespaces.get(identifier) {
            Some(obj) => {
                let object = obj.borrow().clone();
                return Ok((ObjectType::Struct, object));
            }
            None => lookup_in_parent_env(&cur_env, identifier, start, end, lookup_type),
        },
        LookupType::Struct => match cur_env.structs.get(identifier) {
            Some(obj) => {
                let object = obj.borrow().clone();
                return Ok((ObjectType::Namespace, object));
            }
            None => lookup_in_parent_env(&cur_env, identifier, start, end, lookup_type),
        },
        LookupType::Unknown => {
            if cur_env.vars.contains_key(identifier) {
                return lookup(env, identifier, start, end, LookupType::Var);
            } else if cur_env.functions.contains_key(identifier) {
                return lookup(env, identifier, start, end, LookupType::Function);
            } else if cur_env.namespaces.contains_key(identifier) {
                return lookup(env, identifier, start, end, LookupType::Namespace);
            } else if cur_env.structs.contains_key(identifier) {
                return lookup(env, identifier, start, end, LookupType::Struct);
            } else {
                match &cur_env.parent {
                    Some(parent_env) => {
                        return lookup(&parent_env, identifier, start, end, lookup_type)
                    }
                    None => {
                        let msg = String::from(format!(
                            "Did not find identifier with name: '{}'.",
                            identifier
                        ));

                        match find_key_containing_identifier(&cur_env, identifier, LookupType::Var)
                        {
                            Some(key) => {
                                return Err(LErr::runtime_error(
                                    format!(
                                        "Did not find variable with name: '{}'. Did you mean '{}'?",
                                        identifier, key
                                    ),
                                    start,
                                    end,
                                ))
                            }
                            None => (),
                        }
                        match find_key_containing_identifier(
                            &cur_env,
                            identifier,
                            LookupType::Function,
                        ) {
                            Some(key) => {
                                return Err(LErr::runtime_error(
                                    format!(
                                        "Did not find function with name: '{}'. Did you mean '{}'?",
                                        identifier, key
                                    ),
                                    start,
                                    end,
                                ))
                            }
                            None => (),
                        }
                        match find_key_containing_identifier(
                            &cur_env,
                            identifier,
                            LookupType::Struct,
                        ) {
                            Some(key) => {
                                return Err(LErr::runtime_error(
                                    format!(
                                        "Did not find struct with name: '{}'. Did you mean '{}'?",
                                        identifier, key
                                    ),
                                    start,
                                    end,
                                ))
                            }
                            None => (),
                        }
                        match find_key_containing_identifier(
                            &cur_env,
                            identifier,
                            LookupType::Namespace,
                        ) {
                            Some(key) => {
                                return Err(LErr::runtime_error(
                                    format!(
                                    "Did not find namespace with name: '{}'. Did you mean '{}'?",
                                    identifier, key
                                ),
                                    start,
                                    end,
                                ))
                            }
                            None => (),
                        }

                        return Err(LErr::runtime_error(msg, start, end));
                    }
                }
            }
        }
    }
}

pub fn get_all_builtin_functions(env: &Rc<RefCell<Env>>) -> LRes<Obj> {
    let cur_env = try_borrow(&env)?;
    let mut built_in_function_names: Vec<(String, String)> = Vec::new();

    for (_key, (obj_type, obj, namespace_type)) in &cur_env.functions {
        if let ObjectType::Function = obj_type {
            let obj_borrow = obj.borrow();
            match obj_borrow.clone() {
                Obj::Func(f) => match *f {
                    Func::Builtin(b) => {
                        built_in_function_names
                            .push((b.builtin_name().to_string(), namespace_type.get_name()));
                    }
                    _ => {} // Ignore other types of functions that are not built_in functions.
                },
                _ => return Err(LErr::internal_error("Not a function.".to_string())),
            }
        }
    }

    for name in &built_in_function_names {
        println!("{} ({})", name.0, name.1);
    }

    let eval_built_in_function_names: Vec<String> = built_in_function_names
        .iter()
        .map(|(name, n_t)| format!("{} ({})", name, n_t))
        .collect();

    return Ok(Obj::Seq(Seq::String(Rc::new(
        eval_built_in_function_names.join(", "),
    ))));
}

pub fn get_all_builtin_functions_for_namespace(
    env: &Rc<RefCell<Env>>,
    namespace_name: String,
) -> LRes<Obj> {
    let cur_env = try_borrow(&env)?;

    match cur_env
        .namespaces
        .iter()
        .filter(|(ns_n, _)| **ns_n == namespace_name)
        .next()
    {
        Some(namespace) => match namespace.1.borrow().clone() {
            Obj::Func(f) => match *f {
                Func::Namespace(n) => {
                    let mut res: Vec<String> = Vec::new();
                    for name in n.get_function_names() {
                        println!("{}", name);
                        res.push(name);
                    }

                    return Ok(Obj::Seq(Seq::String(Rc::new(res.join("")))));
                }
                _ => (),
            },
            _ => (),
        },
        None => println!("Namespace '{}' does not exist.", namespace_name),
    }

    Ok(Obj::Null)
}

pub fn get_all_namespaces(env: &Rc<RefCell<Env>>) -> LRes<Obj> {
    let cur_env = try_borrow(env)?;
    let mut namespace_names: Vec<String> = Vec::new();
    for (_key, obj) in &cur_env.namespaces {
        let obj_borrow = obj.borrow();
        match obj_borrow.clone() {
            Obj::Func(f) => match *f {
                Func::Namespace(n) => namespace_names.push(n.namespace_name().to_string()),
                _ => {} // Ignore other types of functions that are not namespaces.
            },
            _ => return Err(LErr::internal_error("Not a namespace.".to_string())),
        }
    }

    for name in &namespace_names {
        println!("{}", name);
    }

    Ok(Obj::Null)
}

pub fn get_all_extension_functions(
    env: &Rc<RefCell<Env>>,
    lib_type: &Option<NamespaceType>,
) -> LRes<Obj> {
    let cur_env = try_borrow(&env)?;
    let mut extension_names: Vec<(String, String)> = Vec::new();

    for (_key, (obj_type, obj, lib)) in &cur_env.functions {
        if let ObjectType::Function = obj_type {
            let obj_borrow = obj.borrow();
            match obj_borrow.clone() {
                Obj::Func(f) => match *f {
                    Func::Extension(b) => {
                        if lib_type.is_none() {
                            extension_names.push((b.extension_name().to_string(), lib.get_name()));
                        } else if lib_type.is_some() && lib_type.as_ref().unwrap() == lib {
                            extension_names.push((b.extension_name().to_string(), lib.get_name()));
                        }
                    }
                    _ => {} // Ignore other types of functions that are not extension functions.
                },
                _ => return Err(LErr::internal_error("Not a function.".to_string())),
            }
        }
    }

    for name in &extension_names {
        println!("{} ({})", name.0, name.1);
    }

    let eval_extension_function_names: Vec<String> = extension_names
        .iter()
        .map(|(name, n_t)| format!("{} ({})", name, n_t))
        .collect();

    return Ok(Obj::Seq(Seq::String(Rc::new(
        eval_extension_function_names.join(", "),
    ))));
}

fn find_key_containing_identifier(
    env: &Ref<'_, Env>,
    identifier: &String,
    lookup_type: LookupType,
) -> Option<String> {
    match lookup_type {
        LookupType::Var => {
            for (key, _) in env.vars.iter() {
                if key.contains(identifier) {
                    return Some(key.to_string());
                }
            }
        }
        LookupType::Function => {
            for (key, _) in env.functions.iter() {
                if key.contains(identifier) {
                    return Some(key.to_string());
                }
            }
        }
        LookupType::Namespace => {
            for (key, _) in env.namespaces.iter() {
                if key.contains(identifier) {
                    return Some(key.to_string());
                }
            }
        }
        LookupType::Struct => {
            for (key, _) in env.structs.iter() {
                if key.contains(identifier) {
                    return Some(key.to_string());
                }
            }
        }
        LookupType::Unknown => return None,
    }
    None
}
