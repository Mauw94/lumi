use std::rc::Rc;

use crate::{
    check_args, define_var, undefine_var, Builtin, CodeLoc, Env, Expr, LErr, LInt, LNum, LRes,
    LibType, LumiExpr, Namespace, NamespaceType, Obj, ObjectType, Seq,
};

pub trait FromObj: Sized {
    fn from_obj(obj: &Obj) -> Result<Self, LErr>;
}

impl FromObj for i16 {
    fn from_obj(obj: &Obj) -> Result<Self, LErr> {
        let val = obj.get_int_val()?;
        Ok(val as i16)
    }
}

impl FromObj for i32 {
    fn from_obj(obj: &Obj) -> Result<Self, LErr> {
        let val = obj.get_int_val()?;
        Ok(val as i32)
    }
}

impl FromObj for String {
    fn from_obj(obj: &Obj) -> Result<Self, LErr> {
        obj.get_str_val()
    }
}

impl FromObj for f32 {
    fn from_obj(obj: &Obj) -> Result<Self, LErr> {
        obj.get_float_val()
    }
}

impl FromObj for u8 {
    fn from_obj(obj: &Obj) -> Result<Self, LErr> {
        obj.get_byte_val()
    }
}

// Usage example
// let list = obj.get_list_val()?;
// let vec = vectors::get_list_values_to_rust_vec::<i64>(&list)?;
// IMPORTANT: the return type needs to implement the trait FromObj (see above)
pub fn parse_lumi_list_to_rust_vec<T>(args: &Vec<Obj>) -> Result<Vec<T>, LErr>
where
    T: FromObj,
{
    let new_vec: Vec<T> = args
        .iter()
        .map(T::from_obj)
        .collect::<Result<Vec<T>, LErr>>()?;

    Ok(new_vec)
}

pub fn parse_u8vec_to_lumi_vec(bytes: Vec<u8>) -> LRes<Obj> {
    let lumi_vec: Vec<Obj> = bytes.iter().map(|b| Obj::Num(LNum::Byte(*b))).collect();
    Ok(Obj::Seq(Seq::List(Rc::new(lumi_vec))))
}

pub fn get_list_type(lst: Vec<Obj>) -> Result<ObjectType, LErr> {
    lst.first()
        .and_then(|first_val| Some(first_val.get_object_type()))
        .unwrap_or(Ok(ObjectType::None))
}

#[derive(Debug)]
pub struct Vector;

impl Namespace for Vector {
    fn load_functions(&self, env: &Rc<std::cell::RefCell<crate::Env>>) -> LRes<()> {
        let mut e = env.borrow_mut();

        e.insert_builtin(Sum, NamespaceType::StdLib(LibType::Vec));
        e.insert_builtin(Push, NamespaceType::StdLib(LibType::Vec));

        Ok(())
    }

    fn unload_functions(&self, env: &Rc<std::cell::RefCell<crate::Env>>) -> LRes<()> {
        let mut e = env.borrow_mut();

        e.remove_builtin(Sum.builtin_name())?;
        e.remove_builtin(Push.builtin_name())?;

        Ok(())
    }

    fn get_function_names(&self) -> Vec<String> {
        vec![
            Sum.builtin_name().to_string(),
            Push.builtin_name().to_string(),
        ]
    }

    fn namespace_name(&self) -> &str {
        "vector"
    }
}

#[derive(Debug)]
struct Sum;

impl Builtin for Sum {
    fn run(
        &self,
        env: &Rc<std::cell::RefCell<Env>>,
        _trigger: &Box<LumiExpr>,
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
                    let vec = crate::vectors::parse_lumi_list_to_rust_vec::<i32>(&list)?;
                    let sum: i32 = vec.iter().sum();

                    return Ok(Obj::Num(LNum::Int(LInt::new(sum as i64))));
                }
                ObjectType::Float => {
                    let vec = crate::vectors::parse_lumi_list_to_rust_vec::<f32>(&list)?;
                    let sum: f32 = vec.iter().sum();

                    return Ok(Obj::Num(LNum::Float(sum)));
                }
                ObjectType::String => {
                    let vec = crate::vectors::parse_lumi_list_to_rust_vec::<String>(&list)?;
                    let concatenated: String = vec.iter().fold(String::new(), |mut acc, s| {
                        acc.push_str(&s);
                        acc
                    });

                    return Ok(Obj::Seq(Seq::String(Rc::new(concatenated))));
                }
                ObjectType::List => {
                    let mut res: Vec<i64> = Vec::new();
                    for o in list.iter() {
                        let val = self.run(env, _trigger, vec![o.clone()], start, end)?;
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

#[derive(Debug)]
struct Len;

impl Builtin for Len {
    fn run(
        &self,
        _env: &Rc<std::cell::RefCell<Env>>,
        _trigger: &Box<LumiExpr>,
        _args: Vec<Obj>,
        _start: CodeLoc,
        _end: CodeLoc,
    ) -> LRes<Obj> {
        todo!()
    }

    fn builtin_name(&self) -> &str {
        "len"
    }
}

#[derive(Debug)]
struct Push;

impl Builtin for Push {
    fn run(
        &self,
        env: &Rc<std::cell::RefCell<Env>>,
        trigger: &Box<LumiExpr>,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(2, 2, &args, start, end)?;

        let mut lst = args.get(0).unwrap().get_list_val()?;
        let lst_type = get_list_type(lst.clone())?;
        let val_to_add = args.get(1).unwrap();

        if !val_to_add.is_type(&lst_type) {
            return Err(LErr::internal_error(format!(
                "Tried adding '{}' to a list of type: '{}'",
                val_to_add.get_type_name(),
                lst_type.get_type_name()
            )));
        }

        lst.push(val_to_add.clone());

        match &trigger.expr {
            Expr::Identifier(identifier) => {
                undefine_var(env, &identifier)?;
                define_var(
                    env,
                    identifier.to_string(),
                    ObjectType::List,
                    Obj::Seq(Seq::List(Rc::new(lst.clone()))),
                )?;
            }
            _ => {
                return Err(LErr::internal_error(format!(
                    "Expected an identifier here. Found {}",
                    trigger.expr
                )))
            }
        }

        Ok(Obj::Null)
    }

    fn builtin_name(&self) -> &str {
        "push"
    }
}
