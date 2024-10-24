use std::{cell::RefCell, rc::Rc};

use crate::{
    check_args, try_borrow_mut, CodeLoc, Env, Extension, LErr, LInt, LNum, LRes, LibType, Namespace, NamespaceType, Obj, ObjectType, Seq
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

    Ok(Obj::Seq(Seq::List(Rc::new(RefCell::new(lumi_vec)))))
}

pub fn get_list_type(lst: &Vec<Obj>) -> Result<ObjectType, LErr> {
    lst.first()
        .and_then(|first_val| Some(first_val.get_object_type()))
        .unwrap_or(Ok(ObjectType::None))
}

#[derive(Debug)]
pub struct Vector;

impl Namespace for Vector {
    fn load_functions(&self, env: &Rc<std::cell::RefCell<crate::Env>>) -> LRes<()> {
        let mut e = try_borrow_mut(env)?;

        e.insert_extension(Sum, NamespaceType::StdLib(LibType::Vec));
        e.insert_extension(Len, NamespaceType::StdLib(LibType::Vec));
        e.insert_extension(Push, NamespaceType::StdLib(LibType::Vec));
        e.insert_extension(Last, NamespaceType::StdLib(LibType::Vec));
        e.insert_extension(First, NamespaceType::StdLib(LibType::Vec));
        e.insert_extension(Pop, NamespaceType::StdLib(LibType::Vec));
        e.insert_extension(Slice, NamespaceType::StdLib(LibType::Vec));
        e.insert_extension(OrderBy, NamespaceType::StdLib(LibType::Vec));
        e.insert_extension(Take, NamespaceType::StdLib(LibType::Vec));

        Ok(())
    }

    fn unload_functions(&self, env: &Rc<std::cell::RefCell<crate::Env>>) -> LRes<()> {
        let mut e = try_borrow_mut(env)?;

        e.remove_function(Sum.extension_name())?;
        e.remove_function(Len.extension_name())?;
        e.remove_function(Push.extension_name())?;
        e.remove_function(Last.extension_name())?;
        e.remove_function(First.extension_name())?;
        e.remove_function(Pop.extension_name())?;
        e.remove_function(Slice.extension_name())?;
        e.remove_function(OrderBy.extension_name())?;
        e.remove_function(Take.extension_name())?;

        Ok(())
    }

    fn get_function_names(&self) -> Vec<String> {
        vec![
            Sum.extension_name().to_string(),
            Len.extension_name().to_string(),
            Push.extension_name().to_string(),
            Last.extension_name().to_string(),
            First.extension_name().to_string(),
            Pop.extension_name().to_string(),
            OrderBy.extension_name().to_string(),
            Take.extension_name().to_string(),
        ]
    }

    fn namespace_name(&self) -> &str {
        "vector"
    }
}

#[derive(Debug)]
struct Sum;

impl Extension for Sum {
    fn run(
        &self,
        env: &Rc<RefCell<Env>>,
        var_name: &str,
        vec: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(0, 0, &args, start, end)?;

        if vec.is_list() {
            let list_val = vec.get_list_val()?;
            let list = list_val.borrow();
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
                        let val = self.run(env, var_name, o.clone(), args.clone(), start, end)?;
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

    fn extension_name(&self) -> &str {
        "sum"
    }
}

#[derive(Debug)]
struct Len;

impl Extension for Len {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        _var_name: &str,
        vec: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(0, 0, &args, start, end)?;

        // TODO: this needs to be removed from here. These functions only go for vecs
        if vec.is_string() {
            let str_val = vec.get_str_val()?;
            Ok(Obj::Num(LNum::Int(LInt::new(str_val.len() as i64))))
        } else if vec.is_list() {
            let list_val = vec.get_list_val()?;
            let list = list_val.borrow();
            Ok(Obj::Num(LNum::Int(LInt::new(list.len() as i64))))
        } else {
            Err(LErr::runtime_error(
                format!(
                    "Argument needs to be of type list or str. Found {}",
                    vec.get_type_name()
                ),
                start,
                end,
            ))
        }
    }

    fn extension_name(&self) -> &str {
        "len"
    }
}

#[derive(Debug)]
struct Push;

impl Extension for Push {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        _var_name: &str,
        vec: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(1, 1, &args, start, end)?;

        let list_val = vec.get_list_val()?;
        let mut list = try_borrow_mut(list_val)?;
        let lst_type = get_list_type(&list)?;
        let val_to_add = args.get(0).unwrap();

        if !val_to_add.is_type(&lst_type) {
            return Err(LErr::internal_error(format!(
                "Tried adding '{}' to a list of type: '{}'",
                val_to_add.get_type_name(),
                lst_type.get_type_name()
            )));
        }

        list.push(val_to_add.clone());

        Ok(Obj::Null)
    }

    fn extension_name(&self) -> &str {
        "push"
    }
}

#[derive(Debug)]
struct Last;

impl Extension for Last {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        _var_name: &str,
        vec: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(0, 0, &args, start, end)?;

        let list_val = vec.get_list_val()?;
        let list = list_val.borrow();
        list.last().map_or_else(|| Ok(Obj::Null), |v| Ok(v.clone()))
    }

    fn extension_name(&self) -> &str {
        "last"
    }
}

#[derive(Debug)]
struct First;

impl Extension for First {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        _var_name: &str,
        vec: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(0, 0, &args, start, end)?;

        let list_val = vec.get_list_val()?;
        let list = list_val.borrow();
        list.first()
            .map_or_else(|| Ok(Obj::Null), |v| Ok(v.clone()))
    }

    fn extension_name(&self) -> &str {
        "first"
    }
}

#[derive(Debug)]
struct Pop;

impl Extension for Pop {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        _var_name: &str,
        vec: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(0, 0, &args, start, end)?;

        let list_val = vec.get_list_val()?;
        let mut list = try_borrow_mut(list_val)?;
        let result = list.pop().map_or_else(|| Ok(Obj::Null), |v| Ok(v.clone()));

        result
    }

    fn extension_name(&self) -> &str {
        "pop"
    }
}

// Slice takes 3 arguments
// The list, start index, end index (not incl)
// Returns a list.
#[derive(Debug)]
struct Slice;

impl Extension for Slice {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        _var_name: &str,
        vec: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(2, 2, &args, start, end)?;

        let from_obj = args.get(0).unwrap();
        let to_obj = args.get(1).unwrap();

        if vec.is_list() {
            let list_val = vec.get_list_val()?;
            let list = list_val.borrow();
            let from = from_obj.get_int_val()? as usize;
            let to = to_obj.get_int_val()? as usize;

            if to > list.len() {
                return Err(LErr::runtime_error(
                    format!("Index out of bounds. Index {to}."),
                    start,
                    end,
                ));
            }

            let res = &list[from..to];

            return Ok(Obj::Seq(Seq::List(Rc::new(RefCell::new(res.to_vec())))));
        } else {
            return Err(LErr::internal_error(format!(
                "Expected a list, found {}",
                vec.get_type_name()
            )));
        }
    }

    fn extension_name(&self) -> &str {
        "slice"
    }
}

#[derive(Debug)]
struct OrderBy;

// TODO: extend so it'll also work for floats, etc...
impl Extension for OrderBy {
    fn run(
        &self,
        _env: &Rc<RefCell<Env>>,
        __var_name: &str,
        obj: Obj,
        args: Vec<Obj>,
        start: CodeLoc,
        end: CodeLoc,
    ) -> LRes<Obj> {
        check_args(0, 0, &args, start, end)?;

        let list_val = obj.get_list_val()?;
        let list = list_val.borrow();
        let mut real_val: Vec<i64> = list
            .iter()
            .map(|v| v.get_int_val())
            .collect::<Result<_, _>>()?;

        real_val.sort_by(|a, b| a.abs().cmp(&b.abs()));
        let res: Vec<Obj> = real_val
            .iter()
            .map(|v| Obj::Num(LNum::Int(LInt::new(*v))))
            .collect();

        Ok(Obj::Seq(Seq::List(Rc::new(RefCell::new(res)))))
    }

    fn extension_name(&self) -> &str {
        "order_by"
    }
}

#[derive(Debug)]
struct Take;

impl Extension for Take {
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

        let take_amount = args.get(0).unwrap();
        let take = take_amount.get_int_val()?;

        let list_val = obj.get_list_val()?;
        let list = list_val.borrow();
        let res: Vec<Obj> = list.iter().take(take as usize).cloned().collect();

        Ok(Obj::Seq(Seq::List(Rc::new(RefCell::new(res)))))
    }

    fn extension_name(&self) -> &str {
        "take"
    }
}
