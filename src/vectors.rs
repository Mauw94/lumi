use std::rc::Rc;

use crate::{LErr, LNum, LRes, Obj, Seq};

pub trait FromObj: Sized {
    fn from_obj(obj: &Obj) -> Result<Self, LErr>;
}

impl FromObj for i32 {
    fn from_obj(obj: &Obj) -> Result<Self, LErr> {
        obj.get_int_val()
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
