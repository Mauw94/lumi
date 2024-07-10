use crate::{LErr, Obj};

pub trait FromObj: Sized {
    fn from_obj(obj: &Obj) -> Result<Self, LErr>;
}

impl FromObj for i64 {
    fn from_obj(obj: &Obj) -> Result<Self, LErr> {
        obj.get_int_val()
    }
}

impl FromObj for String {
    fn from_obj(obj: &Obj) -> Result<Self, LErr> {
        obj.get_str_val()
    }
}

impl FromObj for f64 {
    fn from_obj(obj: &Obj) -> Result<Self, LErr> {
        obj.get_float_val()
    }
}

pub fn get_list_values_to_rust_vec<T>(args: &Vec<Obj>) -> Result<Vec<T>, LErr>
where
    T: FromObj,
{
    let mut new_vec: Vec<T> = Vec::new();
    for val in args.iter() {
        let value = T::from_obj(val)?;
        new_vec.push(value);
    }

    Ok(new_vec)
}
