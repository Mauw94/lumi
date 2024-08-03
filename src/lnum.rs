use num_traits::{Bounded, NumCast};

use crate::CompareType;

#[derive(Debug, Clone, PartialEq)]
pub enum LNum {
    Byte(u8),
    Int(LInt),
    Float(f32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LInt {
    Small(i16),
    Big(i32),
    Long(i64),
}

impl LNum {
    pub fn compare_lnums(num1: &LNum, num2: &LNum, compare_type: CompareType) -> bool {
        let f1 = match num1 {
            LNum::Float(f) => *f,
            LNum::Int(int) => match int {
                LInt::Small(i) => *i as f32,
                LInt::Big(i) => *i as f32,
                LInt::Long(i) => *i as f32,
            },
            LNum::Byte(b) => *b as f32,
        };
        let f2 = match num2 {
            LNum::Float(f) => *f,
            LNum::Int(int) => match int {
                LInt::Small(i) => *i as f32,
                LInt::Big(i) => *i as f32,
                LInt::Long(i) => *i as f32,
            },
            LNum::Byte(b) => *b as f32,
        };

        match compare_type {
            CompareType::Equal => f1 == f2,
            CompareType::Greater => f1 > f2,
            CompareType::GreaterEqual => f1 >= f2,
            CompareType::Less => f1 < f2,
            CompareType::LessEqual => f1 <= f2,
        }
    }

    pub fn get_num_val_usize(&self) -> usize {
        match &self {
            LNum::Int(int) => match int {
                LInt::Small(i) => *i as usize,
                LInt::Big(i) => *i as usize,
                LInt::Long(i) => *i as usize,
            },
            LNum::Float(f) => *f as usize,
            LNum::Byte(b) => *b as usize,
        }
    }

    pub fn default_int() -> LNum {
        LNum::Int(LInt::Small(0))
    }

    pub fn default_float() -> LNum {
        LNum::Float(0.0)
    }
}

impl LInt {
    pub fn new(i: i64) -> Self {
        match i {
            _ if Self::fits_in::<i16>(i) => LInt::Small(i as i16),
            _ if Self::fits_in::<i32>(i) => LInt::Big(i as i32),
            _ => LInt::Long(i),
        }
    }

    pub fn get_as_i64(&self) -> i64 {
        match self {
            LInt::Small(i) => *i as i64,
            LInt::Big(i) => *i as i64,
            LInt::Long(i) => *i as i64,
        }
    }

    pub fn fits_in<T: Bounded + NumCast>(f: i64) -> bool {
        let min = T::min_value().to_i64().unwrap();
        let max = T::max_value().to_i64().unwrap();
        f >= min && f <= max
    }
}
