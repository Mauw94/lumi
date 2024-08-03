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
    // TOOD: check byte comparing
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

    pub fn fits_in_i16(f: i64) -> bool {
        f >= i16::MIN as i64 && f <= i16::MAX as i64
    }

    pub fn fits_in_i32(f: i64) -> bool {
        f >= i32::MIN as i64 && f <= i32::MAX as i64
    }

    pub fn make_int(i: i64) -> LNum {
        if LNum::fits_in_i16(i) {
            LNum::Int(LInt::Small(i as i16))
        } else if LNum::fits_in_i32(i) {
            LNum::Int(LInt::Big(i as i32))
        } else {
            LNum::Int(LInt::Long(i as i64))
        }
    }
}

impl LInt {
    pub fn new(i: i64) -> Self {
        if LNum::fits_in_i16(i) {
            LInt::Small(i as i16)
        } else if LNum::fits_in_i32(i) {
            LInt::Big(i as i32)
        } else {
            LInt::Long(i)
        }
    }
}
