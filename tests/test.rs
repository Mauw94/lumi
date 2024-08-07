use lumi_lib::{execute_examples, quick_eval, LInt, LNum, Obj};

extern crate lumi_lib;

fn i(n: i16) -> Obj {
    Obj::i16(n)
}

fn f(n: f32) -> Obj {
    Obj::f32(n)
}

fn b(x: bool) -> Obj {
    Obj::Bool(x)
}

// fn s(str: String) -> Obj {
//     Obj::Seq(Seq::String(Rc::new(str)))
// }

// TODO: keep track of where we are in the testing phase, so when something goes wrong we can print which test failed.
// TODO: make test to check if smallint and int are created correctly
// cargo test examples -- --nocapture
#[test]
fn examples() {
    let passed_tests = match execute_examples() {
        Ok(_) => true,
        Err(e) => {
            eprintln!("{:?}", e);
            false
        }
    };
    assert_eq!(passed_tests, true);
}

#[test]
fn operators() {
    assert_eq!(quick_eval("2 + 2").unwrap(), i(4));
    assert_eq!(quick_eval("4 - 2").unwrap(), i(2));
    assert_eq!(quick_eval("3 * 4").unwrap(), i(12));
    assert_eq!(quick_eval("15 / 3").unwrap(), i(5));
    assert_eq!(quick_eval("-3 * -3 + 4").unwrap(), i(13));
    assert_eq!(quick_eval("-3 * -3 + 4").unwrap(), i(13));
    assert_eq!(
        quick_eval("-2 * 3 + 4 / 3 * -1 - 7").unwrap(),
        f(-14.333333333333334)
    );
}

#[test]
fn or() {
    assert_eq!(quick_eval("2 == 2 or \"test\" == 3").unwrap(), b(true));
    assert_eq!(
        quick_eval("2 == 3 or \"test\" == \"test\"").unwrap(),
        b(true)
    );
    assert_eq!(
        quick_eval("2 == 3 or \"test\" == \"abc\"").unwrap(),
        b(false)
    );
    assert_eq!(
        quick_eval("5 * -3 + 2 == -13 or \"test\" == \"abc\"").unwrap(),
        b(true)
    );
    assert_eq!(
        quick_eval("5 * -2 == 10 or \"this is a test\" == \"this is a test\"").unwrap(),
        b(true)
    );
}

#[test]
fn and() {
    assert_eq!(
        quick_eval("7 == 7 and \"test\" == \"test\"").unwrap(),
        b(true)
    );
    assert_eq!(
        quick_eval("7 == 8 and \"test\" == \"test\"").unwrap(),
        b(false)
    );
    assert_eq!(
        quick_eval("7 == 7 and \"test\" == \"abc\"").unwrap(),
        b(false)
    );
    assert_eq!(
        quick_eval("2 * 4 * -2 == -16 and \"test\" == \"test\"").unwrap(),
        b(true)
    );
    assert_eq!(
        quick_eval("5 == 2 and \"this is a test\" == \"test\"").unwrap(),
        b(false)
    );
}

#[test]
fn declare() {
    assert_eq!(quick_eval("a -> 2").unwrap(), Obj::Null);
    assert_eq!(quick_eval("b-> 5").unwrap(), Obj::Null);
}

#[test]
fn list_functions() {
    assert_eq!(
        quick_eval("a: list -> [1, 2, 3]\n print a.first()").unwrap(),
        Obj::Num(LNum::Int(LInt::Small(1)))
    );
    assert_eq!(
        quick_eval("a: list -> [1, 2, 3]\n print a.last()").unwrap(),
        Obj::Num(LNum::Int(LInt::Small(3)))
    );
}

#[test]
fn int_results() {
    assert_eq!(
        quick_eval("a: int -> 123\n print a").unwrap(),
        Obj::Num(LNum::Int(LInt::Small(123)))
    );
    assert_eq!(
        quick_eval("a: int -> 32768\n print a").unwrap(),
        Obj::Num(LNum::Int(LInt::Big(32768)))
    );
    assert_eq!(
        quick_eval("a: int -> 2147483648\n print a").unwrap(),
        Obj::Num(LNum::Int(LInt::Long(2147483648)))
    );
}
