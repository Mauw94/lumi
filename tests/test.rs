use lumi_lib::{execute_examples, quick_eval, Obj};

extern crate lumi_lib;

fn i(n: i64) -> Obj {
    Obj::i64(n)
}

fn f(n: f64) -> Obj {
    Obj::f64(n)
}

fn b(x: bool) -> Obj {
    Obj::Bool(x)
}

// fn s(str: String) -> Obj {
//     Obj::Seq(Seq::String(Rc::new(str)))
// }

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
