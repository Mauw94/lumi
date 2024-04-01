use std::rc::Rc;

use lumi::{quick_eval, Obj, Seq};

extern crate lumi;

fn i(n: i64) -> Obj {
    Obj::i64(n)
}

fn f(n: f64) -> Obj {
    Obj::f64(n)
}

fn b(x: bool) -> Obj {
    Obj::Bool(x)
}

fn s(str: String) -> Obj {
    Obj::Seq(Seq::String(Rc::new(str)))
}

#[test]
fn operators() {
    assert_eq!(quick_eval("2 + 2"), f(4.0));
    assert_eq!(quick_eval("4 - 2"), f(2.0));
    assert_eq!(quick_eval("3 * 4"), f(12.0));
    assert_eq!(quick_eval("15 / 3"), f(5.0));
    assert_eq!(quick_eval("-3 * -3 + 4"), f(13.0));
    assert_eq!(quick_eval("-3 * -3 + 4"), f(13.0));
    assert_eq!(
        quick_eval("-2 * 3 + 4 / 3 * -1 - 7"),
        f(-14.333333333333334)
    );
}

#[test]
fn or() {
    assert_eq!(quick_eval("2 == 2 or \"test\" == 3"), b(true));
    assert_eq!(quick_eval("2 == 3 or \"test\" == \"test\""), b(true));
    assert_eq!(quick_eval("2 == 3 or \"test\" == \"abc\""), b(false));
    assert_eq!(
        quick_eval("5 * -3 + 2 == -13 or \"test\" == \"abc\""),
        b(true)
    );
    assert_eq!(
        quick_eval("5 * -2 == 10 or \"this is a test\" == \"this is a test\""),
        b(true)
    );
}

#[test]
fn and() {
    assert_eq!(quick_eval("7 == 7 and \"test\" == \"test\""), b(true));
    assert_eq!(quick_eval("7 == 8 and \"test\" == \"test\""), b(false));
    assert_eq!(quick_eval("7 == 7 and \"test\" == \"abc\""), b(false));
    assert_eq!(
        quick_eval("2 * 4 * -2 == -16 and \"test\" == \"test\""),
        b(true)
    );
    assert_eq!(
        quick_eval("5 == 2 and \"this is a test\" == \"test\""),
        b(false)
    );
}

#[test]
fn print() {
    assert_eq!(quick_eval("print \"test\";"), s("test".to_string()));
    assert_eq!(quick_eval("print 5;"), i(5));
    assert_eq!(quick_eval("print 5.0;"), f(5.0));
}

#[test]
fn declare() {
    assert_eq!(quick_eval("a -> 2;"), Obj::Null);
    assert_eq!(quick_eval("b-> 5;"), Obj::Null);
}
