use lumi::{quick_eval, Obj};

extern crate lumi;

fn n(n: f64) -> Obj {
    Obj::f64(n)
}

fn b(x: bool) -> Obj {
    Obj::Bool(x)
}

#[test]
fn operators() {
    assert_eq!(quick_eval("2 + 2"), n(4.0));
    assert_eq!(quick_eval("4 - 2"), n(2.0));
    assert_eq!(quick_eval("3 * 4"), n(12.0));
    assert_eq!(quick_eval("15 / 3"), n(5.0));
}

#[test]
fn or() {
    assert_eq!(quick_eval("2 == 2 or \"test\" == 3"), b(true));
    assert_eq!(quick_eval("2 == 3 or \"test\" == \"test\""), b(true));
    assert_eq!(quick_eval("2 == 3 or \"test\" == \"abc\""), b(false));
}
