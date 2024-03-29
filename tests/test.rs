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
    assert_eq!(quick_eval("-3 * -3 + 4"), n(13.0));
    assert_eq!(quick_eval("-3 * -3 + 4"), n(13.0));
    assert_eq!(
        quick_eval("-2 * 3 + 4 / 3 * -1 - 7"),
        n(-14.333333333333334)
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

// TODO:
// test print and declaration
