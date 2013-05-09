extern mod r5;

use r5::runtime::{Runtime};
use r5::parser::Parser;

fn eval_test(src: ~str, expected_src: ~str) {
    let expr =
    do io::with_str_reader(src) |rdr| {
        let mut parser = Parser(rdr);
        match parser.parse_datum() {
            Ok(v) => @v,
            Err(e) => fail!(e),
        }
    };

    let expected =
    do io::with_str_reader(expected_src) |rdr| {
        let mut parser = Parser(rdr);
        match parser.parse_datum() {
            Ok(v) => @v,
            Err(e) => fail!(e),
        }
    };

    let mut runtime = Runtime::new_std();

    let val = runtime.eval(expr);

    assert_eq!(val, Ok(expected));
}

#[test]
fn add_test() {
    eval_test(~"(+ 1 2)", ~"3");
}

#[test]
fn mul_test() {
    eval_test(~"(* 1 2 3)", ~"6");
}

#[test]
fn quote_test() {
    eval_test(~"'(a b)", ~"(a b)");
}

#[test]
fn car_test() {
    eval_test(~"(car '(a b))", ~"a");
}

#[test]
fn cdr_test() {
    eval_test(~"(cdr '(a b))", ~"(b)");
}

#[test]
fn if_true_test() {
    eval_test(~"(if #t 1 0)", ~"1");
}

#[test]
fn if_false_test() {
    eval_test(~"(if #f 1 0)", ~"0");
}
