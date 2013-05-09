extern mod r5;

use r5::runtime::{Runtime};
use r5::parser::Parser;

fn eval_test(src: ~str, expected_src: ~str) {
    let expr =
    do io::with_str_reader(src) |rdr| {
        let mut parser = Parser(rdr);
        @result::unwrap(parser.parse_datum())
    };

    let expected =
    do io::with_str_reader(expected_src) |rdr| {
        let mut parser = Parser(rdr);
        @result::unwrap(parser.parse_datum())
    };

    let mut runtime = Runtime::new_std();

    let val = result::unwrap(runtime.eval(expr));

    assert_eq!(val, expected);
}

#[test]
fn add_test() {
    eval_test(~"(+ 1 2)", ~"3");
}
