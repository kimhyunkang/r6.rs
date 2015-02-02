extern crate r6;

use std::old_io::BufReader;
use r6::runtime::Runtime;
use r6::compiler::Compiler;
use r6::base::libbase;
use r6::parser::Parser;

macro_rules! assert_evaluates_to {
    ($src:expr, $expected:expr) => (
        {
            let mut src_reader = BufReader::new($src.as_bytes());
            let mut src_parser = Parser::new(&mut src_reader);
            let sourcecode = match src_parser.parse_datum() {
                Ok(code) => code,
                Err(e) => panic!("failed to parse source: {:?}", e)
            };

            let mut res_reader = BufReader::new($expected.as_bytes());
            let mut res_parser = Parser::new(&mut res_reader);
            let expected = match res_parser.parse_datum() {
                Ok(val) => val,
                Err(e) => panic!("failed to parse result: {:?}", e)
            };

            let base = libbase();
            let compiler = Compiler::new(&base);
            let bytecode = match compiler.compile(&sourcecode) {
                Ok(code) => code,
                Err(e) => panic!("compile failure: {:?}", e)
            };
            let mut runtime = Runtime::new(bytecode);
            let result = runtime.run();
            if !((result == expected) && (expected == result)) {
                panic!("test failed: expected `{:?}` but got `{:?}`", expected, result);
            }
        }
    )
}

#[test]
fn lexical_scoping() {
    // (\y f -> f 2) #f ((\y -> (\x -> y)) #t)
    // If it's dynamic scope, it should return 0
    // If it's static scope, it should return 1
    assert_evaluates_to!("((lambda (y f) (f 2)) #f ((lambda (y) (lambda (x) y)) #t))", "#t")
}

#[test]
fn if_expression_test() {
    assert_evaluates_to!("(if #t 1 0)", "1");
    assert_evaluates_to!("(if #f 1 0)", "0");
}

#[test]
fn eval_to_self_test() {
    assert_evaluates_to!("#t", "#t");
    assert_evaluates_to!("23", "23");
}

#[test]
fn numeric_expressions_test() {
    assert_evaluates_to!("(+ 23 42)", "65");
    assert_evaluates_to!("(+ 14 (* 23 42))", "980");
    assert_evaluates_to!("(- 3 1)", "2");
    assert_evaluates_to!("(- 3)", "-3");
}

#[test]
fn let_test() {
    assert_evaluates_to!("(let ((x 23) (y 42)) (+ x y))", "65");
}

#[test]
fn set_test() {
    assert_evaluates_to!("(let ((x 23)) (set! x 24) x)", "24");
}
