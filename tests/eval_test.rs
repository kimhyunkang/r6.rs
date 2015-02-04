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

macro_rules! assert_evaluates_datum {
    ($src:expr, $expected:expr) => (
        {
            let mut src_reader = BufReader::new($src.as_bytes());
            let mut src_parser = Parser::new(&mut src_reader);
            let sourcecode = match src_parser.parse_datum() {
                Ok(code) => code,
                Err(e) => panic!("failed to parse source: {:?}", e)
            };

            let base = libbase();
            let compiler = Compiler::new(&base);
            let bytecode = match compiler.compile(&sourcecode) {
                Ok(code) => code,
                Err(e) => panic!("compile failure: {:?}", e)
            };
            let mut runtime = Runtime::new(bytecode);
            let result = runtime.run();
            if !((result == $expected) && ($expected == result)) {
                panic!("test failed: expected `{:?}` but got `{:?}`", $expected, result);
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
fn lambda_test() {
    assert_evaluates_to!("((lambda (x) (+ x x)) 4)", "8");
}

#[test]
fn var_arg_test() {
    assert_evaluates_to!("((lambda (x y . z) z) 3 4 5 6)", "(5 6)");
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
    assert_evaluates_to!("(/ 3 2)", "3/2");
    assert_evaluates_to!("(/ 3.0 2)", "1.5");
}

#[test]
fn let_test() {
    assert_evaluates_to!("(let ((x 23) (y 42)) (+ x y))", "65");
}

#[test]
fn override_syntax_test() {
    assert_evaluates_to!("(let ((if (lambda (x) x))) (if 3))", "3");
}

#[test]
fn let_star_test() {
    assert_evaluates_to!("(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))", "70");
}

#[test]
fn letrec_test() {
    assert_evaluates_to!("(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (lambda (n) (if (zero? n) #f (even? (- n 1)))))) (even? 8))", "#t");
}

#[test]
fn letrec_undefined_test() {
    use r6::datum::Datum;
    use r6::runtime::RuntimeData;

    assert_evaluates_datum!(
        "(letrec ((quicksand quicksand)) quicksand)",
        Datum::Ext(RuntimeData::Undefined)
    );
}

#[test]
fn set_test() {
    assert_evaluates_to!("(let ((x 23)) (set! x 24) x)", "24");
}

#[test]
fn list_test() {
    assert_evaluates_to!("(list 1 2 3)", "(1 2 3)");
    assert_evaluates_to!("(list)", "()");
}

#[test]
fn quote_test() {
    assert_evaluates_to!(r#"(quote a)"#, "a");
    assert_evaluates_to!(r#"(quote (1 2 3))"#, "(1 2 3)");
    assert_evaluates_to!(r#"(quote #\a)"#, r#"#\a"#);
}

#[test]
fn quote_abbrev_test() {
    assert_evaluates_to!(r#"'a"#, "a");
    assert_evaluates_to!(r#"'(1 2 3)"#, "(1 2 3)");
    assert_evaluates_to!(r#"'#\a"#, r#"#\a"#);
}

#[test]
fn typecheck_test() {
    assert_evaluates_to!("(boolean? #t)", "#t");
    assert_evaluates_to!("(boolean? #f)", "#t");
    assert_evaluates_to!("(boolean? 1)", "#f");

    assert_evaluates_to!("(pair? (list 1 2))", "#t");
    assert_evaluates_to!("(pair? (list))", "#f");
    assert_evaluates_to!("(pair? 1)", "#f");

    assert_evaluates_to!(r#"(symbol? #\a)"#, "#f");
    assert_evaluates_to!(r#"(symbol? 'a)"#, "#t");

    assert_evaluates_to!(r#"(char? #\a)"#, "#t");
    assert_evaluates_to!(r#"(char? 'a)"#, "#f");

    assert_evaluates_to!(r#"(string? "a")"#, "#t");
    assert_evaluates_to!(r#"(string? 'a)"#, "#f");

    assert_evaluates_to!(r#"(procedure? +)"#, "#t");
    assert_evaluates_to!(r#"(procedure? '+)"#, "#f");

    assert_evaluates_to!(r#"(null? (list))"#, "#t");
    assert_evaluates_to!(r#"(null? (list 1 2))"#, "#f");
}

#[test]
fn car_test() {
    assert_evaluates_to!("(car '(a b))", "a");
}

#[test]
fn cdr_test() {
    assert_evaluates_to!("(cdr '(a b))", "(b)");
}

#[test]
fn number_predicates_test() {
    assert_evaluates_to!("(zero? 0)", "#t");
    assert_evaluates_to!("(zero? +0.0)", "#t");
    assert_evaluates_to!("(zero? -0.0)", "#t");
    assert_evaluates_to!("(zero? +nan.0)", "#f");
}
