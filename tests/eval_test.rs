extern crate r6;
extern crate env_logger;

use std::sync::{Once, ONCE_INIT};
use r6::runtime::Runtime;
use r6::compiler::Compiler;
use r6::base::libbase;
use r6::parser::Parser;

static START: Once = ONCE_INIT;

macro_rules! assert_evaluates_to {
    ($src:expr, $expected:expr) => (
        {
            START.call_once(|| {
                env_logger::init().unwrap();
            });
            let mut src_parser = Parser::new($src.as_bytes());
            let sourcecode = match src_parser.parse_datum() {
                Ok(code) => code,
                Err(e) => panic!("failed to parse {}: {:?}", $src, e)
            };

            let mut res_parser = Parser::new($expected.as_bytes());
            let expected = match res_parser.parse_datum() {
                Ok(val) => val,
                Err(e) => panic!("failed to parse {}: {:?}", $expected, e)
            };

            let base = libbase();
            let compiler = Compiler::new(&base);
            let bytecode = match compiler.compile(&sourcecode) {
                Ok(code) => code,
                Err(e) => panic!("Failed to compile {:?}: {:?}", &sourcecode, e)
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
            let mut src_parser = Parser::new($src.as_bytes());
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
fn local_define_test() {
    assert_evaluates_to!("(let ((x 5)) (define (foo y) (bar x y)) (define (bar a b) (+ (* a b) a)) (foo (+ x 3)))", "45");
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
fn quasiquote_test() {
    assert_evaluates_to!("`(0 1 2)", "(0 1 2)");
    assert_evaluates_to!("`(0 ,(+ 1 2) 4)", "(0 3 4)");
    assert_evaluates_to!("`(0 ,@(list 1 2) 4)", "(0 1 2 4)");
    assert_evaluates_to!("`(1 `,(+ 1 ,(+ 2 3)) 4)", "(1 `,(+ 1 5) 4)");
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

    assert_evaluates_to!(r#"(vector? #(a b c))"#, "#t");
    assert_evaluates_to!(r#"(vector? #(a #(b) c))"#, "#t");
    assert_evaluates_to!(r#"(vector? (list 1 2))"#, "#f");
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

#[test]
fn number_type_test() {
    assert_evaluates_to!("(complex? 3+4i)", "#t");
    assert_evaluates_to!("(complex? 3)", "#t");
    assert_evaluates_to!("(real? 3)", "#t");
    assert_evaluates_to!("(real? -2.5+0.0i)", "#f");
    assert_evaluates_to!("(real? -2.5+0i)", "#t");
    assert_evaluates_to!("(real? -2.5)", "#t");
    assert_evaluates_to!("(real? #e1e10)", "#t");
    assert_evaluates_to!("(rational? 6/10)", "#t");
    assert_evaluates_to!("(rational? 6/3)", "#t");
    assert_evaluates_to!("(rational? 2)", "#t");
    assert_evaluates_to!("(integer? 3+0i)", "#t");
    assert_evaluates_to!("(integer? 3.0)", "#t");
    assert_evaluates_to!("(integer? 8/4)", "#t");
    assert_evaluates_to!("(number? +nan.0)", "#t");
    assert_evaluates_to!("(complex? +nan.0)", "#t");
    assert_evaluates_to!("(real? +nan.0)", "#t");
    assert_evaluates_to!("(rational? +nan.0)", "#f");
    assert_evaluates_to!("(complex? +inf.0)", "#t");
    assert_evaluates_to!("(real? -inf.0)", "#t");
    assert_evaluates_to!("(rational? -inf.0)", "#f");
    assert_evaluates_to!("(integer? -inf.0)", "#f");
}

#[test]
fn number_eq_test() {
    assert_evaluates_to!("(= 2 2)", "#t");
    assert_evaluates_to!("(= +inf.0 +inf.0)", "#t");
    assert_evaluates_to!("(= -inf.0 +inf.0)", "#f");
    assert_evaluates_to!("(= -inf.0 -inf.0)", "#t");

    assert_evaluates_to!("(< -inf.0 2 +inf.0)", "#t");
    assert_evaluates_to!("(> +inf.0 2 -inf.0)", "#t");

    assert_evaluates_to!("(= +nan.0 2)", "#f");
    assert_evaluates_to!("(= +nan.0 +nan.0)", "#f");

    assert_evaluates_to!("(< +nan.0 2)", "#f");
    assert_evaluates_to!("(< +nan.0 +nan.0)", "#f");

    assert_evaluates_to!("(> +nan.0 2)", "#f");
    assert_evaluates_to!("(> +nan.0 +nan.0)", "#f");
}

#[test]
fn and_or_test() {
    assert_evaluates_to!("(and (= 2 2) (> 2 1))", "#t");
    assert_evaluates_to!("(and (= 2 2) (< 2 1))", "#f");
    assert_evaluates_to!("(and 1 2 'c '(f g))", "(f g)");
    assert_evaluates_to!("(and)", "#t");

    assert_evaluates_to!("(or (= 2 2) (> 2 1))", "#t");
    assert_evaluates_to!("(or (= 2 2) (< 2 1))", "#t");
    assert_evaluates_to!("(or #f #t #t)", "#t");
    assert_evaluates_to!("(or '(b c) (/ 3 0))", "(b c)");
    assert_evaluates_to!("(or)", "#f");
}

#[test]
fn cond_test() {
    assert_evaluates_to!("(cond ((> 3 2) 'greater) ((< 3 2) 'less))", "greater");
    assert_evaluates_to!("(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))", "equal");
    assert_evaluates_to!("(cond ('(1 2 3) => car) (else #f))", "1");
}

#[test]
fn not_test() {
    assert_evaluates_to!("(not #t)", "#f");
    assert_evaluates_to!("(not 3)", "#f");

    assert_evaluates_to!("(not (list 3))", "#f");
    assert_evaluates_to!("(not #f)", "#t");
    assert_evaluates_to!("(not '())", "#f");
    assert_evaluates_to!("(not (list))", "#f");
    assert_evaluates_to!("(not 'nil)", "#f");
}

#[test]
fn cons_test() {
    assert_evaluates_to!("(cons 'a '())", "(a)");
    assert_evaluates_to!("(cons '(a) '(b c d))", "((a) b c d)");
    assert_evaluates_to!("(cons 'a 3)", "(a . 3)");
}

#[test]
fn append_test() {
    assert_evaluates_to!("(append '(x) '(y))", "(x y)");
    assert_evaluates_to!("(append '(a) '(b c d))", "(a b c d)");
    assert_evaluates_to!("(append '(a (b)) '((c)))", "(a (b) (c))");
    assert_evaluates_to!("(append '(a b) '(c . d))", "(a b c . d)");
    assert_evaluates_to!("(append '() 'a)", "a");
}

#[test]
fn symbol_string_test() {
    assert_evaluates_to!("(symbol->string 'a)", "\"a\"");
}

#[test]
fn apply_test() {
    assert_evaluates_to!("(apply + '(1 2 3))", "6");
    assert_evaluates_to!("(apply + (list 3 4))", "7");
    assert_evaluates_to!("(apply + 1 2 '(3))", "6");
}
