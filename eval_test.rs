extern mod r5;

use std::io;
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

    match val {
        Ok(v) => if v != expected {
            fail!(fmt!("expected %s, got %s", expected.to_str(), v.to_str()))
        },
        Err(e) => 
            fail!(e.to_str()),
    }
}

#[test]
fn add_test() {
    eval_test(~"(+ 1 2)", ~"3");
}

#[test]
fn sub_test() {
    eval_test(~"(- 3 1)", ~"2");
    eval_test(~"(- 5 2 1)", ~"2");
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
fn quote_macro_test() {
    eval_test(~"(quote (a b))", ~"(a b)");
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

#[test]
fn lambda_test() {
    eval_test(~"((lambda (x y) (+ x y)) 1 2)", ~"3");
}

#[test]
fn quasiquote_test() {
    eval_test(~"`(list ,(+ 1 2) 4)", ~"(list 3 4)");
}

#[test]
fn nested_quasiquote_test() {
    eval_test(~"`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)", ~"(a `(b ,(+ 1 2) ,(foo 4 d) e) f)");
}

#[test]
fn lexical_scoping_test() {
    /*
    (\y f -> f(2)) dyn ((\y -> (\x -> y)) lex)
    lexical scoping returns lex
    dynamic scoping returns dyn
    */

    let src = ~"((lambda (y f) (f 2)) 'dyn ((lambda (y) (lambda (x) y)) 'lex))";
    eval_test(src, ~"lex");
}

#[test]
fn eqv_test() {
    eval_test(~"(eqv? 3 (+ 1 2))", ~"#t");
    eval_test(~"(eqv? #f #f)", ~"#t");
    eval_test(~"(eqv? #\\a #\\a)", ~"#t");
    eval_test(~"(eqv? 'ident 'ident)", ~"#t");
    eval_test(~"(eqv? 3.0 3)", ~"#f");
    eval_test(~"(eqv? '() '())", ~"#t");
    eval_test(~"(eqv? '(1 2) '(1 2))", ~"#f");
    eval_test(~"(eqv? \"abc\" \"abc\")", ~"#f");
    eval_test(~"((lambda (x) (eqv? x x)) '(1 2))", ~"#t");
}

#[test]
fn set_test() {
    eval_test(~"((lambda (x) (set! x 'mut) x) 'immut)", ~"mut");
}

#[test]
fn varargs_test() {
    eval_test(~"((lambda (a b . c) `(,(+ a b) . ,c)) 1 2 3 4)", ~"(3 3 4)");
}

#[test]
fn define_val_test() {
    eval_test(~"((lambda () (define a 2) (+ a 1)))", ~"3");
}

#[test]
fn define_fun_test() {
    eval_test(~"((lambda () (define (list . a) a) (list 1 2)))", ~"(1 2)");
}

#[test]
fn begin_test() {
    eval_test(~"(begin (+ 1 2) (+ 2 3))", ~"5");
}

#[test]
fn num_fun_test() {
    eval_test(~"(number? 3)", ~"#t");
    eval_test(~"(number? 3+4i)", ~"#t");
    eval_test(~"(real? 3)", ~"#t");
    eval_test(~"(real? -2.5+0.0i)", ~"#t");
    eval_test(~"(real? #e1e10)", ~"#t");
    eval_test(~"(integer? 3+0i)", ~"#t");
    eval_test(~"(integer? 3.0)", ~"#t");
    eval_test(~"(integer? 8/4)", ~"#t");
}

#[test]
fn num_cmp_test() {
    eval_test(~"(< 1 2 3)", ~"#t");
    eval_test(~"(< 1 2 2)", ~"#f");
    eval_test(~"(<= 1.0 2.0 3.0)", ~"#t");
    eval_test(~"(<= 1.0 2.0 2.0)", ~"#t");
    eval_test(~"(= #e2 #e4/2 #e2.0)", ~"#t");
    eval_test(~"(> 1 2 3)", ~"#f");
}

#[test]
fn null_test() {
    eval_test(~"(null? '())", ~"#t");
    eval_test(~"(null? '(a b c))", ~"#f");
    eval_test(~"(null? '(a . c))", ~"#f");
}

#[test]
fn pair_test() {
    eval_test(~"(pair? '())", ~"#f");
    eval_test(~"(pair? '(a b c))", ~"#t");
    eval_test(~"(pair? '(a . c))", ~"#t");
}

#[test]
fn and_test() {
    eval_test(~"(and #t #t)", ~"#t");
    eval_test(~"(and #t #f #t)", ~"#f");
    eval_test(~"(and #t #f a)", ~"#f");
    eval_test(~"(and)", ~"#t");
}

#[test]
fn or_test() {
    eval_test(~"(or #f #f)", ~"#f");
    eval_test(~"(or #f #t #f)", ~"#t");
    eval_test(~"(or #f #t a)", ~"#t");
    eval_test(~"(or)", ~"#f");
}

#[test]
fn recursive_test() {
    eval_test(~"(begin (define (f n) (if (> n 0) (* n (f (- n 1))) 1)) (f 4))", ~"24");
}

#[test]
fn let_test() {
    eval_test(~"(let ((x 1) (y 2)) (+ x y))", ~"3");
}

#[test]
fn not_test() {
    eval_test(~"(not #t)", ~"#f");
    eval_test(~"(not #f)", ~"#t");
}

#[test]
fn modrem_test() {
    eval_test(~"(modulo 13 4)", ~"1");
    eval_test(~"(remainder 13 4)", ~"1");
    eval_test(~"(modulo -13 4)", ~"3");
    eval_test(~"(remainder -13 4)", ~"-1");
    eval_test(~"(modulo 13 -4)", ~"-3");
    eval_test(~"(remainder 13 -4)", ~"1");
    eval_test(~"(modulo -13 -4)", ~"-1");
    eval_test(~"(remainder -13 -4)", ~"-1");
}

#[test]
fn string_test() {
    eval_test(~"(string #\\h #\\e #\\l #\\l #\\o)", ~"\"hello\"");
}

#[test]
fn string_length_test() {
    eval_test(~"(string-length \"hello\")", ~"5");
}
