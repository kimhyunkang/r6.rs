extern mod r5;

use std::io;
use r5::runtime::{Runtime};
use r5::parser::Parser;

macro_rules! eval_test (
    ($src:expr, $expected_src:expr) => ({
        let expr =
        do io::with_str_reader($src) |rdr| {
            let mut parser = Parser(rdr);
            match parser.parse_datum() {
                Ok(v) => @v,
                Err(e) => fail!(e),
            }
        };

        let expected =
        do io::with_str_reader($expected_src) |rdr| {
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
    })
)

#[test]
fn add_test() {
    eval_test!("(+ 1 2)", "3");
}

#[test]
fn sub_test() {
    eval_test!("(- 3)", "-3");
    eval_test!("(- 3 1)", "2");
    eval_test!("(- 5 2 1)", "2");
}

#[test]
fn mul_test() {
    eval_test!("(* 1 2 3)", "6");
}

#[test]
fn div_test() {
    eval_test!("(/ 3)", "1/3");
    eval_test!("(/ 3 1)", "3");
    eval_test!("(/ 5 2 1)", "5/2");
}

#[test]
fn quote_test() {
    eval_test!("'(a b)", "(a b)");
}

#[test]
fn quote_macro_test() {
    eval_test!("(quote (a b))", "(a b)");
}

#[test]
fn car_test() {
    eval_test!("(car '(a b))", "a");
}

#[test]
fn cdr_test() {
    eval_test!("(cdr '(a b))", "(b)");
}

#[test]
fn if_true_test() {
    eval_test!("(if #t 1 0)", "1");
}

#[test]
fn if_false_test() {
    eval_test!("(if #f 1 0)", "0");
    eval_test!("(if '() 1 0)", "1");
}

#[test]
fn lambda_test() {
    eval_test!("((lambda (x y) (+ x y)) 1 2)", "3");
}

#[test]
fn quasiquote_test() {
    eval_test!("`(list ,(+ 1 2) 4)", "(list 3 4)");
}

#[test]
fn vector_quasiquote_test() {
    eval_test!("`#(,(+ 1 2) 4)", "#(3 4)");
}

#[test]
fn nested_quasiquote_test() {
    eval_test!("`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)", "(a `(b ,(+ 1 2) ,(foo 4 d) e) f)");
}

#[test]
fn lexical_scoping_test() {
    /*
    (\y f -> f(2)) dyn ((\y -> (\x -> y)) lex)
    lexical scoping returns lex
    dynamic scoping returns dyn
    */

    let src = "((lambda (y f) (f 2)) 'dyn ((lambda (y) (lambda (x) y)) 'lex))";
    eval_test!(src, "lex");
}

#[test]
fn eq_test() {
    eval_test!("(eq? 'a 'a)", "#t")
    eval_test!("(eq? '(a) '(a))", "#f")
    eval_test!("(eq? '() '())", "#t")
    eval_test!("(eq? car car)", "#t")
    eval_test!("(let ((x '(a))) (eq? x x))", "#t");
    eval_test!("(let ((x '#())) (eq? x x))", "#t");
    eval_test!("(let ((p (lambda (x) x))) (eq? p p))", "#t");
}

#[test]
fn eqv_test() {
    eval_test!("(eqv? 'a 'a)", "#t");
    eval_test!("(eqv? 'a 'b)", "#f");
    eval_test!("(eqv? 2 2)", "#t");
    eval_test!("(eqv? (cons 1 2) (cons 1 2))", "#f");
    eval_test!("(eqv? (lambda () 1) (lambda () 2))", "#f");
    eval_test!("(eqv? 3 (+ 1 2))", "#t");
    eval_test!("(eqv? #f #f)", "#t");
    eval_test!("(eqv? #\\a #\\a)", "#t");
    eval_test!("(eqv? 'ident 'ident)", "#t");
    eval_test!("(eqv? 3.0 3)", "#f");
    eval_test!("(eqv? '() '())", "#t");
    eval_test!("(eqv? '(1 2) '(1 2))", "#f");
    eval_test!("(eqv? \"abc\" \"abc\")", "#f");
    eval_test!("((lambda (x) (eqv? x x)) '(1 2))", "#t");
}

#[test]
fn set_test() {
    eval_test!("((lambda (x) (set! x 'mut) x) 'immut)", "mut");
}

#[test]
fn varargs_test() {
    eval_test!("((lambda (a b . c) `(,(+ a b) . ,c)) 1 2 3 4)", "(3 3 4)");
}

#[test]
fn define_val_test() {
    eval_test!("((lambda () (define a 2) (+ a 1)))", "3");
}

#[test]
fn define_fun_test() {
    eval_test!("((lambda () (define (list . a) a) (list 1 2)))", "(1 2)");
}

#[test]
fn begin_test() {
    eval_test!("(begin (+ 1 2) (+ 2 3))", "5");
}

#[test]
fn num_fun_test() {
    eval_test!("(number? 3)", "#t");
    eval_test!("(number? 3+4i)", "#t");
    eval_test!("(real? 3)", "#t");
    eval_test!("(real? -2.5+0.0i)", "#f");
    eval_test!("(real? #e1e10)", "#t");
    eval_test!("(integer? 3+0i)", "#t");
    eval_test!("(integer? 3.0)", "#f");
    eval_test!("(integer? 8/4)", "#t");
}

#[test]
fn num_cmp_test() {
    eval_test!("(< 1 2 3)", "#t");
    eval_test!("(< 1 2 2)", "#f");
    eval_test!("(<= 1.0 2.0 3.0)", "#t");
    eval_test!("(<= 1.0 2.0 2.0)", "#t");
    eval_test!("(= #e2 #e4/2 #e2.0)", "#t");
    eval_test!("(> 1 2 3)", "#f");
}

#[test]
fn null_test() {
    eval_test!("(null? '())", "#t");
    eval_test!("(null? '(a b c))", "#f");
    eval_test!("(null? '(a . c))", "#f");
}

#[test]
fn pair_test() {
    eval_test!("(pair? '())", "#f");
    eval_test!("(pair? '(a b c))", "#t");
    eval_test!("(pair? '(a . c))", "#t");
}

#[test]
fn and_test() {
    eval_test!("(and #t #t)", "#t");
    eval_test!("(and #t #f #t)", "#f");
    eval_test!("(and #t #f a)", "#f");
    eval_test!("(and 'a #t #t)", "#t");
    eval_test!("(and #t #t 'a)", "a");
    eval_test!("(and)", "#t");
}

#[test]
fn or_test() {
    eval_test!("(or #f #f)", "#f");
    eval_test!("(or #f #t #f)", "#t");
    eval_test!("(or #f #t a)", "#t");
    eval_test!("(or #f #f 'a)", "a");
    eval_test!("(or)", "#f");
}

#[test]
fn recursive_test() {
    eval_test!("(begin (define (f n) (if (> n 0) (* n (f (- n 1))) 1)) (f 4))", "24");
}

#[test]
fn let_test() {
    eval_test!("(let ((x 1) (y 2)) (+ x y))", "3");
}

#[test]
fn let_loop_test() {
    eval_test!("(let loop ((f 1) (n 4)) (if (> n 0) (loop (* f n) (- n 1)) f))", "24");
}

#[test]
fn not_test() {
    eval_test!("(not #t)", "#f");
    eval_test!("(not #f)", "#t");
}

#[test]
fn modrem_test() {
    eval_test!("(modulo 13 4)", "1");
    eval_test!("(remainder 13 4)", "1");
    eval_test!("(modulo -13 4)", "3");
    eval_test!("(remainder -13 4)", "-1");
    eval_test!("(modulo 13 -4)", "-3");
    eval_test!("(remainder 13 -4)", "1");
    eval_test!("(modulo -13 -4)", "-1");
    eval_test!("(remainder -13 -4)", "-1");
}

#[test]
fn floor_test() {
    eval_test!("(floor -4.3)", "-5.0");
    eval_test!("(floor 3.5)", "3.0");
    eval_test!("(floor -4.0)", "-4.0");
    eval_test!("(floor 3.0)", "3.0");
    eval_test!("(floor 9/2)", "4");
    eval_test!("(floor -7/2)", "-4");
    eval_test!("(floor 9/3)", "3");
    eval_test!("(floor -8/2)", "-4");
}

#[test]
fn ceiling_test() {
    eval_test!("(ceiling -4.3)", "-4.0");
    eval_test!("(ceiling 3.5)", "4.0");
    eval_test!("(ceiling -4.0)", "-4.0");
    eval_test!("(ceiling 3.0)", "3.0");
    eval_test!("(ceiling 9/2)", "5");
    eval_test!("(ceiling -7/2)", "-3");
    eval_test!("(ceiling 9/3)", "3");
    eval_test!("(ceiling -8/2)", "-4");
}

#[test]
fn round_test() {
    eval_test!("(round -4.3)", "-4.0");
    eval_test!("(round 3.5)", "4.0");
    eval_test!("(round 7/2)", "4");
    eval_test!("(round 7)", "7");
}

#[test]
fn string_test() {
    eval_test!("(string #\\h #\\e #\\l #\\l #\\o)", "\"hello\"");
    eval_test!("(list->string '(#\\h #\\e #\\l #\\l #\\o))", "\"hello\"");
    eval_test!("(string->list \"hello\")", "(#\\h #\\e #\\l #\\l #\\o)");
}

#[test]
fn string_length_test() {
    eval_test!("(string-length \"hello\")", "5");
}

#[test]
fn string_ref_test() {
    eval_test!("(string-ref \"hello\" 4)", "#\\o");
}

#[test]
fn substring_test() {
    eval_test!("(substring \"hello\" 0 4)", "\"hell\"");
}

#[test]
fn letrec_test() {
    eval_test!("(letrec ((fib (lambda (n) (if (= n 0) 1 (* n (fib (- n 1))))))) (fib 4))", "24");
}

#[test]
fn letstar_test() {
    eval_test!("(let* ((x 3) (y (+ x 2))) (+ y 1))", "6");
}

#[test]
fn cond_test() {
    eval_test!("(cond ((< 3 3) 'less) ((= 3 3) 'equal) (else 'greater))", "equal");
    eval_test!("(cond ('() 1) (else 0))", "1");
}

#[test]
fn exp_log_test() {
    eval_test!("(log (exp 1-2i))", "1.0-2.0i");
}

#[test]
fn make_complex_test() {
    eval_test!("(make-rectangular 1 -2)", "1-2i");
    eval_test!("(make-polar 1 0)", "1.0+0.0i");
}

#[test]
fn exact_inexact_test() {
    eval_test!("(exact->inexact 1+2i)", "1.0+2.0i");
    eval_test!("(inexact->exact 2.0+0.5i)", "2+1/2i");
}

#[test]
fn number_to_str_test() {
    eval_test!("(number->string 1+2i)", "\"1+2i\"");
    eval_test!("(number->string 5+6i 2)", "\"101+110i\"");
}

#[test]
fn boolean_test() {
    eval_test!("(boolean? #t)", "#t");
    eval_test!("(boolean? #f)", "#t");
    eval_test!("(boolean? '())", "#f");
}

#[test]
fn procedure_test() {
    eval_test!("(procedure? (lambda (x) x))", "#t");
    eval_test!("(procedure? eval)", "#t");
    eval_test!("(procedure? '())", "#f");
}

#[test]
fn is_vector_test() {
    eval_test!("(vector? #(1 (2) 3))", "#t");
    eval_test!("(vector? #())", "#t");
    eval_test!("(vector? 1)", "#f");
}

#[test]
fn make_vector_test() {
    eval_test!("(make-vector 2 'a)", "#(a a)");
}

#[test]
fn vector_test() {
    eval_test!("(vector 2 'a)", "#(2 a)");
    eval_test!("(vector-length #(2 a))", "2");
    eval_test!("(vector-ref #(2 a) 1)", "a");
}

#[test]
fn vector_conversion_test() {
    eval_test!("(vector->list #(2 a))", "(2 a)");
    eval_test!("(list->vector '(2 a))", "#(2 a)");
}

#[test]
fn vector_set_test() {
    let s = "(let ((vec (vector 0 '(2 2 2 2) \"Anna\")))\
                (vector-set! vec 1 '(\"Sue\" \"Sue\"))\
                vec)";
    let expected = "#(0 (\"Sue\" \"Sue\") \"Anna\")";
    eval_test!(s, expected)
}

#[test]
fn vector_fill_test() {
    let s = "(let ((vec (make-vector 3)))\
                (vector-fill! vec 'a)\
                vec)";
    let expected = "#(a a a)";
    eval_test!(s, expected)
}

#[test]
fn symbol_conversion_test() {
    eval_test!("(symbol->string 'str)", "\"str\"");
    eval_test!("(string->symbol \"str\")", "str");
}

#[test]
fn char_test() {
    eval_test!("(char? #\\space)", "#t");
    eval_test!("(char? \" \")", "#f");
    eval_test!("(char-alphabetic? #\\A)", "#t");
    eval_test!("(char-alphabetic? #\\a)", "#t");
    eval_test!("(char-alphabetic? #\\1)", "#f");
    eval_test!("(char-alphabetic? #\\space)", "#f");
    eval_test!("(char-numeric? #\\A)", "#f");
    eval_test!("(char-numeric? #\\a)", "#f");
    eval_test!("(char-numeric? #\\1)", "#t");
    eval_test!("(char-numeric? #\\space)", "#f");
    eval_test!("(char-whitespace? #\\A)", "#f");
    eval_test!("(char-whitespace? #\\a)", "#f");
    eval_test!("(char-whitespace? #\\1)", "#f");
    eval_test!("(char-whitespace? #\\space)", "#t");
    eval_test!("(char-upper-case? #\\A)", "#t");
    eval_test!("(char-upper-case? #\\a)", "#f");
    eval_test!("(char-lower-case? #\\A)", "#f");
    eval_test!("(char-lower-case? #\\a)", "#t");
}

#[test]
fn char_cmp_test() {
    eval_test!("(char=? #\\1 #\\1)", "#t");
    eval_test!("(char<? #\\0 #\\3)", "#t");
    eval_test!("(char<? #\\A #\\B)", "#t");
    eval_test!("(char<? #\\4 #\\3)", "#f");
    eval_test!("(char<? #\\D #\\B)", "#f");
    eval_test!("(char>? #\\0 #\\3)", "#f");
    eval_test!("(char>? #\\A #\\B)", "#f");
    eval_test!("(char>? #\\4 #\\3)", "#t");
    eval_test!("(char>? #\\D #\\B)", "#t");
    eval_test!("(char<=? #\\0 #\\3)", "#t");
    eval_test!("(char<=? #\\A #\\B)", "#t");
    eval_test!("(char<=? #\\4 #\\3)", "#f");
    eval_test!("(char<=? #\\D #\\B)", "#f");
    eval_test!("(char>=? #\\0 #\\3)", "#f");
    eval_test!("(char>=? #\\A #\\B)", "#f");
    eval_test!("(char>=? #\\4 #\\3)", "#t");
    eval_test!("(char>=? #\\D #\\B)", "#t");
}

#[test]
fn char_ci_cmp_test() {
    eval_test!("(char-ci=? #\\a #\\A)", "#t");
    eval_test!("(char-ci<? #\\a #\\B)", "#t");
    eval_test!("(char-ci<? #\\d #\\B)", "#f");
    eval_test!("(char-ci>? #\\a #\\B)", "#f");
    eval_test!("(char-ci>? #\\d #\\B)", "#t");
    eval_test!("(char-ci<=? #\\a #\\B)", "#t");
    eval_test!("(char-ci<=? #\\d #\\B)", "#f");
    eval_test!("(char-ci>=? #\\a #\\B)", "#f");
    eval_test!("(char-ci>=? #\\d #\\B)", "#t");
}

#[test]
fn string_append_test() {
    eval_test!("(string-append \"r5\" \".\" \"rs\")", "\"r5.rs\"");
}

#[test]
fn expt_test() {
    eval_test!("(expt 3 2)", "9");
    eval_test!("(expt 3.0 2)", "9.0");
    eval_test!("(expt 1/2 -2)", "4");
    eval_test!("(expt 0.5 -2)", "4.0");
}

#[test]
fn map_test() {
    eval_test!("(map + '(1 2 3) '(2 3 4))", "(3 5 7)");
}

#[test]
fn min_max_test() {
    eval_test!("(max 3 4)", "4");
    eval_test!("(max 3.9 4)", "4.0");
    eval_test!("(min 3 4)", "3");
    eval_test!("(min 3.9 4)", "3.9");
}

#[test]
fn abs_test() {
    eval_test!("(abs 3/4)", "3/4");
    eval_test!("(abs -3)", "3");
    eval_test!("(abs 3+4i)", "5.0");
}

#[test]
fn gcd_test() {
    eval_test!("(gcd 32 -36)", "4");
    eval_test!("(gcd)", "0");
    eval_test!("(lcm 32 -36)", "288");
    eval_test!("(lcm)", "1");
}

#[test]
fn sqrt_test() {
    eval_test!("(sqrt 16)", "4.0");
    eval_test!("(sqrt -16)", "0.0+4.0i");
}

#[test]
fn case_test() {
    eval_test!("(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))", "composite")
}

#[test]
fn do_test() {
    let s = "(do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))";
    eval_test!(s, "#(0 1 2 3 4)")
}

#[test]
fn do_test2() {
    let s = "(let ((x '(1 3 5 7 9)))\
                (do ((x x (cdr x))\
                    (sum 0 (+ sum (car x))))\
                    ((null? x) sum)))";
    eval_test!(s, "25")
}
