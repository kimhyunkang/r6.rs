extern crate r6;
extern crate env_logger;

use std::sync::{Once, ONCE_INIT};
use r6::base::{base_syntax, libbase};
use r6::error::{RuntimeError, RuntimeErrorKind};
use r6::parser::Parser;
use r6::runtime::Runtime;

static START: Once = ONCE_INIT;

macro_rules! assert_evaluates_to {
    ( $($src:expr),+ => $expected:expr) => (
        {
            START.call_once(|| {
                env_logger::init().unwrap();
            });

            let mut res_parser = Parser::new($expected.as_bytes());
            let expected = match res_parser.parse_datum() {
                Ok(val) => val,
                Err(e) => panic!("failed to parse {}: {:?}", $expected, e)
            };

            let syntax = base_syntax();
            let base = libbase();
            let mut runtime = Runtime::new(base, syntax);

            let srcs = vec!($($src),+);
            let mut result = Err(RuntimeError {
                kind: RuntimeErrorKind::Panic,
                desc: "Source code not given".to_string()
            });

            for src in srcs.into_iter() {
                let mut src_parser = Parser::new(src.as_bytes());
                let sourcecode = match src_parser.parse_datum::<()>() {
                    Ok(code) => code,
                    Err(e) => panic!("failed to parse {}: {:?}", src, e)
                };
                result = runtime.eval(&sourcecode)
            }

            let datum = result.unwrap();

            if !((datum == expected) && (expected == datum)) {
                panic!("test failed: expected `{:?}` but got `{:?}`", expected, datum);
            }
        }
    )
}

macro_rules! assert_evaluates_datum {
    ($src:expr, $expected:expr) => (
        {
            let mut src_parser = Parser::new($src.as_bytes());
            let sourcecode = match src_parser.parse_datum::<()>() {
                Ok(code) => code,
                Err(e) => panic!("failed to parse source: {:?}", e)
            };

            let syntax = base_syntax();
            let base = libbase();
            let mut runtime = Runtime::new(base, syntax);
            let result = runtime.eval(&sourcecode).unwrap();

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
    assert_evaluates_to!("((lambda (y f) (f 2)) #f ((lambda (y) (lambda (x) y)) #t))" => "#t")
}

#[test]
fn lambda_test() {
    assert_evaluates_to!("((lambda (x) (+ x x)) 4)" => "8");
}

#[test]
fn var_arg_test() {
    assert_evaluates_to!("((lambda (x y . z) z) 3 4 5 6)" => "(5 6)");
}

#[test]
fn local_define_test() {
    assert_evaluates_to!("(let ((x 5)) (define (foo y) (bar x y)) (define (bar a b) (+ (* a b) a)) (foo (+ x 3)))" => "45");
}

#[test]
fn if_expression_test() {
    assert_evaluates_to!("(if #t 1 0)" => "1");
    assert_evaluates_to!("(if #f 1 0)" => "0");

    assert_evaluates_to!("(+ 3 (if #t 1 0))" => "4");
    assert_evaluates_to!("(+ 3 (if #f 1 0))" => "3");
}

#[test]
fn eval_to_self_test() {
    assert_evaluates_to!("#t" => "#t");
    assert_evaluates_to!("23" => "23");
}

#[test]
fn numeric_expressions_test() {
    assert_evaluates_to!("(+ 23 42)" => "65");
    assert_evaluates_to!("(+ 14 (* 23 42))" => "980");
    assert_evaluates_to!("(- 3 1)" => "2");
    assert_evaluates_to!("(- 3)" => "-3");
    assert_evaluates_to!("(/ 3 2)" => "3/2");
    assert_evaluates_to!("(/ 3.0 2)" => "1.5");
}

#[test]
fn let_test() {
    assert_evaluates_to!("(let ((x 23) (y 42)) (+ x y))" => "65");
}

#[test]
fn nested_let_test() {
    assert_evaluates_to!("(let ((x 23)) (let ((y 42)) (+ x y)))" => "65");
}

#[test]
fn let_result_test() {
    assert_evaluates_to!("(+ (let ((x 23)) (let ((y 42)) (+ x y))) 13)" => "78");
}

#[test]
fn override_syntax_test() {
    assert_evaluates_to!("(let ((if (lambda (x) x))) (if 3))" => "3");
}

#[test]
fn let_star_test() {
    assert_evaluates_to!("(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))" => "70");
}

#[test]
fn let_star_value_test() {
    assert_evaluates_to!("(let ((x 2) (y 3))
                                (-
                                    (let* ((x 7) (z (+ x y))) (* z x))
                                    2)
                            )" => "68");
}

#[test]
fn letrec_test() {
    assert_evaluates_to!("(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1))))) (odd? (lambda (n) (if (zero? n) #f (even? (- n 1)))))) (even? 8))" => "#t");
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
fn letrec_value_test() {
    assert_evaluates_to!("
        (+
            (letrec ((fact4 (lambda (n)
                        (if (= n 0)
                            1
                            (* n (fact4 (- n 1)))))))
                (fact4 4))
            3)"
        =>
        "27");
}

#[test]
fn set_test() {
    assert_evaluates_to!("(let ((x 23)) (set! x 24) x)" => "24");
}

#[test]
fn list_test() {
    assert_evaluates_to!("(list 1 2 3)" => "(1 2 3)");
    assert_evaluates_to!("(list)" => "()");
}

#[test]
fn quote_test() {
    assert_evaluates_to!(r#"(quote a)"# => "a");
    assert_evaluates_to!(r#"(quote (1 2 3))"# => "(1 2 3)");
    assert_evaluates_to!(r#"(quote #\a)"# => r#"#\a"#);
}

#[test]
fn quote_abbrev_test() {
    assert_evaluates_to!(r#"'a"# => "a");
    assert_evaluates_to!(r#"'(1 2 3)"# => "(1 2 3)");
    assert_evaluates_to!(r#"'#\a"# => r#"#\a"#);
}

#[test]
fn quasiquote_test() {
    assert_evaluates_to!("`(0 1 2)" => "(0 1 2)");
    assert_evaluates_to!("`(0 ,(+ 1 2) 4)" => "(0 3 4)");
    assert_evaluates_to!("`(0 ,@(list 1 2) 4)" => "(0 1 2 4)");
    assert_evaluates_to!("`(1 `,(+ 1 ,(+ 2 3)) 4)" => "(1 `,(+ 1 5) 4)");
}

#[test]
fn typecheck_test() {
    assert_evaluates_to!("(boolean? #t)" => "#t");
    assert_evaluates_to!("(boolean? #f)" => "#t");
    assert_evaluates_to!("(boolean? 1)" => "#f");

    assert_evaluates_to!("(pair? (list 1 2))" => "#t");
    assert_evaluates_to!("(pair? (list))" => "#f");
    assert_evaluates_to!("(pair? 1)" => "#f");

    assert_evaluates_to!(r#"(symbol? #\a)"# => "#f");
    assert_evaluates_to!(r#"(symbol? 'a)"# => "#t");

    assert_evaluates_to!(r#"(char? #\a)"# => "#t");
    assert_evaluates_to!(r#"(char? 'a)"# => "#f");

    assert_evaluates_to!(r#"(string? "a")"# => "#t");
    assert_evaluates_to!(r#"(string? 'a)"# => "#f");

    assert_evaluates_to!(r#"(procedure? +)"# => "#t");
    assert_evaluates_to!(r#"(procedure? '+)"# => "#f");

    assert_evaluates_to!(r#"(null? (list))"# => "#t");
    assert_evaluates_to!(r#"(null? (list 1 2))"# => "#f");

    assert_evaluates_to!(r#"(vector? #(a b c))"# => "#t");
    assert_evaluates_to!(r#"(vector? #(a #(b) c))"# => "#t");
    assert_evaluates_to!(r#"(vector? (list 1 2))"# => "#f");
}

#[test]
fn make_vector_test() {
    assert_evaluates_to!("(make-vector 5 'a)" => "#(a a a a a)");
    assert_evaluates_to!("(vector? (make-vector 5))" => "#t");
    assert_evaluates_to!("(vector? (make-vector 5 'a))" => "#t");
    assert_evaluates_to!("(vector-ref (make-vector 5 'a) 3)" => "a");
}

#[test]
fn vector_conv_test() {
    assert_evaluates_to!("(vector->list '#(dah dah didah))" => "(dah dah didah)");
    assert_evaluates_to!("(list->vector '(dididit dah))" => "#(dididit dah)");
}

#[test]
fn car_test() {
    assert_evaluates_to!("(car '(a b))" => "a");
}

#[test]
fn cdr_test() {
    assert_evaluates_to!("(cdr '(a b))" => "(b)");
}

#[test]
fn number_predicates_test() {
    assert_evaluates_to!("(zero? 0)" => "#t");
    assert_evaluates_to!("(zero? +0.0)" => "#t");
    assert_evaluates_to!("(zero? -0.0)" => "#t");
    assert_evaluates_to!("(zero? +nan.0)" => "#f");
}

#[test]
fn number_type_test() {
    assert_evaluates_to!("(complex? 3+4i)" => "#t");
    assert_evaluates_to!("(complex? 3)" => "#t");
    assert_evaluates_to!("(real? 3)" => "#t");
    assert_evaluates_to!("(real? -2.5+0.0i)" => "#f");
    assert_evaluates_to!("(real? -2.5+0i)" => "#t");
    assert_evaluates_to!("(real? -2.5)" => "#t");
    assert_evaluates_to!("(real? #e1e10)" => "#t");
    assert_evaluates_to!("(rational? 6/10)" => "#t");
    assert_evaluates_to!("(rational? 6/3)" => "#t");
    assert_evaluates_to!("(rational? 2)" => "#t");
    assert_evaluates_to!("(integer? 3+0i)" => "#t");
    assert_evaluates_to!("(integer? 3.0)" => "#t");
    assert_evaluates_to!("(integer? 8/4)" => "#t");
    assert_evaluates_to!("(number? +nan.0)" => "#t");
    assert_evaluates_to!("(complex? +nan.0)" => "#t");
    assert_evaluates_to!("(real? +nan.0)" => "#t");
    assert_evaluates_to!("(rational? +nan.0)" => "#f");
    assert_evaluates_to!("(complex? +inf.0)" => "#t");
    assert_evaluates_to!("(real? -inf.0)" => "#t");
    assert_evaluates_to!("(rational? -inf.0)" => "#f");
    assert_evaluates_to!("(integer? -inf.0)" => "#f");
}

#[test]
fn number_eq_test() {
    assert_evaluates_to!("(= 2 2)" => "#t");
    assert_evaluates_to!("(= +inf.0 +inf.0)" => "#t");
    assert_evaluates_to!("(= -inf.0 +inf.0)" => "#f");
    assert_evaluates_to!("(= -inf.0 -inf.0)" => "#t");

    assert_evaluates_to!("(< -inf.0 2 +inf.0)" => "#t");
    assert_evaluates_to!("(> +inf.0 2 -inf.0)" => "#t");

    assert_evaluates_to!("(= +nan.0 2)" => "#f");
    assert_evaluates_to!("(= +nan.0 +nan.0)" => "#f");

    assert_evaluates_to!("(< +nan.0 2)" => "#f");
    assert_evaluates_to!("(< +nan.0 +nan.0)" => "#f");

    assert_evaluates_to!("(> +nan.0 2)" => "#f");
    assert_evaluates_to!("(> +nan.0 +nan.0)" => "#f");
}

#[test]
fn and_or_test() {
    assert_evaluates_to!("(and (= 2 2) (> 2 1))" => "#t");
    assert_evaluates_to!("(and (= 2 2) (< 2 1))" => "#f");
    assert_evaluates_to!("(and 1 2 'c '(f g))" => "(f g)");
    assert_evaluates_to!("(and)" => "#t");

    assert_evaluates_to!("(or (= 2 2) (> 2 1))" => "#t");
    assert_evaluates_to!("(or (= 2 2) (< 2 1))" => "#t");
    assert_evaluates_to!("(or #f #t #t)" => "#t");
    assert_evaluates_to!("(or '(b c) (/ 3 0))" => "(b c)");
    assert_evaluates_to!("(or)" => "#f");
}

#[test]
fn cond_test() {
    assert_evaluates_to!("(cond ((> 3 2) 'greater) ((< 3 2) 'less))" => "greater");
    assert_evaluates_to!("(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))" => "equal");
    assert_evaluates_to!("(cond ('(1 2 3) => car) (else #f))" => "1");
}

#[test]
fn case_test() {
    assert_evaluates_to!("(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))" => "composite");
    assert_evaluates_to!("(case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant))" => "consonant");
}

#[test]
fn not_test() {
    assert_evaluates_to!("(not #t)" => "#f");
    assert_evaluates_to!("(not 3)" => "#f");

    assert_evaluates_to!("(not (list 3))" => "#f");
    assert_evaluates_to!("(not #f)" => "#t");
    assert_evaluates_to!("(not '())" => "#f");
    assert_evaluates_to!("(not (list))" => "#f");
    assert_evaluates_to!("(not 'nil)" => "#f");
}

#[test]
fn cons_test() {
    assert_evaluates_to!("(cons 'a '())" => "(a)");
    assert_evaluates_to!("(cons '(a) '(b c d))" => "((a) b c d)");
    assert_evaluates_to!("(cons 'a 3)" => "(a . 3)");
}

#[test]
fn append_test() {
    assert_evaluates_to!("(append '(x) '(y))" => "(x y)");
    assert_evaluates_to!("(append '(a) '(b c d))" => "(a b c d)");
    assert_evaluates_to!("(append '(a (b)) '((c)))" => "(a (b) (c))");
    assert_evaluates_to!("(append '(a b) '(c . d))" => "(a b c . d)");
    assert_evaluates_to!("(append '() 'a)" => "a");
}

#[test]
fn symbol_string_test() {
    assert_evaluates_to!("(symbol->string 'a)" => "\"a\"");
}

#[test]
fn apply_test() {
    assert_evaluates_to!("(apply + '(1 2 3))" => "6");
    assert_evaluates_to!("(apply + (list 3 4))" => "7");
    assert_evaluates_to!("(apply + 1 2 '(3))" => "6");
}

#[test]
fn eqv_test() {
    assert_evaluates_to!("(eqv? 'a 'a)" => "#t");
    assert_evaluates_to!("(eqv? 'a 'b)" => "#f");

    assert_evaluates_to!("(eqv? 2 2)" => "#t");

    assert_evaluates_to!("(eqv? '() '())" => "#t");

    assert_evaluates_to!("(eqv? 100000000 100000000)" => "#t");

    assert_evaluates_to!("(eqv? (cons 1 2) (cons 1 2))" => "#f");

    assert_evaluates_to!("(eqv? (lambda () 1) (lambda () 2))" => "#f");

    assert_evaluates_to!("(eqv? #f 'nil)" => "#f");

    assert_evaluates_to!("(let ((x '(a))) (eqv? x x))" => "#t");

    assert_evaluates_to!("(letrec ((f (lambda () (if (eqv? f g) 'f 'both))) (g (lambda () (if (eqv? f g) 'g 'both)))) (eqv? f g))" => "#f");
}

#[test]
fn equal_test() {
    assert_evaluates_to!("(equal? 'a 'a)" => "#t");
    assert_evaluates_to!("(equal? '(a) '(a))" => "#t");
    assert_evaluates_to!("(equal? '(a (b) c) '(a (b) c))" => "#t");
    assert_evaluates_to!("(equal? \"abc\" \"abc\")" => "#t");
    assert_evaluates_to!("(equal? 2 2)" => "#t");
    assert_evaluates_to!("(equal? (make-vector 5 'a) (make-vector 5 'a))" => "#t");
    assert_evaluates_to!("(let* ((x (list 'a)) (y (list 'a)) (z (list x y))) (list (equal? z (list y x)) (equal? z (list x x))))" => "(#t #t)");
}

#[test]
fn eq_test() {
    assert_evaluates_to!("(eq? 'a 'a)" => "#t");
    assert_evaluates_to!("(eq? (list 'a) (list 'a))" => "#f");
    assert_evaluates_to!("(eq? '() '())" => "#t");
    assert_evaluates_to!("(eq? car car)" => "#t");
    assert_evaluates_to!("(let ((x '(a))) (eq? x x))" => "#t");
}

#[test]
fn first_class_function() {
    assert_evaluates_to!("(let ((h (lambda (op x y) (op x y)))) (h + 23 42))" => "65");
    assert_evaluates_to!("(let ((h (lambda (op x y) (op x y)))) (h * 23 42))" => "966");
}

#[test]
fn test_global_define_lambda() {
    assert_evaluates_to!(
        "(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))",
        "(fact 3)"
        =>
        "6"
    );
}

#[test]
fn test_global_define() {
    assert_evaluates_to!(
        "(define (fact n) (if (zero? n) 1 (* n (fact (- n 1)))))",
        "(fact 3)"
        =>
        "6"
    );
}

#[test]
fn test_selection_sort() {
    assert_evaluates_to!(
        "(define (smallest L A)
            (cond ((null? L) A)
                  ((< (car L) A) (smallest (cdr L) (car L)))
                  (else (smallest (cdr L) A))
            )
         )",
        "(define (remove L A)
            (cond ((null? L) '())
                  ((= (car L) A) (cdr L))
                  (else (cons (car L) (remove (cdr L) A)))
            )
         )",
        "(define (selection L)
            (cond ((null? L) '())
                  (else (cons (smallest L (car L))
                              (selection (remove L (smallest L (car L)))))
                  )
            )
         )",
        "(selection '(6 4 5 3 9 3))"
        =>
        "(3 3 4 5 6 9)"
    );
}

#[test]
fn test_merge_sort() {
    assert_evaluates_to!(
        "(define merge-lists
            (lambda (l1 l2)
                (if (null? l1)
                    l2
                    (if (null? l2)
                        l1
                        (if (< (car l1) (car l2))
                            (cons (car l1) (merge-lists (cdr l1) l2))
                            (cons (car l2) (merge-lists (cdr l2) l1)))))))",
        "(define even-numbers
            (lambda (l)
            (if (null? l)
                '()
                (if (null? (cdr l))
                    '()
                    (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))",
        "(define odd-numbers
            (lambda (l)
            (if (null? l)
                '()
                (if (null? (cdr l))
                    (list (car l))
                    (cons (car l) (odd-numbers (cdr (cdr l))))))))",
        "(define merge-sort
            (lambda (l)
            (if (null? l)
                l
                (if (null? (cdr l))
                    l
                    (merge-lists
                        (merge-sort (odd-numbers l))
                        (merge-sort (even-numbers l)))))))",
        "(merge-sort '(3 4 5 2 3 8 9 70 34 23 12 3 45 34))"
        =>
        "(2 3 3 3 4 5 8 9 12 23 34 34 45 70)"
    );
}

#[test]
fn test_macro() {
    assert_evaluates_to!(
        "(let-syntax
            ((when (syntax-rules ()
                ((when test stmt1)
                (if test stmt1)))))
            (let ((x #t))
            (when x (set! x 'now))
            x))"
        =>
        "now"
    );
}
