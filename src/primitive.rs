use std::borrow::Cow;
use std::cmp::PartialOrd;
use std::iter::{repeat, FromIterator};
use std::rc::Rc;

use num::{Zero, One};

use cast::DatumCast;
use number::Number;
use real::Real;
use datum::{concat, Datum};
use error::{RuntimeError, RuntimeErrorKind};
use runtime::{RDatum, RuntimeData, DatumType};

pub trait PrimFunc {
    fn call(&self, Vec<RDatum>) -> Result<RDatum, RuntimeError>;
}

pub trait PossibleError {
    fn make_result(self) -> Result<RDatum, RuntimeError>;
}

impl <T: DatumCast> PossibleError for T {
    fn make_result(self) -> Result<RDatum, RuntimeError> {
        Ok(DatumCast::wrap(self))
    }
}

impl <T: DatumCast> PossibleError for Result<T, RuntimeError> {
    fn make_result(self) -> Result<RDatum, RuntimeError> {
        self.map(DatumCast::wrap)
    }
}

pub struct Fold<P> {
    fold: fn(Vec<P>) -> P
}

pub struct FoldErr<P> {
    fold: fn(Vec<P>) -> Result<P, RuntimeError>
}

pub struct Fold1<P> {
    fold1: fn(P, Vec<P>) -> P
}

pub struct FoldR2<P, R> {
    fold_r2: fn(&P, &P, &[P]) -> R
}

pub struct Fold1Err<P> {
    fold1: fn(P, Vec<P>) -> Result<P, RuntimeError>
}

pub struct F1<T0, R> {
    f1: fn(T0) -> R
}

pub struct F2<T0, T1, R> {
    f2: fn(T0, T1) -> R
}

pub struct R1<T0, R> {
    r1: fn(&T0) -> R
}

impl<T> PrimFunc for Fold<T> where T: DatumCast {
    fn call(&self, args: Vec<RDatum>) -> Result<RDatum, RuntimeError> {
        let p_args:Result<Vec<T>, RuntimeError> = args.into_iter().map(DatumCast::unwrap).collect();
        let f = self.fold;
        p_args.map(|v| f(v).wrap())
    }
}

impl<T> PrimFunc for FoldErr<T> where T: DatumCast {
    fn call(&self, args: Vec<RDatum>) -> Result<RDatum, RuntimeError> {
        let p_args:Result<Vec<T>, RuntimeError> = args.into_iter().map(DatumCast::unwrap).collect();
        p_args.and_then(|v| (self.fold)(v)).map(DatumCast::wrap)
    }
}

impl<T> PrimFunc for Fold1<T> where T: DatumCast {
    fn call(&self, args: Vec<RDatum>) -> Result<RDatum, RuntimeError> {
        let p_args:Result<Vec<T>, RuntimeError> = args.into_iter().map(DatumCast::unwrap).collect();
        let f = self.fold1;
        p_args.and_then(|mut vs|
            if vs.len() < 1 {
                Err(RuntimeError {
                    kind: RuntimeErrorKind::NumArgs,
                    desc: "Expected at least 1 arguments, received 0".to_string()
                })
            } else {
                let v0 = vs.remove(0);
                Ok(f(v0, vs).wrap())
            }
        )
    }
}

impl<T> PrimFunc for Fold1Err<T> where T: DatumCast {
    fn call(&self, args: Vec<RDatum>) -> Result<RDatum, RuntimeError> {
        let p_args:Result<Vec<T>, RuntimeError> = args.into_iter().map(DatumCast::unwrap).collect();
        let f = self.fold1;
        p_args.and_then(|mut vs|
            if vs.len() < 1 {
                Err(RuntimeError {
                    kind: RuntimeErrorKind::NumArgs,
                    desc: "Expected at least 1 arguments, received 0".to_string()
                })
            } else {
                let v0 = vs.remove(0);
                f(v0, vs).map(|res| res.wrap())
            }
        )
    }
}

impl<P: DatumCast, R: DatumCast> PrimFunc for FoldR2<P, R> {
    fn call(&self, args: Vec<RDatum>) -> Result<RDatum, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::NumArgs,
                desc: format!("Expected 2 or more argument, received {:?}", args.len())
            });
        }

        let vs: Vec<P> = try!(args.into_iter().map(DatumCast::unwrap).collect());
        let f = self.fold_r2;
        Ok(DatumCast::wrap(f(&vs[0], &vs[1], &vs[2..])))
    }
}

impl<T0: DatumCast, R: DatumCast> PrimFunc for R1<T0, R> {
    fn call(&self, mut args: Vec<RDatum>) -> Result<RDatum, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::NumArgs,
                desc: format!("Expected 1 argument, received {:?}", args.len())
            });
        }
        let f = self.r1;

        DatumCast::unwrap(args.remove(0)).map(|v| f(&v).wrap())
    }
}

impl<T0: DatumCast, T1: DatumCast, R: PossibleError> PrimFunc for F2<T0, T1, R> {
    fn call(&self, mut args: Vec<RDatum>) -> Result<RDatum, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::NumArgs,
                desc: format!("Expected 2 arguments, received {:?}", args.len())
            });
        }

        let a1 = try!(DatumCast::unwrap(args.pop().unwrap()));
        let a0 = try!(DatumCast::unwrap(args.pop().unwrap()));

        ((self.f2)(a0, a1)).make_result()
    }
}

impl<T0: DatumCast, T1: DatumCast, R: PossibleError> PrimFunc for F2<T0, Option<T1>, R> {
    fn call(&self, mut args: Vec<RDatum>) -> Result<RDatum, RuntimeError> {
        let (a0, a1) = match args.len() {
            1 => {
                let a0 = try!(DatumCast::unwrap(args.pop().unwrap()));
                (a0, None)
            },
            2 => {
                let a1 = try!(DatumCast::unwrap(args.pop().unwrap()));
                let a0 = try!(DatumCast::unwrap(args.pop().unwrap()));
                (a0, Some(a1))
            },
            _ => return Err(RuntimeError {
                kind: RuntimeErrorKind::NumArgs,
                desc: format!("Expected 1 or 2 arguments, received {:?}", args.len())
            })
        };

        ((self.f2)(a0, a1)).make_result()
    }
}

impl<T0: DatumCast, R: PossibleError> PrimFunc for F1<T0, R> {
    fn call(&self, mut args: Vec<RDatum>) -> Result<RDatum, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::NumArgs,
                desc: format!("Expected 1 argument, received {:?}", args.len())
            });
        }
        DatumCast::unwrap(args.remove(0)).and_then(|v| (self.f1)(v).make_result())
    }
}

fn add(args: Vec<Number>) -> Number {
    let mut sum:Number = Zero::zero();
    for a in args.into_iter() {
        sum = sum + a;
    }
    return sum;
}

/// `(+ n0 n1 ...)`
pub static PRIM_ADD:Fold<Number> = Fold { fold: add };

fn mul(args: Vec<Number>) -> Number {
    let mut product:Number = One::one();
    for a in args.iter() {
        product = &product * a;
    }
    return product;
}

/// `(* n0 n1 ...)`
pub static PRIM_MUL:Fold<Number> = Fold { fold: mul };

fn sub(arg0: Number, args: Vec<Number>) -> Number {
    if args.len() == 0 {
        return -arg0;
    }
    let mut sum:Number = arg0;
    for a in args.into_iter() {
        sum = sum - a;
    }
    return sum;
}

/// `(- n0 n1 ...)`
pub static PRIM_SUB:Fold1<Number> = Fold1 { fold1: sub };

fn div(arg0: Number, args: Vec<Number>) -> Result<Number, RuntimeError> {
    if args.len() == 0 {
        if arg0.is_exact() && arg0.is_zero() {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::DivideByZero,
                desc: "Tried to divied by 0".to_string()
            });
        }
        return Ok(-arg0);
    }

    let mut product:Number = arg0;
    for a in args.into_iter() {
        if a.is_exact() && a.is_zero() {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::DivideByZero,
                desc: "Tried to divied by 0".to_string()
            });
        }
        product = product / a;
    }

    return Ok(product);
}

/// `(/ n0 n1 ...)`
pub static PRIM_DIV:Fold1Err<Number> = Fold1Err { fold1: div };

fn list(args: Vec<RDatum>) -> RDatum {
    debug!("list: args = {:?}", args);
    FromIterator::from_iter(args.into_iter())
}

/// `(list a0 a1 ...)`
pub static PRIM_LIST:Fold<RDatum> = Fold { fold: list };

/// `(make-vector k)` or `(make-vector k fill)`
pub static PRIM_MAKE_VECTOR: F2<usize, Option<RDatum>, Vec<RDatum>> = F2 { f2: make_vector };

fn make_vector(k: usize, fill_opt: Option<RDatum>) -> Vec<RDatum> {
    let fill = fill_opt.unwrap_or(Datum::Ext(RuntimeData::Undefined));
    repeat(fill).take(k).collect()
}

/// `(vector-ref vector k)`
pub static PRIM_VECTOR_REF: F2<Vec<RDatum>, usize, Result<RDatum, RuntimeError>> = F2 { f2: vector_ref };

fn vector_ref(vector: Vec<RDatum>, k: usize) -> Result<RDatum, RuntimeError> {
    match vector.get(k) {
        Some(e) => Ok(e.clone()),
        None => Err(RuntimeError {
            kind: RuntimeErrorKind::IndexOutOfRange,
            desc: format!("vector length is {}, but index is {}", vector.len(), k)
        })
    }
}

/// `(vector->list vector)`
pub static PRIM_VECTOR_TO_LIST: F1<Vec<RDatum>, RDatum> = F1 { f1: vector_to_list };

fn vector_to_list(vector: Vec<RDatum>) -> RDatum {
    Datum::from_iter(vector)
}

/// `(list->vector list)`
pub static PRIM_LIST_TO_VECTOR: F1<RDatum, Result<Vec<RDatum>, RuntimeError>> = F1 { f1: list_to_vector };

fn list_to_vector(list: RDatum) -> Result<Vec<RDatum>, RuntimeError> {
    let v: Result<Vec<RDatum>, ()> = list.iter().collect();
    v.map_err(|_| RuntimeError {
        kind: RuntimeErrorKind::InvalidType,
        desc: format!("Expected list, but received {:?}", DatumType::get_type(&list))
    })
}

/// `(vector a0 a1 ...)`
pub static PRIM_VECTOR:Fold<RDatum> = Fold { fold: vector };

fn vector(args: Vec<RDatum>) -> RDatum {
    Datum::Vector(Rc::new(args))
}

fn car(arg: (RDatum, RDatum)) -> RDatum {
    arg.0
}

/// `(cons x y)`
pub static PRIM_CONS: F2<RDatum, RDatum, RDatum> = F2 { f2: cons };

fn cons(car: RDatum, cdr: RDatum) -> RDatum {
    ::datum::cons(car, cdr)
}

/// `(car x)`
pub static PRIM_CAR: F1<(RDatum, RDatum), RDatum> = F1 { f1: car };

fn cdr(arg: (RDatum, RDatum)) -> RDatum {
    arg.1
}

/// `(cdr x)`
pub static PRIM_CDR: F1<(RDatum, RDatum), RDatum> = F1 { f1: cdr };

/// `(zero? x)`
pub static PRIM_IS_ZERO: R1<Number, bool> = R1 { r1: Zero::is_zero };

fn is_real(arg: &RDatum) -> bool {
    match arg {
        &Datum::Num(Number::Real(_)) => true,
        _ => false
    }
}

/// `(real? x)`
pub static PRIM_IS_REAL: R1<RDatum, bool> = R1 { r1: is_real };

fn is_rational(arg: &RDatum) -> bool {
    match arg {
        &Datum::Num(Number::Real(Real::Flonum(f))) => f.is_finite(),
        &Datum::Num(Number::Real(_)) => true,
        _ => false
    }
}

/// `(rational? x)`
pub static PRIM_IS_RATIONAL: R1<RDatum, bool> = R1 { r1: is_rational };

fn is_integer(arg: &RDatum) -> bool {
    match arg {
        &Datum::Num(Number::Real(ref r)) => r.is_integer(),
        _ => false
    }
}

/// `(integer? x)`
pub static PRIM_IS_INTEGER: R1<RDatum, bool> = R1 { r1: is_integer };

macro_rules! impl_num_comp {
    ($type_name:ident, $static_name:ident, $func_name:ident, $op:ident) => (
        fn $func_name(arg0: &$type_name, arg1: &$type_name, args: &[$type_name]) -> bool {
            if !arg0.$op(arg1) {
                return false;
            }
            let mut last_arg = arg1;
            for arg in args.iter() {
                if !last_arg.$op(arg) {
                    return false;
                }
                last_arg = arg;
            }
            return true;
        }

        pub static $static_name: FoldR2<$type_name, bool> = FoldR2 { fold_r2: $func_name };
    )
}

impl_num_comp!(Number, PRIM_NUM_EQ, num_eq, eq);
impl_num_comp!(Real, PRIM_LT, real_lt, lt);
impl_num_comp!(Real, PRIM_GT, real_gt, gt);
impl_num_comp!(Real, PRIM_LE, real_le, le);
impl_num_comp!(Real, PRIM_GE, real_ge, ge);

macro_rules! impl_typecheck {
    ($static_name:ident, $func_name:ident, $type_name:ident) => (
        fn $func_name(arg: &RDatum) -> bool {
            DatumType::get_type(arg) == DatumType::$type_name
        }

        pub static $static_name: R1<RDatum, bool> = R1 { r1: $func_name };
    )
}

impl_typecheck!(PRIM_IS_BOOLEAN, is_boolean, Bool);
impl_typecheck!(PRIM_IS_PAIR, is_pair, Pair);
impl_typecheck!(PRIM_IS_SYMBOL, is_symbol, Sym);
impl_typecheck!(PRIM_IS_NUMBER, is_number, Num);
impl_typecheck!(PRIM_IS_CHAR, is_char, Char);
impl_typecheck!(PRIM_IS_STRING, is_string, String);
impl_typecheck!(PRIM_IS_VECTOR, is_vector, Vector);
impl_typecheck!(PRIM_IS_PROCEDURE, is_procedure, Callable);
impl_typecheck!(PRIM_IS_NULL, is_null, Null);

pub static PRIM_NOT: R1<RDatum, bool> = R1 { r1: not };

fn not(b: &RDatum) -> bool {
    match b {
        &Datum::Bool(false) => true,
        _ => false
    }
}

pub static PRIM_SYMBOL_TO_STRING: R1<Cow<'static, str>, String> = R1 { r1: symbol_string };

fn symbol_string(sym: &Cow<'static, str>) -> String {
    sym.to_string()
}

pub static PRIM_APPEND: FoldErr<RDatum> = FoldErr { fold: append };

fn append(mut lists: Vec<RDatum>) -> Result<RDatum, RuntimeError> {
    let mut res = match lists.pop() {
        Some(elem) => elem,
        None => return Ok(Datum::Nil)
    };

    loop {
        match lists.pop() {
            Some(elem) => {
                res = match concat(elem, res) {
                    Ok(list) => list,
                    Err(()) => return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidType,
                        desc: "Non-list given to append".to_string()
                    })
                };
            },
            None => {
                return Ok(res)
            }
        }
    }
}

/// Lists all primitive functions with its name
pub fn libprimitive() -> Vec<(&'static str, &'static (PrimFunc + 'static))> {
    vec![
        ("+", &PRIM_ADD),
        ("-", &PRIM_SUB),
        ("*", &PRIM_MUL),
        ("/", &PRIM_DIV),
        ("list", &PRIM_LIST),
        ("vector", &PRIM_VECTOR),
        ("make-vector", &PRIM_MAKE_VECTOR),
        ("vector-ref", &PRIM_VECTOR_REF),
        ("vector->list", &PRIM_VECTOR_TO_LIST),
        ("list->vector", &PRIM_LIST_TO_VECTOR),
        ("boolean?", &PRIM_IS_BOOLEAN),
        ("pair?", &PRIM_IS_PAIR),
        ("symbol?", &PRIM_IS_SYMBOL),
        ("number?", &PRIM_IS_NUMBER),
        ("char?", &PRIM_IS_CHAR),
        ("string?", &PRIM_IS_STRING),
        ("vector?", &PRIM_IS_VECTOR),
        ("procedure?", &PRIM_IS_PROCEDURE),
        ("null?", &PRIM_IS_NULL),
        ("cons", &PRIM_CONS),
        ("car", &PRIM_CAR),
        ("cdr", &PRIM_CDR),
        ("zero?", &PRIM_IS_ZERO),
        // complex? is synonym to number?
        ("complex?", &PRIM_IS_NUMBER),
        ("real?", &PRIM_IS_REAL),
        ("rational?", &PRIM_IS_RATIONAL),
        ("integer?", &PRIM_IS_INTEGER),
        ("not", &PRIM_NOT),
        ("=", &PRIM_NUM_EQ),
        ("<", &PRIM_LT),
        (">", &PRIM_GT),
        ("<=", &PRIM_LE),
        (">=", &PRIM_GE),
        ("symbol->string", &PRIM_SYMBOL_TO_STRING),
        ("append", &PRIM_APPEND)
    ]
}
