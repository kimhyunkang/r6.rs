use std::io;
use std::borrow;
use std::char;
use std::uint;
use std::result;
use std::str;
use std::vec;
use std::num::{One, Zero, ToStrRadix, IntConvertible};
use std::hashmap::HashMap;
use std::managed;
use bounded_iterator::BoundedIterator;
use bigint_helper::modulo;
use extra::bigint::BigInt;
use extra::complex::Cmplx;
use datum::*;
use primitive::*;
use numeric::*;
use real::*;
use rational::Rational;
use stack::*;
use parser::Parser;

enum RuntimeData {
    RBot,
    RUndef,
    RInputPort(@Reader),
    ROutputPort(@Writer),
    RPrim(PFunc),
    RProc(~[@str],
        Option<@str>,
        ~[@LDatum<RuntimeData>],
        @mut Stack<HashMap<@str, @LDatum<RuntimeData>>>),
}

impl Clone for RuntimeData {
    fn clone(&self) -> RuntimeData {
        match self {
            &RBot => RBot,
            &RUndef => RUndef,
            &RPrim(f) => RPrim(f),
            &RProc(ref args, ref vargs, ref body, ref env) => {
                let cloneargs = do args.map |&arg| {
                    arg
                };
                RProc(cloneargs, *vargs, body.clone(), *env)
            },
            &RInputPort(rdr) => RInputPort(rdr),
            &ROutputPort(wr) => ROutputPort(wr),
        }
    }
}

fn eq(lhs: &RuntimeData, rhs: &RuntimeData) -> bool {
    match (lhs, rhs) {
        (&RPrim(l), &RPrim(r)) => l == r,
        (&RProc(_,_,_,_), &RProc(_,_,_,_)) => lhs == rhs,
        _ => false,
    }
}

impl Eq for RuntimeData {
    fn eq(&self, other: &RuntimeData) -> bool {
        eq(self, other)
    }

    fn ne(&self, other: &RuntimeData) -> bool {
        !eq(self, other)
    }
}

fn data_to_str(data: &RuntimeData) -> ~str {
    match *data {
        RBot => ~"<bottom>",
        RUndef => ~"<undefined>",
        RPrim(f) => fmt!("<primitive:%s>", f.to_str()),
        RProc(_, _, _, _) => fmt!("<procedure 0x%08x>", borrow::to_uint(data)),
        RInputPort(_) => fmt!("<input-port>"),
        ROutputPort(_) => fmt!("<output-port>"),
    }
}

impl ToStr for RuntimeData {
    fn to_str(&self) -> ~str {
        data_to_str(self)
    }
}

type RDatum = LDatum<RuntimeData>;

pub trait DatumConv {
    fn from_datum<R>(@RDatum, &fn(&Self) -> R) -> Option<R>;
    fn move_datum(Self) -> @RDatum;
    fn typename() -> ~str;
}

impl LDatum<RuntimeData> {
    pub fn is_bottom(&self) -> bool {
        match *self {
            LExt(RBot) => true,
            _ => false,
        }
    }
}

impl DatumConv for @RDatum {
    fn from_datum<R>(datum: @RDatum, op: &fn(&@RDatum) -> R) -> Option<R> {
        Some(op(&datum))
    }

    fn move_datum(x: @RDatum) -> @RDatum {
        x
    }

    fn typename() -> ~str {
        ~"datum"
    }
}

impl DatumConv for RuntimeData {
    fn from_datum<R>(datum: @RDatum, op: &fn(&RuntimeData) -> R) -> Option<R> {
        match datum {
            @LExt(ref r) => Some(op(r)),
            _ => None,
        }
    }

    fn move_datum(x: RuntimeData) -> @RDatum {
        @LExt(x)
    }

    fn typename() -> ~str {
        ~"procedure"
    }
}

impl DatumConv for @Reader {
    fn from_datum<R>(datum: @RDatum, op: &fn(&@Reader) -> R) -> Option<R> {
        match datum {
            @LExt(RInputPort(ref r)) => Some(op(r)),
            _ => None,
        }
    }

    fn move_datum(x: @Reader) -> @RDatum {
        @LExt(RInputPort(x))
    }

    fn typename() -> ~str {
        ~"input-port"
    }
}

impl DatumConv for @Writer {
    fn from_datum<R>(datum: @RDatum, op: &fn(&@Writer) -> R) -> Option<R> {
        match datum {
            @LExt(ROutputPort(ref r)) => Some(op(r)),
            _ => None,
        }
    }

    fn move_datum(x: @Writer) -> @RDatum {
        @LExt(ROutputPort(x))
    }

    fn typename() -> ~str {
        ~"output-port"
    }
}

impl DatumConv for LNumeric {
    fn from_datum<R>(datum: @RDatum, op: &fn(&LNumeric) -> R) -> Option<R> {
        match datum {
            @LNum(ref n) => Some(op(n)),
            _ => None,
        }
    }

    fn move_datum(x: LNumeric) -> @RDatum {
        @LNum(x)
    }

    fn typename() -> ~str {
        ~"number"
    }
}

impl DatumConv for Cmplx<f64> {
    fn from_datum<R>(datum: @RDatum, op: &fn(&Cmplx<f64>) -> R) -> Option<R> {
        match datum {
            @LNum(NInexact(ref n)) => Some(op(n)),
            _ => None,
        }
    }

    fn move_datum(x: Cmplx<f64>) -> @RDatum {
        @LNum(NInexact(x))
    }

    fn typename() -> ~str {
        ~"inexact number"
    }
}

impl DatumConv for f64 {
    fn from_datum<R>(datum: @RDatum, op: &fn(&f64) -> R) -> Option<R> {
        match datum {
            @LNum(NInexact(ref n)) if n.im.is_zero() => Some(op(&n.re)),
            _ => None,
        }
    }

    fn move_datum(x: f64) -> @RDatum {
        @LNum(from_f64(x))
    }

    fn typename() -> ~str {
        ~"inexact real number"
    }
}

impl DatumConv for Cmplx<Rational> {
    fn from_datum<R>(datum: @RDatum, op: &fn(&Cmplx<Rational>) -> R) -> Option<R> {
        match datum {
            @LNum(NExact(ref n)) => Some(op(n)),
            _ => None,
        }
    }

    fn move_datum(x: Cmplx<Rational>) -> @RDatum {
        @LNum(NExact(x))
    }

    fn typename() -> ~str {
        ~"exact number"
    }
}

impl DatumConv for LReal {
    fn from_datum<R>(datum: @RDatum, op: &fn(&LReal) -> R) -> Option<R> {
        match datum {
            @LNum(NReal(ref r)) => Some(op(r)),
            _ => None,
        }
    }

    fn move_datum(x: LReal) -> @RDatum {
        @LNum(NReal(x))
    }

    fn typename() -> ~str {
        ~"real number"
    }
}

impl DatumConv for Rational {
    fn from_datum<R>(datum: @RDatum, op: &fn(&Rational) -> R) -> Option<R> {
        match datum {
            @LNum(NExact(ref n)) if n.im.is_zero() => {
                Some(op(&n.re))
            },
            _ => None,
        }
    }

    fn move_datum(x: Rational) -> @RDatum {
        @LNum(from_rational(x))
    }

    fn typename() -> ~str {
        ~"rational number"
    }
}

impl DatumConv for BigInt {
    fn from_datum<R>(datum: @RDatum, op: &fn(&BigInt) -> R) -> Option<R> {
        match datum {
            @LNum(ref n) => match *n {
                NReal(RInt(ref n)) => Some(op(n)),
                _ => None,
            },
            _ => None,
        }
    }

    fn move_datum(x: BigInt) -> @RDatum {
        @LNum(from_bigint(x))
    }

    fn typename() -> ~str {
        ~"integer"
    }
}

impl DatumConv for uint {
    fn from_datum<R>(datum: @RDatum, op: &fn(&uint) -> R) -> Option<R> {
        let max_int:BigInt = IntConvertible::from_int(Bounded::max_value::<int>());
        match datum {
            @LNum(NReal(RInt(ref n))) =>
                if !n.is_negative() {
                    if *n <= max_int {
                        Some(op(&(n.to_int() as uint)))
                    } else {
                        None
                    }
                } else {
                    None
                },
            _ => None,
        }
    }

    fn move_datum(x: uint) -> @RDatum {
        @LNum(from_uint(x))
    }

    fn typename() -> ~str {
        ~"unsigned integer"
    }
}

impl DatumConv for (@RDatum, @RDatum) {
    fn from_datum<R>(datum: @RDatum, op: &fn(&(@RDatum, @RDatum)) -> R) -> Option<R> {
        match datum {
            @LCons(a, b) => Some(op(&(a, b))),
            _ => None,
        }
    }

    fn move_datum((x, y): (@RDatum, @RDatum)) -> @RDatum {
        @LCons(x, y)
    }

    fn typename() -> ~str {
        ~"cons"
    }
}

impl DatumConv for bool {
    fn from_datum<R>(datum: @RDatum, op: &fn(&bool) -> R) -> Option<R> {
        match datum {
            @LBool(ref b) => Some(op(b)),
            _ => None,
        }
    }

    fn move_datum(x: bool) -> @RDatum {
        @LBool(x)
    }

    fn typename() -> ~str {
        ~"boolean"
    }
}

impl DatumConv for char {
    fn from_datum<R>(datum: @RDatum, op: &fn(&char) -> R) -> Option<R> {
        match datum {
            @LChar(ref c) => Some(op(c)),
            _ => None,
        }
    }

    fn move_datum(x: char) -> @RDatum {
        @LChar(x)
    }

    fn typename() -> ~str {
        ~"character"
    }
}

impl DatumConv for () {
    fn from_datum<R>(datum: @RDatum, op: &fn(&()) -> R) -> Option<R> {
        match datum {
            @LExt(RBot) => Some(op(&())),
            _ => None,
        }
    }

    fn move_datum(_: ()) -> @RDatum {
        @LExt(RBot)
    }

    fn typename() -> ~str {
        ~"bottom"
    }
}

impl<T: DatumConv> DatumConv for ~[T] {
    #[inline]
    fn from_datum<R>(datum: @RDatum, op: &fn(&~[T]) -> R) -> Option<R> {
        match datum.to_list() {
            Some(l) => {
                let mut idx = 0u;
                let mut list = vec::with_capacity(l.len());

                while idx < l.len() {
                    match DatumConv::from_datum::<T, T>(l[idx], |&x| {x}) {
                        Some(x) => list.push(x),
                        None => return None,
                    }

                    idx += 1;
                }

                return Some(op(&list))
            }
            _ => None,
        }
    }

    #[inline]
    fn move_datum(x: ~[T]) -> @RDatum {
        let mut bottom = @LNil;
        do x.consume_reverse |_, a| {
            bottom = @LCons(DatumConv::move_datum(a), bottom);
        };
        bottom
    }

    fn typename() -> ~str {
        fmt!("list of %s", DatumConv::typename::<T>())
    }
}

impl DatumConv for @mut ~[@RDatum] {
    #[inline]
    fn from_datum<R>(datum: @RDatum, op: &fn(&@mut ~[@RDatum]) -> R) -> Option<R> {
        match datum {
            @LVector(ref v) => Some(op(v)),
            _ => None,
        }
    }

    #[inline]
    fn move_datum(x: @mut ~[@RDatum]) -> @RDatum {
        @LVector(x)
    }

    fn typename() -> ~str {
        ~"vector"
    }
}

impl DatumConv for ~str {
    fn from_datum<R>(datum: @RDatum, op: &fn(&~str) -> R) -> Option<R> {
        match datum {
            @LString(ref s) => Some(op(s)),
            _ => None,
        }
    }

    fn move_datum(x: ~str) -> @RDatum {
        @LString(x)
    }

    fn typename() -> ~str {
        ~"string"
    }
}

impl DatumConv for @str {
    fn from_datum<R>(datum: @RDatum, op: &fn(&@str) -> R) -> Option<R> {
        match datum {
            @LIdent(ref s) => Some(op(s)),
            _ => None,
        }
    }

    fn move_datum(x: @str) -> @RDatum {
        @LIdent(x)
    }

    fn typename() -> ~str {
        ~"symbol"
    }
}

struct Runtime {
    stdin: @Reader,
    stdout: @Writer,
    stderr: @Writer,
    env: @mut Stack<HashMap<@str, @RDatum>>,
    global: HashMap<@str, Either<@RDatum, PrimSyntax>>,
    qq_lvl: uint,
}

#[deriving(Eq)]
enum RuntimeError {
    UnboundVariable(@str),
    RefMacro(@str),
    NotCallable,
    NotList,
    ArgNumError(uint, Option<uint>, uint),
    TypeError,
    DivideByZeroError,
    NilEval,
    BadSyntax(PrimSyntax, ~str),
    ParseError(uint, uint, ~str),
    RangeError,
    IOError(~str),
    PrimitiveError(~str),
}

impl ToStr for RuntimeError {
    fn to_str(&self) -> ~str {
        err_to_str(self)
    }
}

priv fn err_to_str(&err: &RuntimeError) -> ~str {
    match err {
        UnboundVariable(name) => ~"unbound variable: " + name,
        RefMacro(name) => ~"cannot reference macro name: " + name,
        NotCallable => ~"not callable",
        NotList => ~"not list",
        ArgNumError(min, Some(max), argnum) => {
            if min == max {
                fmt!("expected %u arguments, but found %u arguments", min, argnum)
            } else {
                fmt!("expected %u-%u arguments, but found %u arguments", min, max, argnum)
            }
        },
        ArgNumError(expected, None, argnum) => {
            fmt!("expected %u or more arguments, but found %u arguments", expected, argnum)
        },
        TypeError => ~"type error",
        DivideByZeroError => ~"divide by zero",
        NilEval => ~"() cannot be evaluated",
        BadSyntax(syn, reason) => ~"bad syntax for " + syn.to_str() + ": " + reason,
        ParseError(line, col, reason) => fmt!("failed to parse: %u:%u: %s", line, col, reason),
        RangeError => ~"index out of range", 
        IOError(e) => fmt!("io error: %s", e),
        PrimitiveError(e) => e,
    }
}

fn load_prelude() -> HashMap<@str, Either<@RDatum, PrimSyntax>> {
    let mut map = HashMap::new();
    let mut prim_iter = BoundedIterator::new::<PFunc>();
    for prim_iter.advance |prim:PFunc| {
        let key = proc_to_str(&prim);
        map.insert(key, Left(@LExt(RPrim(prim))));
    }

    let mut syntax_iter = BoundedIterator::new::<PrimSyntax>();
    for syntax_iter.advance |syntax:PrimSyntax| {
        let key = syntax_to_str(&syntax);
        map.insert(key, Right(syntax));
    }

    map.insert("pi".to_managed(), Left(@LNum(inexact(Real::pi(), 0f64))));
    map
}

priv fn typecheck<A: DatumConv>(args: &[@RDatum]) -> Result<@RDatum, RuntimeError>
{
    match args {
        [arg] => {
            let res = do DatumConv::from_datum::<A, ()>(arg) |_| { () };
            match res {
                Some(_) => Ok(@LBool(true)),
                None => Ok(@LBool(false)),
            }
        },
        _ => Err(ArgNumError(1, Some(1), args.len())),
    }
}

priv fn call_tc1<A: DatumConv, R: DatumConv> (
        args: &[@RDatum], op: &fn(&A) -> R
    ) -> Result<@RDatum, RuntimeError>
{
    match args {
        [arg] => {
            let res = DatumConv::from_datum::<A, R>(arg, op);
            match res {
                Some(x) => Ok(DatumConv::move_datum(x)),
                None => Err(TypeError),
            }
        },
        _ => Err(ArgNumError(1, Some(1), args.len())),
    }
}

priv fn call_tc2<A: DatumConv, B:DatumConv, R: DatumConv> (
        args: &[@RDatum], op: &fn(&A, &B) -> R
    ) -> Result<@RDatum, RuntimeError>
{
    match args {
        [arg0, arg1] => {
            let res = do DatumConv::from_datum::<A, Option<R>>(arg0) |a| {
                do DatumConv::from_datum::<B, R>(arg1) |b| {
                    op(a, b)
                }
            };
            match res {
                Some(Some(x)) => Ok(DatumConv::move_datum(x)),
                _ => Err(TypeError),
            }
        },
        _ => Err(ArgNumError(2, Some(2), args.len())),
    }
}

priv fn call_vargs<A: DatumConv, R: DatumConv> (args: &[@RDatum], op: &fn(&[A]) -> R)
    -> Result<@RDatum, RuntimeError>
{
    let mut idx = 0u;
    let mut vec = vec::with_capacity(args.len());

    while idx < args.len() {
        match DatumConv::from_datum(args[idx], |&a| {a}) {
            Some(x) => vec.push(x),
            None => return Err(TypeError),
        };

        idx += 1;
    }

    let res:@RDatum = DatumConv::move_datum(op(vec));
    Ok(res)
}

priv fn call_bfoldl<A: Clone + DatumConv> (args: &[@RDatum], op: &fn(&A, &A) -> bool)
    -> Result<@RDatum, RuntimeError>
{
    match args {
        [arg0, ..tail] => match DatumConv::from_datum(arg0, |&x| {x}) {
            None => Err(TypeError),
            Some(x) => {
                let mut a = x;
                let mut err = false;

                let res = do tail.each |&arg| {
                    match DatumConv::from_datum(arg, |&x| {x}) {
                        Some(b) => {
                            let res = op(&a, &b);
                            a = b;
                            res
                        },
                        None => {
                            err = true;
                            false
                        },
                    }
                };

                if err {
                    Err(TypeError)
                } else {
                    Ok(@LBool(res))
                }
            },
        },
        _ => Err(ArgNumError(1, None, args.len())),
    }
}

priv fn call_foldl<A: Clone + DatumConv> (
        args: &[@RDatum], a0: &A, op: &fn(&A, &A) -> Result<A, RuntimeError>
    ) -> Result<@RDatum, RuntimeError>
{
    let mut res = a0.clone();
    let mut err = None;
    do args.each |&arg| {
        match DatumConv::from_datum(arg, |a| { op(&res, a) }) {
            None => err = Some(TypeError),
            Some(Ok(n)) => res = n,
            Some(Err(e)) => err = Some(e),
        };
        err.is_none()
    };

    match err {
        Some(e) => Err(e),
        _ => Ok(DatumConv::move_datum(res)),
    }
}

priv fn call_err1<A: DatumConv, R: DatumConv> (
        args: &[@RDatum], op: &fn(&A) -> Result<R, RuntimeError>
    ) -> Result<@RDatum, RuntimeError>
{
    match args {
        [arg] => {
            match DatumConv::from_datum(arg, op) {
                Some(Ok(x)) => Ok(DatumConv::move_datum(x)),
                Some(Err(e)) => Err(e),
                None => Err(TypeError),
            }
        },
        _ => Err(ArgNumError(1, Some(1), args.len())),
    }
}

priv fn call_err2<A: DatumConv, B: DatumConv, R: DatumConv> (
        args: &[@RDatum], op: &fn(&A, &B) -> Result<R, RuntimeError>
    ) -> Result<@RDatum, RuntimeError>
{
    match args {
        [arg0, arg1] => {
            let r = do DatumConv::from_datum::<A, Result<R, RuntimeError>>(arg0) |a| {
                let res = do DatumConv::from_datum::<B, Result<R, RuntimeError>>(arg1) |b| {
                    op(a, b)
                };
                match res {
                    Some(x) => x,
                    None => Err(TypeError),
                }
            };
            match r {
                Some(Ok(x)) => Ok(DatumConv::move_datum(x)),
                Some(Err(e)) => Err(e),
                None => Err(TypeError),
            }
        },
        _ => Err(ArgNumError(2, Some(2), args.len())),
    }
}

priv fn call_err3<A: DatumConv, B: DatumConv, C: DatumConv, R: DatumConv> (
        args: &[@RDatum], op: &fn(&A, &B, &C) -> Result<R, RuntimeError>
    ) -> Result<@RDatum, RuntimeError>
{
    match args {
        [arg0, arg1, arg2] => {
            let r0 = do DatumConv::from_datum::<A, Result<R, RuntimeError>>(arg0) |a| {
                let r1 = do DatumConv::from_datum::<B, Result<R, RuntimeError>>(arg1) |b| {
                    let r2 = do DatumConv::from_datum::<C, Result<R, RuntimeError>>(arg2) |c| {
                        op(a, b, c)
                    };
                    match r2 {
                        Some(x) => x,
                        None => Err(TypeError),
                    }
                };
                match r1 {
                    Some(x) => x,
                    None => Err(TypeError),
                }
            };
            match r0 {
                Some(Ok(x)) => Ok(DatumConv::move_datum(x)),
                Some(Err(e)) => Err(e),
                None => Err(TypeError),
            }
        },
        _ => Err(ArgNumError(3, Some(3), args.len())),
    }
}

priv fn transpose(args: &[@RDatum]) -> Result<~[~[@RDatum]], RuntimeError>
{
    if args.len() == 0 {
        return Err(ArgNumError(2, None, 1))
    }
    let mut ptrs: ~[@RDatum] = do args.map |&arg| { arg };
    let mut trans = ~[];
    loop {
        match ptrs[0] {
            @LNil => if ptrs.each(|&arg| {*arg == LNil}) {
                    return Ok(trans)
                } else {
                    return Err(PrimitiveError(~"length of args are not equal"))
                },
            @LCons(_, _) => {
                let mut i = 0u;
                let mut line = vec::with_capacity(args.len());
                while i < ptrs.len() {
                    match ptrs[i] {
                        @LCons(h, t) => {
                            line.push(h);
                            ptrs[i] = t;
                        },
                        @LNil => 
                            return Err(PrimitiveError(~"length of args are not equal")),
                        _ => 
                            return Err(PrimitiveError(~"non-list argument")),
                    }
                    i += 1
                }
                trans.push(line)
            },
            _ => return Err(NotList),
        }
    }
}

#[inline]
priv fn ci_cmp(a: char, b: char, op: &fn(u8, u8) -> bool) -> bool
{
    op(a.to_ascii().to_lower().to_byte(), b.to_ascii().to_lower().to_byte())
}

#[inline]
priv fn str_ci_cmp(a: &str, b: &str, op: &fn(~str, ~str) -> bool) -> bool
{
    op(a.to_ascii().to_lower().to_str_ascii(), b.to_ascii().to_lower().to_str_ascii())
}

priv fn get_bindings(arg: &RDatum) -> Result<~[(@str, @RDatum)], ~str> {
    match arg.to_list() {
        None => Err(~"non-list bindings"),
        Some(bindings) => do result::map_vec(bindings) |datum| {
            match datum.to_list() {
                Some([@LIdent(name), expr]) => Ok((name, expr)),
                Some(_) | None => Err(~"invalid binding")
            }
        }
    }
}

priv fn read(rdr: @io::Reader) -> Result<@RDatum, RuntimeError>
{
    let mut parser = Parser(rdr);
    match parser.parse() {
        Ok(datum) => Ok(@datum),
        Err(e) => {
            let (line, col) = parser.pos();
            Err(ParseError(line, col, e))
        },
    }
}

priv fn get_syms(&arg: &@RDatum) -> Result<(~[@str], Option<@str>), ~str> {
    let mut iter = arg;
    let mut args : ~[@str] = ~[];
    let mut varargs : Option<@str> = None;

    loop {
        match *iter {
            LCons(h, t) => match *h {
                LIdent(name) => {
                    args.push(name);
                    iter = t;
                },
                _ => {
                    return Err(~"non-symbol argument");
                }
            },
            LIdent(name) => {
                varargs = Some(name);
                break;
            },
            LNil => {
                break;
            },
            _ => {
                return Err(~"non-list argument");
            },
        }
    }

    Ok((args, varargs))
}

impl Runtime {
    fn get_syntax(&self, val: &RDatum) -> Option<PrimSyntax> {
        match *val {
            LIdent(name) => match self.global.find(&name) {
                Some(&Right(syn)) => Some(syn),
                _ => None,
            },
            _ => None,
        }
    }

    fn find_var(&self, name: &@str) -> Result<@RDatum, RuntimeError> {
        let mut val: Option<@RDatum> = None;

        do self.env.each |frame| {
            match frame.find(name) {
                None => true,
                Some(v) => {
                    val = Some(*v);
                    false
                }
            }
        };

        match val {
            None => match self.global.find(name) {
                Some(&Left(v)) => Ok(v),
                Some(&Right(_)) => Err(RefMacro(*name)),
                None => Err(UnboundVariable(*name)),
            },
            Some(v) => Ok(v),
        }
    }

    fn syn_let(&mut self, bindings: &RDatum, body: &[@RDatum]) -> Result<@RDatum, RuntimeError> {
        match get_bindings(bindings) {
            Err(e) => Err(BadSyntax(SynLet, e)),
            Ok(b) => {
                let mut arg_frame = HashMap::new();
                let mut err:Option<RuntimeError> = None;
                do b.each |&(name, expr)| {
                    match self.eval(expr) {
                        Ok(val) => {
                            arg_frame.insert(name, val);
                            true
                        }
                        Err(e) => {
                            err = Some(e);
                            false
                        }
                    }
                };
                match err {
                    Some(e) => Err(e),
                    None => self.local_eval(arg_frame, self.env, body)
                }
            }
        }
    }

    fn syn_letstar(&mut self, bindings: &RDatum, body: &[@RDatum])
        -> Result<@RDatum, RuntimeError>
    {
        match get_bindings(bindings) {
            Err(e) => Err(BadSyntax(SynLet, e)),
            Ok(b) => {
                let old_frame = self.env;
                let mut err:Option<RuntimeError> = None;
                do b.each |&(name, expr)| {
                    match self.eval(expr) {
                        Ok(val) => {
                            let mut arg_frame = HashMap::new();
                            arg_frame.insert(name, val);
                            self.env = @mut push(self.env, arg_frame);
                            true
                        },
                        Err(e) => {
                            err = Some(e);
                            false
                        },
                    }
                };

                let mut res:Result<@RDatum, RuntimeError> = Err(NilEval);
                match err {
                    Some(e) => {
                        res = Err(e);
                    },
                    None => {
                        do body.each |&val| {
                            res = self.eval(val);
                            res.is_ok()
                        };
                    }
                };

                self.env = old_frame;
                return res
            }
        }
    }

    fn syn_letrec(&mut self, bindings: &RDatum, body: &[@RDatum]) -> Result<@RDatum, RuntimeError>
    {
        match get_bindings(bindings) {
            Err(e) => Err(BadSyntax(SynLet, e)),
            Ok(b) => {
                let old_frame = self.env;
                let mut arg_frame = HashMap::new();
                let (names, exprs) = vec::unzip(b);
                for names.each |&name| {
                    arg_frame.insert(name, @LExt(RUndef));
                }
                self.env = @mut push(old_frame, arg_frame);

                let mut res:Result<@RDatum, RuntimeError> = Err(NilEval);
                match result::map_vec(exprs, |&expr| { self.eval(expr) }) {
                    Ok(vals) => {
                        do self.env.mut_top |frame| {
                            for uint::range(0, names.len()) |i| {
                                frame.insert(names[i], vals[i]);
                            }
                        };

                        do body.each |&val| {
                            res = self.eval(val);
                            res.is_ok()
                        };
                    },
                    Err(e) => {
                        res = Err(e);
                    },
                }

                self.env = old_frame;
                res
            }
        }
    }

    fn cond(&mut self, conds: &[@RDatum]) -> Result<@RDatum, RuntimeError>
    {
        let mut i = 0u;
        let mut exprs = vec::with_capacity(conds.len());
        let mut else_opt = None;

        while i < conds.len() {
            match conds[i].to_list() {
                Some([@LIdent(els), expr]) if els.as_slice() == "else" => 
                    if i == conds.len()-1 {
                        else_opt = Some(expr);
                    } else {
                        return Err(BadSyntax(SynCond, ~"trailing conditions after else"));
                    },
                Some([pred, expr]) => exprs.push((pred, expr)),
                _ => return Err(BadSyntax(SynCond, ~"invalid conditional expression")),
            }
            i += 1;
        }

        let mut res = Ok(@LExt(RBot));

        let expr_end = do exprs.each |&(pred, expr)| {
            match self.eval(pred) {
                Err(e) => {
                    res = Err(e);
                    false
                },
                Ok(@LBool(false)) => true,
                _ => {
                    res = self.eval(expr);
                    false
                },
            }
        };

        match else_opt {
            Some(else_expr) if expr_end => self.eval(else_expr),
            _ => res
        }
    }

    fn define(&mut self, args: ~[@RDatum]) -> Result<(@str, @RDatum), RuntimeError> {
        match get_syms(&args[0]) {
            Err(e) => Err(BadSyntax(SynDefine, e)),
            Ok((anames, varargs)) =>
                if anames.is_empty() {
                    match varargs {
                        None => Err(BadSyntax(SynDefine, ~"name not given")),
                        Some(name) => if args.len() != 2 {
                                Err(BadSyntax(SynDefine, ~"multiple expressions"))
                            } else {
                                do self.eval(args[1]).map |&val| {
                                    (name, val)
                                }
                            }
                    }
                } else {
                    let name = anames[0];
                    let anames = anames.slice(1, anames.len()).to_owned();
                    let seq = args.slice(1, args.len()).to_owned();
                    let proc = @LExt(RProc(anames, varargs, seq, self.env));
                    Ok((name, proc))
                }
        }
    }

    fn run_syntax(&mut self,
                syn: PrimSyntax,
                args: ~[@RDatum]) -> Result<@RDatum, RuntimeError>
    {
        match syn {
            SynIf => if args.len() == 3 {
                    do self.eval(args[0]).chain |cond| {
                        match *cond {
                            LBool(false) => self.eval(args[2]),
                            _ => self.eval(args[1]),
                        }
                    }
                } else {
                    Err(BadSyntax(SynIf, ~"bad number of arguments"))
                },
            SynCond => self.cond(args),
            SynLambda => if args.len() < 2 {
                    Err(BadSyntax(SynLambda, ~"no body given"))
                } else {
                    match get_syms(&args[0]) {
                        Err(e) => Err(BadSyntax(SynLambda, e)),
                        Ok((anames, varargs)) => {
                            let seq = args.slice(1, args.len()).to_owned();
                            Ok(@LExt(RProc(anames, varargs, seq, self.env)))
                        },
                    }
                },
            SynLet => if args.len() < 2 {
                    Err(BadSyntax(SynLet, ~"no body given"))
                } else {
                    self.syn_let(args[0], args.slice(1, args.len()))
                },
            SynLetRec => if args.len() < 2 {
                    Err(BadSyntax(SynLetRec, ~"no body given"))
                } else {
                    self.syn_letrec(args[0], args.slice(1, args.len()))
                },
            SynLetStar => if args.len() < 2 {
                    Err(BadSyntax(SynLetRec, ~"no body given"))
                } else {
                    self.syn_letstar(args[0], args.slice(1, args.len()))
                },
            SynDefine => if args.len() < 2 {
                    Err(BadSyntax(SynDefine, ~"no body given"))
                } else {
                    let definition = self.define(args);
                    match definition {
                        Err(e) => Err(e),
                        Ok((name, val)) => {
                            if self.env.size_hint() == Some(0) {
                                // this is the top-level context
                                // just bind the definition in global
                                self.global.insert(name, Left(val));
                            } else {
                                // this is not the top-level context
                                // create a new frame
                                let mut frame = HashMap::new();
                                frame.insert(name, val);
                                self.env = @mut push(self.env, frame);
                            };
                            Ok(@LNil)
                        },
                    }
                },
            SynSet => if args.len() != 2 {
                    Err(BadSyntax(SynSet, ~"bad number of arguments"))
                } else {
                    match *args[0] {
                        LIdent(name) => do self.eval(args[1]).chain |val| {
                            if set_var(self.env, &name, val) {
                                Ok(@LNil)
                            } else {
                                Err(BadSyntax(SynSet, ~"unbound variable"))
                            }
                        },
                        _ => Err(BadSyntax(SynSet, ~"cannot set non-variable"))
                    }
                },
            SynQuote => if args.len() == 1 {
                    Ok(args[0])
                } else {
                    Err(BadSyntax(SynQuote, ~"bad number of arguments"))
                },
            SynQQuote => if args.len() == 1 {
                    self.quasiquote(&args[0])
                } else {
                    Err(BadSyntax(SynQQuote, ~"bad number of arguments"))
                },
            SynUnquote => if args.len() == 1 {
                    self.unquote(&args[0])
                } else {
                    Err(BadSyntax(SynUnquote, ~"bad number of arguments"))
                },
            SynAnd => self.syn_and(args),
            SynOr => self.syn_or(args),
        }
    }

    priv fn syn_and(&mut self, args: &[@RDatum]) -> Result<@RDatum, RuntimeError>
    {
        let mut res = @LBool(true);
        let mut i = 0u;
        while i < args.len() {
            match self.eval(args[i]) {
                Ok(@LBool(false)) => return Ok(@LBool(false)),
                Ok(x) => { res = x },
                Err(e) => return Err(e),
            };
            i += 1;
        }
        return Ok(res)
    }

    priv fn syn_or(&mut self, args: &[@RDatum]) -> Result<@RDatum, RuntimeError>
    {
        let mut i = 0u;
        while i < args.len() {
            match self.eval(args[i]) {
                Ok(@LBool(false)) => (),
                Ok(x) => return Ok(x),
                Err(e) => return Err(e),
            };
            i += 1;
        }
        return Ok(@LBool(false))
    }

    fn call_proc(&mut self,
                anames: &[@str],
                vargs: Option<@str>,
                code: &[@RDatum],
                frame: @mut Stack<HashMap<@str, @RDatum>>,
                args: &[@RDatum]) -> Result<@RDatum, RuntimeError>
    {
        // create new frame to store args
        let mut arg_frame = HashMap::new();

        match vargs {
            None => if args.len() != anames.len() {
                    return Err(ArgNumError(anames.len(), Some(anames.len()), args.len()));
                },
            Some(vname) => if args.len() < anames.len() {
                    return Err(ArgNumError(anames.len(), None, args.len()));
                } else {
                    let vslice = args.slice(anames.len(), args.len());
                    let va = do vslice.rev_iter().fold(@LNil) |a, &l| {
                        @LCons(l, a)
                    };
                    arg_frame.insert(vname, va);
                },
        }

        for uint::range(0, anames.len()) |i| {
            arg_frame.insert(anames[i], args[i]);
        }

        self.local_eval(arg_frame, frame, code)
    }

    fn local_eval(&mut self,
                arg_frame: HashMap<@str, @RDatum>,
                frame: @mut Stack<HashMap<@str, @RDatum>>,
                code: &[@RDatum])
        -> Result<@RDatum, RuntimeError>
    {
        // store current env
        let old_env = self.env;

        // create new local env
        self.env = @mut push(frame, arg_frame);
        let mut res:Result<@RDatum, RuntimeError> = Err(NilEval);
        do code.each() |&val| {
            res = self.eval(val);
            res.is_ok()
        };

        // restore env
        self.env = old_env;

        res
    }

    fn call_prim(&mut self,
                f: PFunc,
                args: &[@RDatum]) -> Result<@RDatum, RuntimeError>
    {
        match f {
            PEval => match args {
                [arg] => self.eval(arg),
                _ => Err(ArgNumError(1, Some(1), args.len())),
            },
            PApply => do call_err2::<RuntimeData, ~[@RDatum], @RDatum>(args) |f, &l| {
                self.apply(f, l)
            },
            PMap => match args {
                [] => Err(ArgNumError(2, None, 0)),
                [@LExt(ref f), ..list] => do transpose(list).chain |trans| {
                    result::map_vec(trans, |&l| { self.apply(f, l) }).map(|&res| {
                        LDatum::from_list(res)
                    })
                },
                _ => Err(TypeError),
            },
            PForEach => match args {
                [] => Err(ArgNumError(2, None, 0)),
                [@LExt(ref f), ..list] => do transpose(list).chain |trans| {
                    result::map_vec(trans, |&l| { self.apply(f, l) }).map(|_| {
                        @LExt(RBot)
                    })
                },
                _ => Err(TypeError),
            },
            PBegin => if args.len() == 0 {
                    Ok(@LExt(RBot))
                } else {
                    Ok(*args.last())
                },
            PAdd => do call_foldl::<LNumeric>(args, &Zero::zero()) |&lhs, &rhs| { Ok(lhs + rhs) },
            PSub => match args {
                [] => Err(ArgNumError(1, None, 0)),
                [@LNum(ref x)] => Ok(@LNum(-*x)),
                [@LNum(ref x), ..tail] =>
                    do call_foldl::<LNumeric>(tail, x) |&lhs, &rhs| { Ok(lhs - rhs) },
                _ => Err(TypeError),
            },
            PMul => do call_foldl::<LNumeric>(args, &One::one()) |&lhs, &rhs| { Ok(lhs * rhs) },
            PDiv => match args {
                [] => Err(ArgNumError(1, None, 0)),
                [@LNum(ref x)] => if x.is_zero() {
                        Err(DivideByZeroError)
                    } else {
                        Ok(@LNum(x.recip()))
                    },
                [@LNum(ref x), ..tail] =>
                    do call_foldl::<LNumeric>(tail, x) |&lhs, &rhs| {
                        if rhs.is_zero() {
                            Err(DivideByZeroError)
                        } else {
                            Ok(lhs / rhs)
                        }
                    },
                _ => Err(TypeError),
            },
            PQuotient => do call_err2::<BigInt, BigInt, BigInt>(args) |&lhs, &rhs| {
                if rhs.is_zero() {
                    Err(DivideByZeroError)
                } else {
                    Ok(lhs / rhs)
                }
            },
            PRemainder => do call_err2::<BigInt, BigInt, BigInt>(args) |&lhs, &rhs| {
                if rhs.is_zero() {
                    Err(DivideByZeroError)
                } else {
                    Ok(lhs % rhs)
                }
            },
            PModulo => do call_err2::<BigInt, BigInt, BigInt>(args) |&lhs, &rhs| {
                if rhs.is_zero() {
                    Err(DivideByZeroError)
                } else {
                    Ok(modulo(lhs, rhs))
                }
            },
            PFloor => do call_tc1::<LReal, LReal>(args) |&x| { x.floor() },
            PCeiling => do call_tc1::<LReal, LReal>(args) |&x| { x.ceil() },
            PRound => do call_tc1::<LReal, LReal>(args) |&x| { x.round() },
            PTruncate => do call_tc1::<LReal, LReal>(args) |&x| { x.trunc() },
            PExp => do call_tc1::<LNumeric, LNumeric>(args) |&x| { x.exp() },
            PLog => do call_tc1::<LNumeric, LNumeric>(args) |&x| { x.ln() },
            PSin => do call_tc1::<LNumeric, LNumeric>(args) |&x| { x.sin() },
            PCos => do call_tc1::<LNumeric, LNumeric>(args) |&x| { x.cos() },
            PTan => do call_tc1::<LNumeric, LNumeric>(args) |&x| { x.tan() },
            PAsin => do call_tc1::<LNumeric, LNumeric>(args) |&x| { x.asin() },
            PAcos => do call_tc1::<LNumeric, LNumeric>(args) |&x| { x.acos() },
            PAtan => do call_tc1::<LNumeric, LNumeric>(args) |&x| { x.atan() },
            PSqrt => do call_tc1::<LNumeric, LNumeric>(args) |&x| { x.sqrt() },
            PExpt => do call_tc2::<LNumeric, LNumeric, LNumeric>(args) |x, r| { x.pow(r) },
            PMakeRectangular => do call_tc2::<LReal, LReal, LNumeric>(args) |rx, ry| {
                coerce_real(rx, ry, |&a, &b| { exact(Rational::from_bigint(a),
                                                    Rational::from_bigint(b)) },
                                    |&a, &b| { exact(a, b) },
                                    |&a, &b| { inexact(a, b) })
            },
            PMakePolar => do call_tc2::<LReal, LReal, LNumeric>(args) |rx, ry| {
                polar(rx.to_f64(), ry.to_f64())
            },
            PRealPart => do call_err1::<LNumeric, LReal>(args) |&x|  {
                match x {
                    NExact( Cmplx { re: ref re, im: _ } ) => Ok(LReal::from_rational(re.clone())),
                    NInexact( Cmplx { re: re, im: _ } ) => Ok(Rf64(re)),
                    _ => Err(TypeError),
                }
            },
            PImagPart => do call_err1::<LNumeric, LReal>(args) |&x|  {
                match x {
                    NExact( Cmplx { re: _, im: ref im } ) => Ok(LReal::from_rational(im.clone())),
                    NInexact( Cmplx { re: _, im: im } ) => Ok(Rf64(im)),
                    _ => Err(TypeError)
                }
            },
            PMagnitude => do call_tc1::<LNumeric, f64>(args) |x| {
                x.to_icmplx().norm()
            },
            PAngle => do call_tc1::<LNumeric, f64>(args) |x| {
                x.to_icmplx().arg()
            },
            PNumerator => do call_tc1::<Rational, BigInt>(args) |x| { x.numerator().clone() },
            PDenominator => do call_tc1::<Rational, BigInt>(args) |x| { x.denominator().clone() },
            PCar => do call_tc1::<(@RDatum, @RDatum), @RDatum>(args) |&(h, _)| { h },
            PCdr => do call_tc1::<(@RDatum, @RDatum), @RDatum>(args) |&(_, t)| { t },
            PCons => match args {
                [arg0, arg1] => Ok(@LCons(arg0, arg1)),
                _ => Err(ArgNumError(2, Some(2), args.len())),
            },
            PEqv => do call_tc2::<@RDatum, @RDatum, bool>(args) |&arg1, &arg2| {
                match (arg1, arg2) {
                    (@LCons(_, _), @LCons(_, _)) => managed::ptr_eq(arg1, arg2),
                    (@LString(_), @LString(_)) => managed::ptr_eq(arg1, arg2),
                    (@LExt(_), @LExt(_)) => managed::ptr_eq(arg1, arg2),
                    _ => arg1 == arg2,
                }
            },
            PEqual => do call_tc2::<@RDatum, @RDatum, bool>(args) |&a, &b| { a == b },
            PNumber => typecheck::<LNumeric>(args),
            PReal => typecheck::<LReal>(args),
            PInteger => typecheck::<BigInt>(args),
            PExact => typecheck::<Cmplx<Rational>>(args),
            PInexact => typecheck::<Cmplx<f64>>(args),
            PExactInexact => do call_tc1::<LNumeric, LNumeric>(args) |x| { x.to_inexact() },
            PInexactExact => do call_tc1::<LNumeric, LNumeric>(args) |x| { x.to_exact() },
            PNumberString => match args.len() {
                1 => do call_tc1::<LNumeric, ~str>(args) |x| { x.to_str() },
                2 => do call_err2::<LNumeric, uint, ~str>(args) |&x, &radix| {
                        match x {
                            NReal(Rf64(ref f)) => if radix == 10 {
                                    Ok(f.to_str())
                                } else {
                                    Err(PrimitiveError(~"number->string is not allowed\
                                    for inexact number with radix"))
                                },
                            NReal(ref n) => Ok(n.to_str_radix(radix)),
                            NExact(ref n) => Ok(n.to_str_radix(radix)),
                            _ => if radix == 10 {
                                Ok(x.to_str())
                            } else {
                                Err(PrimitiveError(~"number->string is not allowed\
                                for inexact number with radix"))
                            },
                        }
                },
                n => Err(ArgNumError(1, Some(2), n))
            },
            PMin => do call_tc2::<LReal, LReal, LReal>(args) |x, y| { x.min(y) },
            PMax => do call_tc2::<LReal, LReal, LReal>(args) |x, y| { x.max(y) },
            PEQ => do call_bfoldl::<LNumeric>(args) |&lhs, &rhs| { lhs == rhs },
            PGT => do call_bfoldl::<LReal>(args) |&lhs, &rhs| { lhs > rhs },
            PLT => do call_bfoldl::<LReal>(args) |&lhs, &rhs| { lhs < rhs },
            PGE => do call_bfoldl::<LReal>(args) |&lhs, &rhs| { lhs >= rhs },
            PLE => do call_bfoldl::<LReal>(args) |&lhs, &rhs| { lhs <= rhs },
            PZero => do call_tc1::<LNumeric, bool>(args) |&n| { n.is_zero() },
            PPositive => do call_tc1::<LReal, bool>(args) |&n| { n.is_positive() },
            PNegative => do call_tc1::<LReal, bool>(args) |&n| { n.is_negative() },
            PNot => do call_tc1::<@RDatum, bool>(args) |&arg| {
                match arg {
                    @LBool(false) => true,
                    _ => false,
                }
            },
            PBoolean => typecheck::<bool>(args),
            PChar => typecheck::<char>(args),
            PCharAlphabetic => do call_tc1::<char, bool>(args) |&c| { char::is_alphabetic(c) },
            PCharNumeric => do call_tc1::<char, bool>(args) |&c| { char::is_digit(c) },
            PCharWhitespace => do call_tc1::<char, bool>(args) |&c| { char::is_whitespace(c) },
            PCharUpperCase => do call_tc1::<char, bool>(args) |&c| { char::is_uppercase(c) },
            PCharLowerCase => do call_tc1::<char, bool>(args) |&c| { char::is_lowercase(c) },
            PCharEQ => do call_bfoldl::<char>(args) |&lhs, &rhs| { lhs == rhs },
            PCharGT => do call_bfoldl::<char>(args) |&lhs, &rhs| { lhs > rhs },
            PCharLT => do call_bfoldl::<char>(args) |&lhs, &rhs| { lhs < rhs },
            PCharGE => do call_bfoldl::<char>(args) |&lhs, &rhs| { lhs >= rhs },
            PCharLE => do call_bfoldl::<char>(args) |&lhs, &rhs| { lhs <= rhs },
            PCharCIEQ => do call_bfoldl::<char>(args) |&lhs, &rhs| {
                do ci_cmp(lhs, rhs) |a, b| { a == b }
            },
            PCharCIGT => do call_bfoldl::<char>(args) |&lhs, &rhs| {
                do ci_cmp(lhs, rhs) |a, b| { a > b }
            },
            PCharCILT => do call_bfoldl::<char>(args) |&lhs, &rhs| {
                do ci_cmp(lhs, rhs) |a, b| { a < b }
            },
            PCharCIGE => do call_bfoldl::<char>(args) |&lhs, &rhs| {
                do ci_cmp(lhs, rhs) |a, b| { a >= b }
            },
            PCharCILE => do call_bfoldl::<char>(args) |&lhs, &rhs| {
                do ci_cmp(lhs, rhs) |a, b| { a <= b }
            },
            PCharInteger => do call_tc1::<char, uint>(args) |&c| {
                c.to_ascii().to_byte() as uint
            },
            PIntegerChar => do call_err1::<uint, char>(args) |&n| {
                if n <= 0xff { Ok(n as char) } else { Err(TypeError) }
            },
            PCharUpcase => do call_tc1::<char, char>(args) |&c| {
                c.to_ascii().to_upper().to_byte() as char
            },
            PCharDowncase => do call_tc1::<char, char>(args) |&c| {
                c.to_ascii().to_lower().to_byte() as char
            },
            PProcedure => match args {
                [@LExt(RPrim(_))] => Ok(@LBool(true)),
                [@LExt(RProc(_, _, _, _))] => Ok(@LBool(true)),
                [_] => Ok(@LBool(false)),
                _ => Err(ArgNumError(1, Some(1), args.len())),
            },
            PIsVector => match args {
                [@LVector(_)] => Ok(@LBool(true)),
                [_] => Ok(@LBool(false)),
                _ => Err(ArgNumError(1, Some(1), args.len())),
            },
            PMakeVector => match args {
                [@LNum(ref x)] => match get_uint(x) {
                    Some(k) => {
                        let mut v = ~[];
                        v.grow(k, &@LExt(RUndef));
                        Ok(@LVector(@mut v))
                    },
                    None => Err(TypeError),
                },
                [@LNum(ref x), ref y] => match get_uint(x) {
                    Some(k) => {
                        let mut v = ~[];
                        v.grow(k, y);
                        Ok(@LVector(@mut v))
                    },
                    None => Err(TypeError),
                },
                [_] | [_, _] => Err(TypeError),
                _ => Err(ArgNumError(1, Some(2), args.len())),
            },
            PVector => Ok(@LVector(@mut args.to_owned())),
            PVectorLength => do call_tc1::<@mut ~[@RDatum], uint>(args) |v| { v.len() },
            PVectorRef => do call_err2::<@mut ~[@RDatum], uint, @RDatum>(args) |v, &idx| {
                if idx < v.len() { Ok(v[idx]) } else { Err(RangeError) }
            },
            PVectorSet => do call_err3::<@mut ~[@RDatum], uint, @RDatum, ()>(args) |v, &idx, &x| {
                if idx < v.len() { Ok(v[idx] = x) } else { Err(RangeError) }
            },
            PVectorFill => do call_tc2::<@mut ~[@RDatum], @RDatum, ()>(args) |v, &x| {
                let mut i = 0u;
                while i < v.len() {
                    v[i] = x;
                    i += 1;
                }
            },
            PVectorList => do call_tc1::<@mut ~[@RDatum], @RDatum>(args) |&v| {
                LDatum::from_list(*v)
            },
            PListVector => do call_err1::<@RDatum, @mut ~[@RDatum]>(args) |&l| {
                match l.to_list() {
                    Some(v) => Ok(@mut v),
                    None => Err(TypeError),
                }
            },
            PNull => match args {
                [@LNil] => Ok(@LBool(true)),
                [_] => Ok(@LBool(false)),
                _ => Err(ArgNumError(1, Some(1), args.len())),
            },
            PPair => typecheck::<(@RDatum, @RDatum)>(args),
            PIsString => typecheck::<~str>(args),
            PStringEQ => do call_bfoldl::<~str>(args) |&lhs, &rhs| { lhs == rhs },
            PStringGT => do call_bfoldl::<~str>(args) |&lhs, &rhs| { lhs > rhs },
            PStringLT => do call_bfoldl::<~str>(args) |&lhs, &rhs| { lhs < rhs },
            PStringGE => do call_bfoldl::<~str>(args) |&lhs, &rhs| { lhs >= rhs },
            PStringLE => do call_bfoldl::<~str>(args) |&lhs, &rhs| { lhs <= rhs },
            PStringCIEQ => do call_bfoldl::<~str>(args) |&lhs, &rhs| {
                do str_ci_cmp(lhs, rhs) |a, b| { a == b }
            },
            PStringCIGT => do call_bfoldl::<~str>(args) |&lhs, &rhs| {
                do str_ci_cmp(lhs, rhs) |a, b| { a > b }
            },
            PStringCILT => do call_bfoldl::<~str>(args) |&lhs, &rhs| {
                do str_ci_cmp(lhs, rhs) |a, b| { a < b }
            },
            PStringCIGE => do call_bfoldl::<~str>(args) |&lhs, &rhs| {
                do str_ci_cmp(lhs, rhs) |a, b| { a >= b }
            },
            PStringCILE => do call_bfoldl::<~str>(args) |&lhs, &rhs| {
                do str_ci_cmp(lhs, rhs) |a, b| { a <= b }
            },
            PString => do call_vargs::<char, ~str>(args) |chars| { str::from_chars(chars) },
            PStringLength => do call_tc1::<~str, uint>(args) |s| { s.len() },
            PStringRef => do call_err2::<~str, uint, char>(args) |s, &idx| {
                if idx <= s.len() {
                    Ok(s.char_at(idx))
                } else {
                    Err(RangeError)
                }
            },
            PSubstring => match args.len() {
                2 => do call_err2::<~str, uint, ~str>(args) |s, &start| {
                    if start <= s.len() {
                        Ok(s.slice(start, s.len()).to_owned())
                    } else {
                        Err(RangeError)
                    }
                },
                3 => do call_err3::<~str, uint, uint, ~str>(args) |s, &start, &end| {
                    if start <= end && end <= s.len() {
                        Ok(s.slice(start, end).to_owned())
                    } else {
                        Err(RangeError)
                    }
                },
                n => Err(ArgNumError(2, Some(3), n)),
            },
            PStringAppend => do call_vargs::<~str, ~str>(args) |strs| { strs.concat() },
            PStringList => do call_tc1::<~str, ~[char]>(args) |&s| {
                s.iter().collect()
            },
            PListString => do call_tc1::<~[char], ~str>(args) |&cs| {
                str::from_chars(cs)
            },
            PSymbol => typecheck::<@str>(args),
            PSymbolString => do call_tc1::<@str, ~str>(args) |&s| { s.to_owned() },
            PStringSymbol => do call_tc1::<~str, @str>(args) |&s| { s.to_managed() },
            PInputPort => typecheck::<@Reader>(args),
            POutputPort => typecheck::<@Writer>(args),
            PCurrentInputPort => if args.len() == 0 {
                    Ok(@LExt(RInputPort(self.stdin)))
                } else {
                    Err(ArgNumError(0, Some(0), args.len()))
                },
            PCurrentOutputPort => if args.len() == 0 {
                    Ok(@LExt(ROutputPort(self.stdout)))
                } else {
                    Err(ArgNumError(0, Some(0), args.len()))
                },
            POpenInputFile => do call_err1::<~str, @Reader>(args) |&s| {
                match io::file_reader(&Path(s)) {
                    Ok(rdr) => Ok(rdr),
                    Err(e) => Err(IOError(e)),
                }
            },
            POpenOutputFile => do call_err1::<~str, @Writer>(args) |&s| {
                match io::file_writer(&Path(s), []) {
                    Ok(wr) => Ok(wr),
                    Err(e) => Err(IOError(e)),
                }
            },
            PCloseInputPort => do call_tc1::<@Reader, ()>(args) |&_| { () },
            PCloseOutputPort => do call_tc1::<@Writer, ()>(args) |&_| { () },
            PRead => match args.len() {
                0 => read(self.stdin),
                1 => do call_err1::<@Reader, @RDatum>(args) |&rdr| { read(rdr) },
                n => Err(ArgNumError(0, Some(1), n)),
            },
            PReadChar => match args.len() {
                0 => Ok(@LChar(self.stdin.read_char())),
                1 => do call_tc1::<@Reader, char>(args) |&rdr| { rdr.read_char() },
                n => Err(ArgNumError(0, Some(1), n)),
            },
            PWrite => match args.len() {
                1 => do call_tc1::<@RDatum, ()>(args) |&x| { x.write(self.stdout) },
                2 => do call_tc2::<@RDatum, @Writer, ()>(args) |&x, &wr| { x.write(wr) },
                n => Err(ArgNumError(1, Some(2), n)),
            },
            PDisplay => match args.len() {
                1 => do call_tc1::<@RDatum, ()>(args) |&x| { x.write_raw(self.stdout) },
                2 => do call_tc2::<@RDatum, @Writer, ()>(args) |&x, &wr| { x.write_raw(wr) },
                n => Err(ArgNumError(1, Some(2), n)),
            },
            PNewline => match args.len() {
                0 => {
                    self.stdout.write_char('\n');
                    Ok(@LNil)
                },
                1 => do call_tc1::<@Writer, ()>(args) |&wr| { wr.write_char('\n') },
                n => Err(ArgNumError(0, Some(1), n)),
            },
            PWriteChar => match args.len() {
                1 => do call_tc1::<char, ()>(args) |&c| { self.stdout.write_char(c) },
                2 => do call_tc2::<char, @Writer, ()>(args) |&c, &wr| { wr.write_char(c) },
                n => Err(ArgNumError(1, Some(2), n)),
            },
            PLoad => do call_err1::<~str, @RDatum>(args) |&name| { 
                match io::file_reader(&Path(name)) {
                    Ok(rdr) => self.load(rdr),
                    Err(e) => Err(IOError(e)),
                }
            },
        }
    }

    fn recursive_qq(&mut self, val: &@RDatum) -> Result<@RDatum, RuntimeError> {
        match *val {
            @LCons(ref h, ref t) =>
                match is_quote(h,t) {
                    Some((QuasiQuote, ref v)) => 
                        do self.quasiquote(v).map |&qv| {
                            @LCons(@LIdent(@"quasiquote"), @LCons(qv, @LNil))
                        },
                    Some((Unquote, ref v)) => 
                        self.unquote(v),
                    _ =>
                        do self.recursive_qq(h).chain |qh| {
                            do self.recursive_qq(t).map |&qt| {
                                @LCons(qh, qt)
                            }
                        },
                },
            @LVector(ref v) => {
                match result::map_vec(**v, |x| { self.recursive_qq(x) }) {
                    Ok(qmap) => Ok(@LVector(@mut qmap)),
                    Err(e) => Err(e),
                }
            },
            _ => 
                Ok(*val),
        }
    }

    fn quasiquote(&mut self, val: &@RDatum) -> Result<@RDatum, RuntimeError> {
        self.qq_lvl += 1;
        let res = self.recursive_qq(val);
        self.qq_lvl -= 1;
        res
    }

    fn unquote(&mut self, val: &@RDatum) -> Result<@RDatum, RuntimeError> {
        if self.qq_lvl == 0 {
            Err(BadSyntax(SynUnquote, ~"unquote not nested in quasiquote"))
        } else {
            self.qq_lvl -= 1;
            let res =
            if self.qq_lvl == 0 {
                self.eval(*val)
            } else {
                do self.recursive_qq(val).map |&qval| {
                    @LCons(@LIdent(@"unquote"), @LCons(qval, @LNil))
                }
            };
            self.qq_lvl += 1;
            res
        }
    }

    fn apply(&mut self, proc: &RuntimeData, args: &[@RDatum]) -> Result<@RDatum, RuntimeError> {
        match proc {
            &RPrim(f) =>
                self.call_prim(f, args),
            &RProc(ref anames, ref vargs, ref code, ref env) =>
                self.call_proc(*anames, *vargs, *code, *env, args),
            _ =>
                Err(NotCallable),
        }
    }

    fn call(&mut self, proc: &RuntimeData, aexprs: ~[@RDatum]) -> Result<@RDatum, RuntimeError> {
        match result::map_vec(aexprs, |&expr| self.eval(expr))
        {
            Ok(args) => self.apply(proc, args),
            Err(e) => Err(e),
        }
    }

    pub fn new_std() -> Runtime {
        Runtime {
            stdin: io::stdin(),
            stdout: io::stdout(),
            stderr: io::stderr(),
            env: @mut Stack::new(),
            global: load_prelude(),
            qq_lvl: 0,
        }
    }

    pub fn eval(&mut self, val: @RDatum) -> Result<@RDatum, RuntimeError>
    {
        match *val {
            LIdent(name) => self.find_var(&name),
            LCons(fexpr, aexpr) =>
                match aexpr.to_list() {
                    None => Err(NotList),
                    Some(aexprs) => {
                        match self.get_syntax(fexpr) {
                            Some(syntax) =>
                                self.run_syntax(syntax, aexprs),
                            None =>
                                match self.eval(fexpr) {
                                    Ok(@LExt(ref proc)) => self.call(proc, aexprs),
                                    Ok(_) => Err(NotCallable),
                                    Err(e) => Err(e),
                                },
                        }
                    },
                },
            LNil => Err(NilEval),
            _ => Ok(val),
        }
    }

    pub fn load(&mut self, rdr: @io::Reader) -> Result<@RDatum, RuntimeError>
    {
        do read(rdr).chain() |datum| {
            self.eval(datum)
        }
    }
}

priv fn set_var(env: @mut Stack<HashMap<@str, @RDatum>>,
                name: &@str,
                val: @RDatum) -> bool {
    let mut success = false;

    do env.each_mut |frame| {
        match frame.find_mut(name) {
            None => (),
            Some(v) => {
                success = true;
                *v = val;
            }
        }
        !success
    };

    success
}
