use std::io;
use std::borrow;
use std::uint;
use std::result;
use std::str;
use std::vec;
use std::num::{One, Zero, ToStrRadix};
use std::hashmap::HashMap;
use std::managed;
use bounded_iterator::BoundedIterator;
use extra::bigint::BigInt;
use extra::complex::Cmplx;
use datum::*;
use primitive::*;
use numeric::*;
use rational::Rational;
use stack::*;
use parser::Parser;

enum RuntimeData {
    RUndef,
    RPrim(PFunc),
    RProc(~[@str],
        Option<@str>,
        ~[@LDatum<RuntimeData>],
        @mut Stack<HashMap<@str, @LDatum<RuntimeData>>>),
}

impl Clone for RuntimeData {
    fn clone(&self) -> RuntimeData {
        match self {
            &RUndef => RUndef,
            &RPrim(f) => RPrim(f),
            &RProc(ref args, ref vargs, ref body, ref env) => {
                let cloneargs = do args.map |&arg| {
                    arg
                };
                RProc(cloneargs, *vargs, body.clone(), *env)
            },
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
        RUndef => ~"<undefined>",
        RPrim(f) => fmt!("<primitive:%s>", f.to_str()),
        RProc(_, _, _, _) => fmt!("<procedure 0x%08x>", borrow::to_uint(data)),
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
    fn to_datum(&self) -> @RDatum;
    fn typename() -> ~str;
}

impl DatumConv for @RDatum {
    fn from_datum<R>(datum: @RDatum, op: &fn(&@RDatum) -> R) -> Option<R> {
        Some(op(&datum))
    }

    fn to_datum(&self) -> @RDatum {
        *self
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

    fn to_datum(&self) -> @RDatum {
        @LExt(self.clone())
    }

    fn typename() -> ~str {
        ~"procedure"
    }
}

impl DatumConv for LNumeric {
    fn from_datum<R>(datum: @RDatum, op: &fn(&LNumeric) -> R) -> Option<R> {
        match datum {
            @LNum(ref n) => Some(op(n)),
            _ => None,
        }
    }

    fn to_datum(&self) -> @RDatum {
        @LNum(self.clone())
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

    fn to_datum(&self) -> @RDatum {
        @LNum(NInexact(*self))
    }

    fn typename() -> ~str {
        ~"inexact number"
    }
}

impl DatumConv for Cmplx<Rational> {
    fn from_datum<R>(datum: @RDatum, op: &fn(&Cmplx<Rational>) -> R) -> Option<R> {
        match datum {
            @LNum(NExact(ref n)) => Some(op(n)),
            _ => None,
        }
    }

    fn to_datum(&self) -> @RDatum {
        @LNum(NExact(self.clone()))
    }

    fn typename() -> ~str {
        ~"exact number"
    }
}

impl DatumConv for LReal {
    fn from_datum<R>(datum: @RDatum, op: &fn(&LReal) -> R) -> Option<R> {
        match datum {
            @LNum(ref n) => match get_real(n) {
                Some(ref r) => Some(op(r)),
                None => None,
            },
            _ => None,
        }
    }

    fn to_datum(&self) -> @RDatum {
        @LNum(from_real(self))
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

    fn to_datum(&self) -> @RDatum {
        @LNum(from_rational(self))
    }

    fn typename() -> ~str {
        ~"rational number"
    }
}

impl DatumConv for BigInt {
    fn from_datum<R>(datum: @RDatum, op: &fn(&BigInt) -> R) -> Option<R> {
        match datum {
            @LNum(ref n) => match *n {
                NExact( Cmplx{ re: ref re, im: ref im } ) =>
                    if im.is_zero() && *re.numerator() == One::one() {
                        Some(op(re.denominator()))
                    } else {
                        None
                    },
                NInexact(_) => None,
            },
            _ => None,
        }
    }

    fn to_datum(&self) -> @RDatum {
        @LNum(from_bigint(self.clone()))
    }

    fn typename() -> ~str {
        ~"integer"
    }
}

impl DatumConv for (@RDatum, @RDatum) {
    fn from_datum<R>(datum: @RDatum, op: &fn(&(@RDatum, @RDatum)) -> R) -> Option<R> {
        match datum {
            @LCons(a, b) => Some(op(&(a, b))),
            _ => None,
        }
    }

    fn to_datum(&self) -> @RDatum {
        @LCons(self.first(), self.second())
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

    fn to_datum(&self) -> @RDatum {
        @LBool(*self)
    }

    fn typename() -> ~str {
        ~"boolean"
    }
}

impl DatumConv for () {
    fn from_datum<R>(datum: @RDatum, op: &fn(&()) -> R) -> Option<R> {
        match datum {
            @LNil => Some(op(&())),
            _ => None,
        }
    }

    fn to_datum(&self) -> @RDatum {
        @LNil
    }

    fn typename() -> ~str {
        ~"()"
    }
}

struct GetList {
    list: ~[@RDatum]
}

impl DatumConv for GetList {
    #[inline]
    fn from_datum<R>(datum: @RDatum, op: &fn(&GetList) -> R) -> Option<R> {
        match datum.to_list() {
            Some(l) => Some(op(&GetList{ list: l })),
            _ => None,
        }
    }

    #[inline]
    fn to_datum(&self) -> @RDatum {
        LDatum::from_list(self.list)
    }

    fn typename() -> ~str {
        ~"list"
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
    }
}

fn load_prelude() -> HashMap<@str, Either<@RDatum, PrimSyntax>> {
    let mut map = HashMap::new();
    let mut prim_iter = BoundedIterator::new::<PFunc>();
    for prim_iter.advance |prim:PFunc| {
        let key = prim.to_str();
        map.insert(key.to_managed(), Left(@LExt(RPrim(prim))));
    }

    let mut syntax_iter = BoundedIterator::new::<PrimSyntax>();
    for syntax_iter.advance |syntax:PrimSyntax| {
        let key = syntax.to_str();
        map.insert(key.to_managed(), Right(syntax));
    }

    map.insert("pi".to_managed(), Left(@LNum(inexact(Real::pi(), 0f64))));
    map
}

priv fn call_prim1(args: &[@RDatum],
                op: &fn(@RDatum) -> Result<@RDatum, RuntimeError>)
    -> Result<@RDatum, RuntimeError>
{
    if args.len() == 1 {
        op(args[0])
    } else {
        Err(ArgNumError(1, Some(1), args.len()))
    }
}

priv fn call_prim2(args: &[@RDatum],
                op: &fn(@RDatum, @RDatum) -> Result<@RDatum, RuntimeError>)
    -> Result<@RDatum, RuntimeError>
{
    if args.len() == 2 {
        op(args[0], args[1])
    } else {
        Err(ArgNumError(2, Some(2), args.len()))
    }
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
                Some(ref x) => Ok(x.to_datum()),
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
                Some(Some(x)) => Ok(x.to_datum()),
                _ => Err(TypeError),
            }
        },
        _ => Err(ArgNumError(2, Some(2), args.len())),
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
                Some(Ok(x)) => Ok(x.to_datum()),
                Some(Err(e)) => Err(e),
                None => Err(TypeError),
            }
        },
        _ => Err(ArgNumError(2, Some(2), args.len())),
    }
}

priv fn call_num_foldl(args: &[@RDatum],
                    a0: &LNumeric,
                    op: &fn(&LNumeric, &LNumeric) -> Result<LNumeric, RuntimeError>)
    -> Result<@RDatum, RuntimeError>
{
    let mut res:LNumeric = a0.clone();
    let mut err = false;
    do args.each |&arg| {
        match arg {
            @LNum(ref a) => {
                match op(&res, a) {
                    Ok(n) => {
                        res = n;
                        err = false;
                    },
                    _ => {
                        err = true;
                    }
                }
            },
            _ => {
                err = true;
            }
        }
        !err
    };

    if err {
        Err(TypeError)
    } else {
        Ok(@LNum(res))
    }
}

priv fn call_real_bfoldl(args: &[@RDatum], op: &fn(&LReal, &LReal) -> bool)
    -> Result<@RDatum, RuntimeError>
{
    let n = args.len();
    if n < 2 {
        return Err(ArgNumError(2, None, n));
    }

    let mut a = match args[0] {
        @LNum(ref n) => match get_real(n) {
            None => return Err(TypeError),
            Some(r) => r,
        },
        _ => return Err(TypeError),
    };

    let mut idx = 1;

    while idx < n {
        let b = match args[idx] {
            @LNum(ref n) => match get_real(n) {
                None => return Err(TypeError),
                Some(r) => r,
            },
            _ => return Err(TypeError),
        };

        if !op(&a, &b) {
            return Ok(@LBool(false));
        }

        a = b;

        idx += 1;
    }

    return Ok(@LBool(true));
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

        let mut res = Ok(@LExt(RUndef));

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
            PApply => do call_err2::<RuntimeData, GetList, @RDatum>(args) |f, l| {
                self.apply(f, l.list)
            },
            PBegin => if args.len() == 0 {
                    Ok(@LExt(RUndef))
                } else {
                    Ok(*args.last())
                },
            PAdd => do call_num_foldl(args, &Zero::zero()) |&lhs, &rhs| { Ok(lhs + rhs) },
            PSub => match args {
                [] => Err(ArgNumError(1, None, 0)),
                [@LNum(ref x)] => Ok(@LNum(-*x)),
                [@LNum(ref x), ..tail] =>
                    do call_num_foldl(tail, x) |&lhs, &rhs| { Ok(lhs - rhs) },
                _ => Err(TypeError),
            },
            PMul => do call_num_foldl(args, &One::one()) |&lhs, &rhs| { Ok(lhs * rhs) },
            PDiv => match args {
                [] => Err(ArgNumError(1, None, 0)),
                [@LNum(ref x)] => if x.is_zero() {
                        Err(DivideByZeroError)
                    } else {
                        Ok(@LNum(x.recip()))
                    },
                [@LNum(ref x), ..tail] =>
                    do call_num_foldl(tail, x) |&lhs, &rhs| {
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
                coerce(rx, ry, |&a, &b| { exact(a, b) }, |a, b| { inexact(a, b) })
            },
            PMakePolar => do call_tc2::<LReal, LReal, LNumeric>(args) |rx, ry| {
                polar(rx.to_inexact(), ry.to_inexact())
            },
            PRealPart => do call_tc1::<LNumeric, LNumeric>(args) |&x|  {
                match x {
                    NExact( Cmplx { re: ref re, im: _ } ) => from_rational(re),
                    NInexact( Cmplx { re: re, im: _ } ) => from_f64(re),
                }
            },
            PImagPart => do call_tc1::<LNumeric, LNumeric>(args) |&x|  {
                match x {
                    NExact( Cmplx { re: _, im: ref im } ) => from_rational(im),
                    NInexact( Cmplx { re: _, im: im } ) => from_f64(im),
                }
            },
            PMagnitude => do call_tc1::<LNumeric, LNumeric>(args) |x|  {
                let (norm, _) = x.to_inexact().to_polar();
                from_f64(norm)
            },
            PAngle => do call_tc1::<LNumeric, LNumeric>(args) |x|  {
                let (_, arg) = x.to_inexact().to_polar();
                from_f64(arg)
            },
            PNumerator => do call_tc1::<Rational, BigInt>(args) |x| { x.numerator().clone() },
            PDenominator => do call_tc1::<Rational, BigInt>(args) |x| { x.denominator().clone() },
            PCar => do call_tc1::<(@RDatum, @RDatum), @RDatum>(args) |&(h, _)| { h },
            PCdr => do call_tc1::<(@RDatum, @RDatum), @RDatum>(args) |&(_, t)| { t },
            PCons => do call_prim2(args) |arg1, arg2| { Ok(@LCons(arg1, arg2)) },
            PEqv => do call_prim2(args) |arg1, arg2| {
                let b =
                match (arg1, arg2) {
                    (@LCons(_, _), @LCons(_, _)) => managed::ptr_eq(arg1, arg2),
                    (@LString(_), @LString(_)) => managed::ptr_eq(arg1, arg2),
                    (@LExt(_), @LExt(_)) => managed::ptr_eq(arg1, arg2),
                    _ => arg1 == arg2,
                };
                Ok(@LBool(b))
            },
            PEqual => do call_tc2::<@RDatum, @RDatum, bool>(args) |&a, &b| { a == b },
            PNumber => typecheck::<LNumeric>(args),
            PReal => typecheck::<LReal>(args),
            PInteger => do call_tc1::<@RDatum, bool>(args) |&arg| {
                match arg {
                    @LNum(NExact(Cmplx { re: ref re, im: ref im })) =>
                        *re.numerator() == One::one() && *im.numerator() == One::one(),
                    @LNum(NInexact(Cmplx { re: re, im: im })) =>
                        re.round() == re && im.round() == im,
                    _ => false,
                }
            },
            PExact => typecheck::<Cmplx<Rational>>(args),
            PInexact => typecheck::<Cmplx<f64>>(args),
            PExactInexact => do call_tc1::<LNumeric, Cmplx<f64>>(args) |x| { x.to_inexact() },
            PNumberString => match args {
                [@LNum(ref x)] => Ok(@LString(x.to_str())),
                [@LNum(ref x), @LNum(ref r)] => match get_uint(r) {
                    None => Err(TypeError),
                    Some(radix) => {
                        match x {
                            &NExact(ref n) => Ok(@LString(n.to_str_radix(radix))),
                            _ =>
                                if radix == 10 {
                                    Ok(@LString(x.to_str()))
                                } else {
                                    Err(TypeError)
                                },
                        }
                    },
                },
                [_] | [_, _] => Err(TypeError),
                _ => Err(ArgNumError(1, Some(1), args.len())),
            },
            PEQ => do call_real_bfoldl(args) |&lhs, &rhs| { lhs == rhs },
            PGT => do call_real_bfoldl(args) |&lhs, &rhs| { lhs > rhs },
            PLT => do call_real_bfoldl(args) |&lhs, &rhs| { lhs < rhs },
            PGE => do call_real_bfoldl(args) |&lhs, &rhs| { lhs >= rhs },
            PLE => do call_real_bfoldl(args) |&lhs, &rhs| { lhs <= rhs },
            PNot => do call_tc1::<@RDatum, bool>(args) |&arg| {
                match arg {
                    @LBool(false) => true,
                    _ => false,
                }
            },
            PBoolean => typecheck::<bool>(args),
            PChar => do call_prim1(args) |arg| {
                match arg {
                    @LChar(_) => Ok(@LBool(true)),
                    _ => Ok(@LBool(false)),
                }
            },
            PProcedure => match args {
                [@LExt(RUndef)] => Ok(@LBool(false)),
                [@LExt(_)] => Ok(@LBool(true)),
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
                        Ok(@LVector(v))
                    },
                    None => Err(TypeError),
                },
                [@LNum(ref x), ref y] => match get_uint(x) {
                    Some(k) => {
                        let mut v = ~[];
                        v.grow(k, y);
                        Ok(@LVector(v))
                    },
                    None => Err(TypeError),
                },
                [_] | [_, _] => Err(TypeError),
                _ => Err(ArgNumError(1, Some(2), args.len())),
            },
            PVector => Ok(@LVector(args.to_owned())),
            PVectorLength => match args {
                [@LVector(ref v)] => Ok(@LNum(from_uint(v.len()))),
                [_] => Err(TypeError),
                _ => Err(ArgNumError(1, Some(1), args.len())),
            },
            PVectorRef => match args {
                [@LVector(ref v), @LNum(ref k)] => match get_uint(k) {
                    Some(i) =>
                        if i < v.len() {
                            Ok(v[i])
                        } else {
                            Err(RangeError)
                        },
                    None => Err(TypeError),
                },
                [_, _] => Err(TypeError),
                _ => Err(ArgNumError(2, Some(2), args.len())),
            },
            PVectorList => match args {
                [@LVector(ref v)] => Ok(LDatum::from_list(*v)),
                [_] => Err(TypeError),
                _ => Err(ArgNumError(1, Some(1), args.len())),
            },
            PListVector => match args {
                [arg] => match arg.to_list() {
                    Some(v) => Ok(@LVector(v)),
                    None => Err(TypeError),
                },
                _ => Err(ArgNumError(1, Some(1), args.len())),
            },
            PNull => typecheck::<()>(args),
            PPair => typecheck::<(@RDatum, @RDatum)>(args),
            PIsString => do call_prim1(args) |arg| {
                match arg {
                    @LString(_) => Ok(@LBool(true)),
                    _ => Ok(@LBool(false)),
                }
            },
            PString => {
                let char_list = do result::map_vec(args) |arg| {
                    match *arg {
                        @LChar(c) => Ok(c),
                        _ => Err(TypeError),
                    }
                };
                match char_list {
                    Ok(chars) => Ok(@LString(str::from_chars(chars))),
                    Err(e) => Err(e)
                }
            },
            PStringLength => do call_prim1(args) |arg| {
                match arg {
                    @LString(ref s) => Ok(@LNum(from_int(s.len() as int))),
                    _ => Err(TypeError),
                }
            },
            PStringRef => do call_prim2(args) |arg, idx| {
                match (arg, idx) {
                    (@LString(ref s), @LNum(ref n)) => match get_uint(n) {
                        Some(i) => if i < s.len() {
                                Ok(@LChar(s.char_at(i)))
                            } else {
                                Err(RangeError)
                            },
                        None => Err(TypeError),
                    },
                    _ => Err(TypeError),
                }
            },
            PSubstring => match args {
                [@LString(ref s), @LNum(ref n)] =>
                    match get_uint(n) {
                        Some(i) => if i <= s.len() {
                                Ok(@LString(s.slice(i, s.len()).to_owned()))
                            } else {
                                Err(RangeError)
                            },
                        _ => Err(TypeError),
                    },
                [@LString(ref s), @LNum(ref from), @LNum(ref to)] =>
                    match (get_uint(from), get_uint(to)) {
                        (Some(start), Some(end)) =>
                            if start <= end && end <= s.len() {
                                Ok(@LString(s.slice(start, end).to_owned()))
                            } else {
                                Err(RangeError)
                            },
                        _ => Err(TypeError),
                    },
                [_, _] | [_, _, _] => Err(TypeError),
                _ => Err(ArgNumError(2, Some(3), args.len())),
            },
            PSymbol => do call_prim1(args) |arg| {
                match arg {
                    @LIdent(_) => Ok(@LBool(true)),
                    _ => Ok(@LBool(false)),
                }
            },
            PSymbolString => do call_prim1(args) |arg| {
                match arg {
                    @LIdent(ref s) => Ok(@LString(s.to_owned())),
                    _ => Err(TypeError),
                }
            },
            PStringSymbol => do call_prim1(args) |arg| {
                match arg {
                    @LString(ref s) => Ok(@LIdent(s.to_managed())),
                    _ => Err(TypeError),
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
                match result::map_vec(*v, |x| { self.recursive_qq(x) }) {
                    Ok(qmap) => Ok(@LVector(qmap)),
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
            &RUndef =>
                Err(NotCallable),
            &RPrim(f) =>
                self.call_prim(f, args),
            &RProc(ref anames, ref vargs, ref code, ref env) =>
                self.call_proc(*anames, *vargs, *code, *env, args),
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
        let mut parser = Parser(rdr);
        match parser.parse() {
            Ok(datum) => self.eval(@datum),
            Err(e) => {
                let (line, col) = parser.pos();
                Err(ParseError(line, col, e))
            },
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
