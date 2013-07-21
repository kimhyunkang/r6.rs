use std::io;
use std::borrow;
use std::uint;
use std::result;
use std::str;
use std::vec;
use std::num::{One, Zero};
use std::hashmap::HashMap;
use std::managed;
use extra::complex::Cmplx;
use datum::*;
use primitive::*;
use numeric::*;
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
    StringRangeError,
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
        StringRangeError => ~"string index out of range", 
    }
}

fn load_prelude() -> HashMap<@str, Either<@RDatum, PrimSyntax>> {
    let mut map = HashMap::new();
    for prelude().each |&pair| {
        let (key, func) = pair;
        map.insert(key, Left(@LExt(RPrim(func))));
    }
    for syntax_prelude().each |&pair| {
        let (key, syntax) = pair;
        map.insert(key, Right(syntax));
    }
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

priv fn call_num_prim2(args: ~[@RDatum],
                    op: &fn(&LNumeric, &LNumeric) -> Result<@RDatum, RuntimeError>)
    -> Result<@RDatum, RuntimeError>
{
    if args.len() == 2 {
        match *args[0] {
            LNum(lhs) => match *args[1] {
                LNum(rhs) => op(&lhs, &rhs),
                _ => Err(TypeError),
            },
            _ => Err(TypeError),
        }
    } else {
        Err(ArgNumError(2, Some(2), args.len()))
    }
}

priv fn call_num_foldl(args: &[@RDatum],
                    a0: LNumeric,
                    op: &fn(&LNumeric, &LNumeric) -> Result<LNumeric, RuntimeError>)
    -> Result<@RDatum, RuntimeError>
{
    let mut res = a0;
    let mut err = false;
    do args.each |&arg| {
        match *arg {
            LNum(a) => {
                match op(&res, &a) {
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

priv fn call_num_foldl1(args: &[@RDatum],
                    op: &fn(&LNumeric, &LNumeric) -> Result<LNumeric, RuntimeError>)
    -> Result<@RDatum, RuntimeError>
{
    if args.len() == 0 {
        return Err(ArgNumError(1, None, 0))
    };

    match *args[0] {
        LNum(a) => {
            call_num_foldl(args.slice(1, args.len()), a, op)
        },
        _ => {
            Err(TypeError)
        }
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
                Ok(@LBool(true)) => {
                    res = self.eval(expr);
                    false
                },
                Ok(@LBool(false)) => true,
                _ => {
                    res = Err(BadSyntax(SynCond, ~"invalid conditional predicate"));
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
                            LBool(true) => self.eval(args[1]),
                            LBool(false) => self.eval(args[2]),
                            _ => Err(TypeError),
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
            SynAnd => do self.syn_bool_foldl(args, true) |b0, b1| { b0 && b1 },
            SynOr => do self.syn_bool_foldl(args, false) |b0, b1| { b0 || b1 },
        }
    }

    priv fn syn_bool_foldl(&mut self, args: &[@RDatum], a0: bool, op: &fn(bool, bool) -> bool)
        -> Result<@RDatum, RuntimeError>
    {
        let mut res = a0;
        let mut err:Option<RuntimeError> = None;

        do args.each |arg| {
            match self.eval(*arg) {
                Ok(@LBool(a)) => {
                    res = op(res, a);
                },
                Ok(_) => {
                    err = Some(TypeError);
                },
                Err(e) => {
                    err = Some(e);
                }
            }

            res == a0 && err.is_none()
        };

        match err {
            Some(e) => Err(e),
            None => Ok(@LBool(res)),
        }
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
            PEval => do call_prim1(args) |arg| {
                self.eval(arg)
            },
            PApply => match args {
                [@LExt(ref f), arg] => match arg.to_list() {
                    Some(alist) => self.apply(f, alist),
                    None => Err(NotList),
                },
                [_, _arg] => Err(NotCallable),
                _ => Err(ArgNumError(2, Some(2), 0)),
            },
            PBegin => if args.len() == 0 {
                    Ok(@LNil)
                } else {
                    Ok(*args.last())
                },
            PAdd => do call_num_foldl(args, Zero::zero()) |&lhs, &rhs| { Ok(lhs + rhs) },
            PSub => do call_num_foldl1(args) |&lhs, &rhs| { Ok(lhs - rhs) },
            PMul => do call_num_foldl(args, One::one()) |&lhs, &rhs| { Ok(lhs * rhs) },
            PDiv => do call_num_foldl1(args) |&lhs, &rhs| {
                if rhs.is_zero() {
                    Err(DivideByZeroError)
                } else {
                    Ok(lhs / rhs)
                }
            },
            PQuotient => do call_num_foldl1(args) |&lhs, &rhs| {
                if rhs.is_zero() {
                    Err(DivideByZeroError)
                } else {
                    match (get_int(&lhs), get_int(&rhs)) {
                        (Some(l), Some(r)) => Ok(from_int(l / r)),
                        _ => Err(TypeError),
                    }
                }
            },
            PRemainder => do call_num_foldl1(args) |&lhs, &rhs| {
                if rhs.is_zero() {
                    Err(DivideByZeroError)
                } else {
                    match (get_int(&lhs), get_int(&rhs)) {
                        (Some(l), Some(r)) => Ok(from_int(l % r)),
                        _ => Err(TypeError),
                    }
                }
            },
            PModulo => do call_num_foldl1(args) |&lhs, &rhs| {
                if rhs.is_zero() {
                    Err(DivideByZeroError)
                } else {
                    match (get_int(&lhs), get_int(&rhs)) {
                        (Some(l), Some(r)) => Ok(from_int(modulo(l, r))),
                        _ => Err(TypeError),
                    }
                }
            },
            PNumerator => match args {
                [@LNum(NExact( Cmplx { re: re, im: im } ))] if im.is_zero() =>
                    Ok(@LNum( from_int(re.numerator()) )),
                [_] =>
                    Err(TypeError),
                _ =>
                    Err(ArgNumError(1, Some(1), args.len())),
            },
            PDenominator => match args {
                [@LNum(NExact( Cmplx { re: re, im: im } ))] if im.is_zero() =>
                    Ok(@LNum( from_int(re.denominator()) )),
                [_] =>
                    Err(TypeError),
                _ =>
                    Err(ArgNumError(1, Some(1), args.len())),
            },
            PCar => do call_prim1(args) |arg| {
                match *arg {
                    LCons(h, _) => Ok(h),
                    _ => Err(TypeError),
                }
            },
            PCdr => do call_prim1(args) |arg| {
                match *arg {
                    LCons(_, t) => Ok(t),
                    _ => Err(TypeError),
                }
            },
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
            PEqual => do call_prim2(args) |arg1, arg2| {
                Ok(@LBool(arg1 == arg2))
            },
            PNumber => do call_prim1(args) |arg| {
                match *arg {
                    LNum(_) => Ok(@LBool(true)),
                    _ => Ok(@LBool(false)),
                }
            },
            PReal => do call_prim1(args) |arg| {
                match *arg {
                    LNum(c) => Ok(@LBool(c.is_real())),
                    _ => Ok(@LBool(false)),
                }
            },
            PInteger => do call_prim1(args) |arg| {
                match *arg {
                    LNum(NExact(Cmplx { re: re, im: im })) =>
                        Ok(@LBool(re.numerator() == 1 && im.numerator() == 1)),
                    LNum(NInexact(Cmplx { re: re, im: im })) =>
                        Ok(@LBool(re.round() == re && im.round() == im)),
                    _ => Ok(@LBool(false)),
                }
            },
            PExact => do call_prim1(args) |arg| {
                match *arg {
                    LNum(NExact(_)) => Ok(@LBool(true)),
                    _ => Ok(@LBool(false)),
                }
            },
            PInexact => do call_prim1(args) |arg| {
                match *arg {
                    LNum(NInexact(_)) => Ok(@LBool(true)),
                    _ => Ok(@LBool(false)),
                }
            },
            PEQ => do call_real_bfoldl(args) |&lhs, &rhs| { lhs == rhs },
            PGT => do call_real_bfoldl(args) |&lhs, &rhs| { lhs > rhs },
            PLT => do call_real_bfoldl(args) |&lhs, &rhs| { lhs < rhs },
            PGE => do call_real_bfoldl(args) |&lhs, &rhs| { lhs >= rhs },
            PLE => do call_real_bfoldl(args) |&lhs, &rhs| { lhs <= rhs },
            PNot => do call_prim1(args) |arg| {
                match arg {
                    @LBool(b) => Ok(@LBool(!b)),
                    _ => Err(TypeError),
                }
            },
            PNull => do call_prim1(args) |arg| {
                match arg {
                    @LNil => Ok(@LBool(true)),
                    _ => Ok(@LBool(false)),
                }
            },
            PPair => do call_prim1(args) |arg| {
                match arg {
                    @LCons(_, _) => Ok(@LBool(true)),
                    _ => Ok(@LBool(false)),
                }
            },
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
                                Err(StringRangeError)
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
                                Err(StringRangeError)
                            },
                        _ => Err(TypeError),
                    },
                [@LString(ref s), @LNum(ref from), @LNum(ref to)] =>
                    match (get_uint(from), get_uint(to)) {
                        (Some(start), Some(end)) =>
                            if start <= end && end <= s.len() {
                                Ok(@LString(s.slice(start, end).to_owned()))
                            } else {
                                Err(StringRangeError)
                            },
                        _ => Err(TypeError),
                    },
                [_, _] | [_, _, _] => Err(TypeError),
                _ => Err(ArgNumError(2, Some(3), args.len())),
            }
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
