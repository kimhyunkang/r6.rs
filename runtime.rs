use std::io;
use std::borrow;
use std::uint;
use std::result;
use std::num::{One, Zero};
use std::hashmap::HashMap;
use datum::*;
use primitive::*;
use numeric::LNumeric;
use stack::*;

enum RuntimeData {
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
        RPrim(f) => f.to_str(),
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
    ArgNumError(uint, bool, uint),
    TypeError,
    DivideByZeroError,
    NilEval,
    BadSyntax(PrimSyntax, ~str),
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
        ArgNumError(expected, false, argnum) => {
            fmt!("expected %u arguments, but found %u arguments", expected, argnum)
        },
        ArgNumError(expected, true, argnum) => {
            fmt!("expected %u or more arguments, but found %u arguments", expected, argnum)
        },
        TypeError => ~"type error",
        DivideByZeroError => ~"divide by zero",
        NilEval => ~"() cannot be evaluated",
        BadSyntax(syn, reason) => ~"bad syntax for " + syn.to_str() + ": " + reason,
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

priv fn call_prim1(args: ~[@RDatum],
                op: &fn(@RDatum) -> Result<@RDatum, RuntimeError>)
    -> Result<@RDatum, RuntimeError>
{
    if args.len() == 1 {
        op(args[0])
    } else {
        Err(ArgNumError(1, false, args.len()))
    }
}

priv fn call_prim2(args: ~[@RDatum],
                op: &fn(@RDatum, @RDatum) -> Result<@RDatum, RuntimeError>)
    -> Result<@RDatum, RuntimeError>
{
    if args.len() == 2 {
        op(args[0], args[1])
    } else {
        Err(ArgNumError(2, false, args.len()))
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
        Err(ArgNumError(2, false, args.len()))
    }
}

priv fn call_num_foldl(args: ~[@RDatum],
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
        }
    }

    fn call_proc(&mut self,
                anames: &~[@str],
                vargs: &Option<@str>,
                code: &~[@RDatum],
                &frame: &@mut Stack<HashMap<@str, @RDatum>>,
                args: ~[@RDatum]) -> Result<@RDatum, RuntimeError>
    {
        // create new frame to store args
        let mut arg_frame = HashMap::new();

        match *vargs {
            None => if args.len() != anames.len() {
                    return Err(ArgNumError(anames.len(), false, args.len()));
                },
            Some(vname) => if args.len() < anames.len() {
                    return Err(ArgNumError(anames.len(), true, args.len()));
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
                args: ~[@RDatum]) -> Result<@RDatum, RuntimeError>
    {
        match f {
            PEval => do call_prim1(args) |arg| {
                self.eval(arg)
            },
            PAdd => do call_num_foldl(args, Zero::zero()) |&lhs, &rhs| { Ok(lhs + rhs) },
            PSub => do call_num_foldl(args, One::one()) |&lhs, &rhs| { Ok(lhs - rhs) },
            PMul => do call_num_foldl(args, One::one()) |&lhs, &rhs| { Ok(lhs * rhs) },
            PDiv => do call_num_foldl(args, One::one()) |&lhs, &rhs| {
                if rhs.is_zero() {
                    Err(DivideByZeroError)
                } else {
                    Ok(lhs / rhs)
                }
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
            PEqv => do call_prim2(args) |arg1, arg2| {
                Ok(@LBool(arg1 == arg2))
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

    fn call(&mut self, proc: &RuntimeData, aexprs: ~[@RDatum]) -> Result<@RDatum, RuntimeError> {
        match result::map_vec(aexprs, |&expr| self.eval(expr))
        {
            Ok(args) => match proc {
                &RPrim(f) =>
                    self.call_prim(f, args),
                &RProc(ref anames, ref vargs, ref code, ref env) =>
                    self.call_proc(anames, vargs, code, env, args),
            },
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
