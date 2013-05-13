use datum::*;
use primitive::*;
use numeric::LNumeric;
use core::num::Zero::zero;
use core::num::One::one;
use core::hashmap::linear::LinearMap;
use stack::*;

struct Runtime {
    stdin: @io::Reader,
    stdout: @io::Writer,
    stderr: @io::Writer,
    env: @mut Stack<LinearMap<@str, @LDatum>>,
    global: LinearMap<@str, Either<@LDatum, PrimSyntax>>,
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

fn load_prelude() -> LinearMap<@str, Either<@LDatum, PrimSyntax>> {
    let mut map = LinearMap::new();
    for vec::each(prelude()) |&pair| {
        let (key, func) = pair;
        map.insert(key, Left(@LPrim(func)));
    }
    for vec::each(syntax_prelude()) |&pair| {
        let (key, syntax) = pair;
        map.insert(key, Right(syntax));
    }
    map
}

priv fn call_prim1(args: ~[@LDatum],
                op: &fn(@LDatum) -> Result<@LDatum, RuntimeError>)
    -> Result<@LDatum, RuntimeError>
{
    if args.len() == 1 {
        op(args[0])
    } else {
        Err(ArgNumError(1, false, args.len()))
    }
}

priv fn call_prim2(args: ~[@LDatum],
                op: &fn(@LDatum, @LDatum) -> Result<@LDatum, RuntimeError>)
    -> Result<@LDatum, RuntimeError>
{
    if args.len() == 2 {
        op(args[0], args[1])
    } else {
        Err(ArgNumError(2, false, args.len()))
    }
}

priv fn call_num_prim2(args: ~[@LDatum],
                    op: &fn(&LNumeric, &LNumeric) -> Result<@LDatum, RuntimeError>)
    -> Result<@LDatum, RuntimeError>
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

priv fn call_num_foldl(args: ~[@LDatum],
                    a0: LNumeric,
                    op: &fn(&LNumeric, &LNumeric) -> Result<LNumeric, RuntimeError>)
    -> Result<@LDatum, RuntimeError>
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
    }
    if err {
        Err(TypeError)
    } else {
        Ok(@LNum(res))
    }
}

priv fn get_syms(&arg: &@LDatum) -> Result<(~[@str], Option<@str>), ~str> {
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

priv impl Runtime {
    fn get_syntax(&self, val: @LDatum) -> Option<PrimSyntax> {
        match *val {
            LIdent(name) => match self.global.find(&name) {
                Some(&Right(syn)) => Some(syn),
                _ => None,
            },
            _ => None,
        }
    }

    fn find_var(&self, name: &@str) -> Result<@LDatum, RuntimeError> {
        let mut val: Option<@LDatum> = None;

        do self.env.each |frame| {
            match frame.find(name) {
                None => true,
                Some(v) => {
                    val = Some(*v);
                    false
                }
            }
        }

        match val {
            None => match self.global.find(name) {
                Some(&Left(v)) => Ok(v),
                Some(&Right(_)) => Err(RefMacro(*name)),
                None => Err(UnboundVariable(*name)),
            },
            Some(v) => Ok(v),
        }
    }

    fn run_syntax(&mut self,
                syn: PrimSyntax,
                args: ~[@LDatum]) -> Result<@LDatum, RuntimeError>
    {
        match syn {
            SynIf => if args.len() == 3 {
                    do result::chain(self.eval(args[0])) |cond| {
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
                            let seq = vec::from_slice(vec::slice(args, 1, args.len()));
                            Ok(@LProc(anames, varargs, seq, self.env))
                        },
                    }
                },
            SynDefine => if args.len() < 2 {
                    Err(BadSyntax(SynDefine, ~"no body given"))
                } else {
                    match get_syms(&args[0]) {
                        Err(e) => Err(BadSyntax(SynDefine, e)),
                        Ok((anames, varargs)) =>
                            if anames.is_empty() {
                                match varargs {
                                    None => Err(BadSyntax(SynDefine, ~"name not given")),
                                    Some(name) => if args.len() != 2 {
                                            Err(BadSyntax(SynDefine, ~"multiple expressions"))
                                        } else {
                                            do result::map(&self.eval(args[1])) |&val| {
                                                self.global.insert(name, Left(val));
                                                @LNil
                                            }
                                        }
                                }
                            } else {
                                let name = anames[0];
                                let anames = vec::from_slice(vec::slice(anames, 1, anames.len()));
                                let seq = vec::from_slice(vec::slice(args, 1, args.len()));
                                let proc = @LProc(anames, varargs, seq, self.env);
                                self.global.insert(name, Left(proc));
                                Ok(@LNil)
                            }
                    }
                },
            SynSet => if args.len() != 2 {
                    Err(BadSyntax(SynSet, ~"bad number of arguments"))
                } else {
                    match *args[0] {
                        LIdent(name) => do result::chain(self.eval(args[1])) |val| {
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
                    self.quasiquote(args[0])
                } else {
                    Err(BadSyntax(SynQQuote, ~"bad number of arguments"))
                },
            SynUnquote => if args.len() == 1 {
                    self.unquote(args[0])
                } else {
                    Err(BadSyntax(SynUnquote, ~"bad number of arguments"))
                },
        }
    }

    fn call_proc(&mut self,
                anames: &~[@str],
                vargs: &Option<@str>,
                code: &~[@LDatum],
                &frame: &@mut Stack<LinearMap<@str, @LDatum>>,
                args: ~[@LDatum]) -> Result<@LDatum, RuntimeError>
    {
        // create new frame to store args
        let mut arg_frame = LinearMap::new();

        match *vargs {
            None => if args.len() != anames.len() {
                    return Err(ArgNumError(anames.len(), false, args.len()));
                },
            Some(vname) => if args.len() < anames.len() {
                    return Err(ArgNumError(anames.len(), true, args.len()));
                } else {
                    let vslice = vec::slice(args, anames.len(), args.len());
                    let va = do vec::foldr(vslice, @LNil) |&a, l| {
                        @LCons(a, l)
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
        let mut res:Result<@LDatum, RuntimeError> = Err(NilEval);
        do code.each() |&val| {
            res = self.eval(val);
            res.is_ok()
        }

        // restore env
        self.env = old_env;

        res
    }

    fn call_prim(&mut self,
                f: PFunc,
                args: ~[@LDatum]) -> Result<@LDatum, RuntimeError>
    {
        match f {
            PEval => do call_prim1(args) |arg| {
                self.eval(arg)
            },
            PAdd => do call_num_foldl(args, zero()) |&lhs, &rhs| { Ok(lhs + rhs) },
            PSub => do call_num_foldl(args, one()) |&lhs, &rhs| { Ok(lhs - rhs) },
            PMul => do call_num_foldl(args, one()) |&lhs, &rhs| { Ok(lhs * rhs) },
            PDiv => do call_num_foldl(args, one()) |&lhs, &rhs| {
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

    fn recursive_qq(&mut self, val: @LDatum) -> Result<@LDatum, RuntimeError> {
        match *val {
            LCons(h,t) =>
                do result::chain(self.recursive_qq(h)) |qh| {
                    do result::map(&self.recursive_qq(t)) |&qt| {
                        @LCons(qh, qt)
                    }
                },
            LQQuote(v) =>
                do result::map(&self.quasiquote(v)) |&qv| {
                    @LQQuote(qv)
                },
            LUnquote(v) =>
                self.unquote(v),
            _ => 
                Ok(val),
        }
    }

    fn quasiquote(&mut self, val: @LDatum) -> Result<@LDatum, RuntimeError> {
        self.qq_lvl += 1;
        let res = self.recursive_qq(val);
        self.qq_lvl -= 1;
        res
    }

    fn unquote(&mut self, val: @LDatum) -> Result<@LDatum, RuntimeError> {
        if self.qq_lvl == 0 {
            Err(BadSyntax(SynUnquote, ~"unquote not nested in quasiquote"))
        } else {
            self.qq_lvl -= 1;
            let res =
            if self.qq_lvl == 0 {
                self.eval(val)
            } else {
                do result::map(&self.recursive_qq(val)) |&qval| {
                    @LUnquote(qval)
                }
            };
            self.qq_lvl += 1;
            res
        }
    }
}

priv fn set_var(env: @mut Stack<LinearMap<@str, @LDatum>>,
                name: &@str,
                val: @LDatum) -> bool {
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
    }

    success
}

pub impl Runtime {
    fn new_std() -> Runtime {
        Runtime {
            stdin: io::stdin(),
            stdout: io::stdout(),
            stderr: io::stderr(),
            env: @mut Stack::new(),
            global: load_prelude(),
            qq_lvl: 0,
        }
    }

    fn eval(&mut self, val: @LDatum) -> Result<@LDatum, RuntimeError>
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
                                    Ok(@LPrim(f)) =>
                                        match result::map_vec(aexprs, |&expr| self.eval(expr))
                                        {
                                            Ok(args) => self.call_prim(f, args),
                                            Err(e) => Err(e),
                                        },
                                    Ok(@LProc(ref anames, ref vargs, ref code, ref env)) =>
                                        match result::map_vec(aexprs, |&expr| self.eval(expr))
                                        {
                                            Ok(args) =>
                                                self.call_proc(anames, vargs, code, env, args),
                                            Err(e) =>
                                                Err(e),
                                        },
                                    Ok(_) => Err(NotCallable),
                                    Err(e) => Err(e),
                                },
                        }
                    },
                },
            LQuote(val) => Ok(val),
            LQQuote(val) => self.quasiquote(val),
            LUnquote(val) => self.unquote(val),
            LNil => Err(NilEval),
            _ => Ok(val),
        }
    }
}
