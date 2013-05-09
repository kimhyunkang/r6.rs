use datum::*;
use primitive::*;
use numeric::LNumeric;
use core::hashmap::linear::LinearMap;
use core::num::Zero::zero;
use core::num::One::one;

struct Runtime {
    stdin: @io::Reader,
    stdout: @io::Writer,
    stderr: @io::Writer,
    global: LinearMap<~str, @LDatum>
}

#[deriving(Eq)]
enum RuntimeError {
    UnboundVariable(~str),
    NotCallable,
    NotList,
    ArgNumError,
    TypeError,
    DivideByZeroError,
    NilEval,
}

impl ToStr for RuntimeError {
    fn to_str(&self) -> ~str {
        err_to_str(self)
    }
}

priv fn err_to_str(&err: &RuntimeError) -> ~str {
    match err {
        UnboundVariable(name) => ~"unbound variable: " + copy name,
        NotCallable => ~"not callable",
        NotList => ~"not list",
        ArgNumError => ~"bad number of arguments",
        TypeError => ~"type error",
        DivideByZeroError => ~"divide by zero",
        NilEval => ~"() cannot be evaluated",
    }
}

fn load_prelude() -> LinearMap<~str, @LDatum> {
    let mut map = LinearMap::new();
    for vec::each(prelude()) |&pair| {
        let (key, func) = pair;
        map.insert(key, @LPrim(func));
    }
    map
}

priv fn call_prim1(args: &[@LDatum],
                op: &fn(@LDatum) -> Result<@LDatum, RuntimeError>)
    -> Result<@LDatum, RuntimeError>
{
    if args.len() == 1 {
        op(args[0])
    } else {
        Err(ArgNumError)
    }
}

priv fn call_num_prim2(args: &[@LDatum],
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
        Err(ArgNumError)
    }
}

priv fn call_num_foldl(args: &[@LDatum],
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

pub impl Runtime {
    fn new_std() -> Runtime {
        Runtime {
            stdin: io::stdin(),
            stdout: io::stdout(),
            stderr: io::stderr(),
            global: load_prelude(),
        }
    }

    fn eval(&mut self, val: @LDatum) -> Result<@LDatum, RuntimeError> {
        match copy *val {
            LIdent(name) => 
                match self.global.find(&name) {
                    Some(&datum) => Ok(datum),
                    None => Err(UnboundVariable(name)),
                },
            LCons(fexpr, aexpr) =>
                match aexpr.to_list() {
                    None => Err(NotList),
                    Some(aexprs) => {
                        match self.eval(fexpr) {
                            Ok(@LPrim(f)) => {
                                match result::map_vec(aexprs, |&expr| self.eval(expr)) {
                                    Ok(args) => self.call_prim(f, args),
                                    Err(e) => Err(e),
                                }
                            }
                            Ok(_) => Err(NotCallable),
                            Err(e) => Err(e),
                        }
                    },
                },
            LQuote(val) => Ok(val),
            LNil => Err(NilEval),
            _ => Ok(val),
        }
    }

    fn call_prim(&mut self, f: PFunc, args: &[@LDatum]) -> Result<@LDatum, RuntimeError> {
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
        }
    }
}
