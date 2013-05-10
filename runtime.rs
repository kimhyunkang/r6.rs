use datum::*;
use primitive::*;
use numeric::LNumeric;
use std::fun_treemap;
use std::fun_treemap::Treemap;
use core::num::Zero::zero;
use core::num::One::one;

enum Stack<T> {
    Top(T, @Stack<T>),
    Bot,
}

struct Runtime {
    stdin: @io::Reader,
    stdout: @io::Writer,
    stderr: @io::Writer,
    stack: @Stack<Treemap<@str, @LDatum>>,
    syntax: Treemap<@str, PrimSyntax>,
}

#[deriving(Eq)]
enum RuntimeError {
    UnboundVariable(@str),
    NotCallable,
    NotList,
    ArgNumError,
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
        NotCallable => ~"not callable",
        NotList => ~"not list",
        ArgNumError => ~"bad number of arguments",
        TypeError => ~"type error",
        DivideByZeroError => ~"divide by zero",
        NilEval => ~"() cannot be evaluated",
        BadSyntax(syn, reason) => ~"bad syntax for " + syn.to_str() + ": " + reason,
    }
}

fn load_prelude() -> Treemap<@str, @LDatum> {
    let mut map = fun_treemap::init();
    for vec::each(prelude()) |&pair| {
        let (key, func) = pair;
        map = fun_treemap::insert(map, key, @LPrim(func));
    }
    map
}

fn load_prelude_macro() -> Treemap<@str, PrimSyntax> {
    let mut map = fun_treemap::init();
    for vec::each(syntax_prelude()) |&pair| {
        let (key, syntax) = pair;
        map = fun_treemap::insert(map, key, syntax);
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
        Err(ArgNumError)
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
        Err(ArgNumError)
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

priv impl Runtime {
    fn get_syntax(&self, val: @LDatum) -> Option<PrimSyntax> {
        match *val {
            LIdent(name) => fun_treemap::find(self.syntax, name),
            _ => None,
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
                    Err(BadSyntax(SynLambda, ~"no procedure given"))
                } else {
                    match args[0].to_list() {
                        None => Err(BadSyntax(SynLambda, ~"non-list arguments")),
                        Some(largs) => {
                            let names = do result::map_vec(largs) |&arg| {
                                match *arg {
                                    LIdent(name) => Ok(name),
                                    _ => Err(BadSyntax(SynLambda, ~"non-symbol arguments")),
                                }
                            };
                            do result::map(&names) |&anames| {
                                let seq = vec::from_slice(vec::slice(args, 1, args.len()));
                                @LProc(anames, seq)
                            }
                        },
                    }
                },
            SynQuote => if args.len() == 1 {
                    Ok(args[0])
                } else {
                    Err(BadSyntax(SynQuote, ~"bad number of arguments"))
                },
        }
    }

    fn call_proc(&mut self,
                anames: &~[@str],
                code: &~[@LDatum],
                args: ~[@LDatum]) -> Result<@LDatum, RuntimeError>
    {
        if anames.len() != args.len() {
            Err(ArgNumError)
        } else {
            // store current stack
            let old_stack = self.stack;

            let mut frame =
            match *old_stack {
                // this is a critical failure
                Bot => fail!(~"stack underflow"),
                Top(st, _) => st,
            };

            for uint::range(0, anames.len()) |i| {
                frame = fun_treemap::insert(frame, anames[i], args[i]);
            }

            // add new frame to the stack
            self.stack = @Top(frame, old_stack);

            let mut res:Result<@LDatum, RuntimeError> = Err(NilEval);
            do code.each() |&val| {
                res = self.eval(val);
                res.is_ok()
            }

            // restore stack
            self.stack = old_stack;

            res
        }
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
        }
    }
}

pub impl Runtime {
    fn new_std() -> Runtime {
        Runtime {
            stdin: io::stdin(),
            stdout: io::stdout(),
            stderr: io::stderr(),
            stack: @Top(load_prelude(), @Bot),
            syntax: load_prelude_macro(),
        }
    }

    fn eval(&mut self, val: @LDatum) -> Result<@LDatum, RuntimeError>
    {
        match *val {
            LIdent(name) => 
                match *self.stack {
                    Bot => fail!(~"stack underflow"),
                    Top(frame, _) => match fun_treemap::find(frame, name) {
                        Some(datum) => Ok(datum),
                        None => Err(UnboundVariable(name)),
                    },
                },
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
                                    Ok(@LProc(ref anames, ref code)) =>
                                        match result::map_vec(aexprs, |&expr| self.eval(expr))
                                        {
                                            Ok(args) => self.call_proc(anames, code, args),
                                            Err(e) => Err(e),
                                        },
                                    Ok(_) => Err(NotCallable),
                                    Err(e) => Err(e),
                                },
                        }
                    },
                },
            LQuote(val) => Ok(val),
            LNil => Err(NilEval),
            _ => Ok(val),
        }
    }
}
