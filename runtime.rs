use datum::*;

struct Runtime {
    stdin: @io::Reader,
    stdout: @io::Writer,
    stderr: @io::Writer,
}

#[deriving(Eq)]
enum RuntimeError {
    UnknownLocalVariable(~str),
    NotImplemented,
}

impl ToStr for RuntimeError {
    fn to_str(&self) -> ~str {
        err_to_str(self)
    }
}

priv fn err_to_str(&err: &RuntimeError) -> ~str {
    match err {
        UnknownLocalVariable(name) => ~"unknown local variable: " + copy name,
        NotImplemented => ~"not implemented",
    }
}

pub impl Runtime {
    fn new_std() -> Runtime {
        Runtime {
            stdin: io::stdin(),
            stdout: io::stdout(),
            stderr: io::stderr(),
        }
    }

    fn eval(&mut self, &val: &LDatum) -> Result<LDatum, RuntimeError> {
        match copy val {
            LIdent(name) => Err(UnknownLocalVariable(name)),
            LCons(_,_) => Err(NotImplemented),
            LNil => Err(NotImplemented),
            _ => Ok(val),
        }
    }
}

#[test]
fn test_basic_eval() {
    let mut runtime = Runtime::new_std();
    assert_eq!(runtime.eval(@LInt(486)), Ok(LInt(486)));
}
