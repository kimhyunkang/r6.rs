use datum::*;

struct Runtime {
    stdin: @io::Reader,
    stdout: @io::Writer,
    stderr: @io::Writer,
}

#[deriving(Eq)]
enum RuntimeError {
    UnknownLocalVariable,
    NotImplemented,
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
        match val {
            LIdent(_) => Err(UnknownLocalVariable),
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
