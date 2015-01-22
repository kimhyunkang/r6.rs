use std::string::CowString;
use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::ops::Deref;

use error::{CompileError, CompileErrorKind, RuntimeError};
use datum::Datum;
use runtime::{Inst, MemRef, RDatum, RuntimeData, Closure};

#[derive(Copy, Clone, PartialEq)]
pub enum Syntax {
    Lambda
}

pub enum EnvVar {
    Syntax(Syntax),
    PrimFunc(&'static str, Rc<fn(&[RDatum]) -> Result<RDatum, RuntimeError>>),
    Closure(Closure)
}

pub struct Compiler<'g> {
    global_env: &'g HashMap<CowString<'static>, EnvVar>
}

impl<'g> Compiler<'g> {
    pub fn new<'a>(global_env: &'a HashMap<CowString<'static>, EnvVar>) -> Compiler<'a> {
        Compiler {
            global_env: global_env
        }
    }

    pub fn compile(&mut self, datum: &RDatum) -> Result<Vec<Inst>, CompileError> {
        let mut link_size = 0;
        let mut code = try!(self.compile_expr(&[], &[], &mut link_size, datum));
        code.push(Inst::Return);
        return Ok(code);
    }

    fn compile_call(&mut self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                    link_size: &mut usize, datum: &RDatum)
            -> Result<Vec<Inst>, CompileError>
    {
        let mut code = Vec::new();
        let mut arg_count = 0;
        for d in datum.iter() {
            match d {
                Ok(d) => {
                    code.push_all(try!(self.compile_expr(static_scope, args, link_size, &d)).as_slice());
                    arg_count += 1;
                },
                Err(()) => return Err(CompileError { kind: CompileErrorKind::DottedEval })
            }
        }

        if arg_count == 0 {
            Err(CompileError { kind: CompileErrorKind::NullEval })
        } else {
            code.push(Inst::Call(arg_count - 1));
            Ok(code)
        }
    }

    fn compile_expr(&mut self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                    link_size: &mut usize, datum: &RDatum)
            -> Result<Vec<Inst>, CompileError>
    {
        match datum {
            &Datum::Cons(ref h, ref t) =>
                if let &Datum::Sym(ref n) = h.borrow().deref() {
                    if let Some(&EnvVar::Syntax(Syntax::Lambda)) = self.global_env.get(n) {
                        if let &Datum::Cons(ref cur_args, ref body) = t.borrow().deref() {
                            let new_stackenv = {
                                let mut nenv = static_scope.to_vec();
                                nenv.push(args.to_vec());
                                nenv
                            };

                            let new_args = {
                                let mut nargs = Vec::new();
                                for arg in cur_args.borrow().iter() {
                                    if let Ok(Datum::Sym(s)) = arg {
                                        nargs.push(s)
                                    } else {
                                        return Err(CompileError { kind: CompileErrorKind::BadLambdaSyntax });
                                    }
                                }
                                nargs
                            };

                            let mut new_link_size = 0;
                            let mut code = Vec::new();
                            for expr in body.borrow().iter() {
                                if let Ok(e) = expr {
                                    let piece = try!(self.compile_expr(
                                            new_stackenv.as_slice(),
                                            new_args.as_slice(),
                                            &mut new_link_size,
                                            &e));
                                    code.push_all(piece.as_slice());
                                } else {
                                    return Err(CompileError { kind: CompileErrorKind::DottedBody });
                                }
                            }

                            code.push(Inst::Return);

                            return Ok(vec![Inst::PushArg(MemRef::Closure(Rc::new(code), new_link_size))]);
                        } else {
                            return Err(CompileError { kind: CompileErrorKind::BadLambdaSyntax })
                        }
                    } else {
                        self.compile_call(static_scope, args, link_size, datum)
                    }
                } else {
                    self.compile_call(static_scope, args, link_size, datum)
                },
            &Datum::Nil => Err(CompileError { kind: CompileErrorKind::NullEval }),
            &Datum::Sym(ref sym) => {
                if let Some(i) = range(0, args.len()).find(|&i| args[i] == *sym) {
                    return Ok(vec![Inst::PushArg(MemRef::Arg(i))])
                }

                // (0, static_scope[-1]), (1, static_scope[-2]), (2, static_scope[-3]), ...
                for (i, up_args) in static_scope.iter().rev().enumerate() {
                    for (j, arg) in up_args.iter().enumerate() {
                        if *arg == *sym {
                            if *link_size < i+1 {
                                *link_size = i+1;
                            }
                            return Ok(vec![Inst::PushArg(MemRef::UpValue(i, j))]);
                        }
                    }
                }

                match self.global_env.get(sym) {
                    Some(data) => match data {
                        &EnvVar::Syntax(_) =>
                            Err(CompileError { kind: CompileErrorKind::SyntaxReference }),
                        &EnvVar::PrimFunc(ref name, ref func) =>
                            Ok(vec![Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc(name.clone(), func.clone()))))]),
                        &EnvVar::Closure(ref closure) =>
                            Ok(vec![Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::Closure(closure.clone()))))])
                    },
                    None => 
                        Err(CompileError { kind: CompileErrorKind::UnboundVariable })
                }
            },
            _ => Ok(vec![Inst::PushArg(MemRef::Const(datum.clone()))])
        }
    }
}

impl fmt::Show for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "lambda")
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;
    use std::rc::Rc;
    use datum::Datum;
    use runtime::{Inst, MemRef, RuntimeData};
    use base::libbase;
    use primitive::PRIM_ADD;
    use super::Compiler;

    #[test]
    fn test_simple_expr() {
        let env = libbase();
        let mut compiler = Compiler::new(&env);
        let expected = Ok(vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", Rc::new(PRIM_ADD))))),
            Inst::PushArg(MemRef::Const(Datum::Num(1))),
            Inst::PushArg(MemRef::Const(Datum::Num(2))),
            Inst::Call(2),
            Inst::Return
        ]);
        let code = compiler.compile(&list![sym!("+"), num!(1), num!(2)]);
        assert_eq!(expected, code);
    }

    #[test]
    fn test_nested_expr() {
        let env = libbase();
        let mut compiler = Compiler::new(&env);
        let expected = Ok(vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", Rc::new(PRIM_ADD))))),
            Inst::PushArg(MemRef::Const(Datum::Num(3))),
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", Rc::new(PRIM_ADD))))),
            Inst::PushArg(MemRef::Const(Datum::Num(1))),
            Inst::PushArg(MemRef::Const(Datum::Num(2))),
            Inst::Call(2),
            Inst::Call(2),
            Inst::Return
        ]);
        let code = compiler.compile(&list![sym!("+"), num!(3), list![sym!("+"), num!(1), num!(2)]]);
        assert_eq!(expected, code);
    }

    #[test]
    fn test_lambda() {
        let env = libbase();
        let mut compiler = Compiler::new(&env);

        let f = vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", Rc::new(PRIM_ADD))))),
            Inst::PushArg(MemRef::Arg(0)),
            Inst::PushArg(MemRef::Const(Datum::Num(2))),
            Inst::Call(2),
            Inst::Return
        ];
        let expected = Ok(vec![
            Inst::PushArg(MemRef::Closure(Rc::new(f), 0)),
            Inst::PushArg(MemRef::Const(Datum::Num(1))),
            Inst::Call(1),
            Inst::Return
        ]);
        let code = compiler.compile(&list![
                           list![sym!("lambda"), list![sym!("x")],
                                list![sym!("+"), sym!("x"), num!(2)]],
                            num!(1)]);
        assert_eq!(expected, code);
    }

    #[test]
    fn test_upvalue() {
        let env = libbase();
        let mut compiler = Compiler::new(&env);

        let f = vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", Rc::new(PRIM_ADD))))),
            Inst::PushArg(MemRef::UpValue(0, 0)),
            Inst::PushArg(MemRef::Arg(0)),
            Inst::Call(2),
            Inst::Return
        ];
        let g = vec![
            Inst::PushArg(MemRef::Closure(Rc::new(f), 1)),
            Inst::Return
        ];
        let expected = Ok(vec![
            Inst::PushArg(MemRef::Closure(Rc::new(g), 0)),
            Inst::PushArg(MemRef::Const(Datum::Num(2))),
            Inst::Call(1),
            Inst::PushArg(MemRef::Const(Datum::Num(3))),
            Inst::Call(1),
            Inst::Return
        ]);

        // ((
        //   (lambda (x)            # = g
        //     (lambda (y) (+ x y)) # = f
        //   ) 2) 3)
        let code = compiler.compile(&list![
            list![
                list![sym!("lambda"), list![sym!("x")],
                    list![sym!("lambda"), list![sym!("y")],
                        list![sym!("+"), sym!("x"), sym!("y")]]],
                num!(2)],
            num!(3)
        ]);
        assert_eq!(expected, code)
    }
}
