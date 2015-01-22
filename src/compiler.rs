use std::string::CowString;
use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::ops::Deref;

use error::{CompileError, CompileErrorKind, RuntimeError};
use datum::Datum;
use runtime::{Inst, MemRef, RDatum, RuntimeData};

/// Syntax variables
#[derive(Copy, Clone, PartialEq)]
pub enum Syntax {
    /// `lambda` syntax
    Lambda,

    /// `if` syntax
    If
}

/// Environment variables in the global environment
pub enum EnvVar {
    /// Syntax variables
    Syntax(Syntax),

    /// Primitive functions
    PrimFunc(&'static str, Rc<fn(&[RDatum]) -> Result<RDatum, RuntimeError>>),

    /// Compiled library functions
    Procedure(Rc<Vec<Inst>>)
}

/// Compiler compiles Datum into a bytecode evaluates it
pub struct Compiler<'g> {
    /// Global environment
    global_env: &'g HashMap<CowString<'static>, EnvVar>
}

struct CodeGenContext {
    code: Vec<Inst>,
    link_size: usize
}

impl<'g> Compiler<'g> {
    /// Creates a new compiler with given environment
    pub fn new<'a>(global_env: &'a HashMap<CowString<'static>, EnvVar>) -> Compiler<'a> {
        Compiler {
            global_env: global_env
        }
    }

    /// Compiles the datum into a bytecode evaluates it
    pub fn compile(&mut self, datum: &RDatum) -> Result<Vec<Inst>, CompileError> {
        debug!("compile({:?})", datum);
        let mut ctx = CodeGenContext {
            code: Vec::new(),
            link_size: 0
        };
        try!(self.compile_expr(&[], &[], &mut ctx, datum));
        ctx.code.push(Inst::Return);
        return Ok(ctx.code);
    }

    fn compile_call(&mut self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                    ctx: &mut CodeGenContext, datum: &RDatum)
            -> Result<(), CompileError>
    {
        debug!("compile_call({:?})", datum);
        let mut arg_count = 0;
        for d in datum.iter() {
            match d {
                Ok(d) => {
                    try!(self.compile_expr(static_scope, args, ctx, &d));
                    arg_count += 1;
                },
                Err(()) => return Err(CompileError { kind: CompileErrorKind::DottedEval })
            }
        }

        if arg_count == 0 {
            Err(CompileError { kind: CompileErrorKind::NullEval })
        } else {
            ctx.code.push(Inst::Call(arg_count - 1));
            Ok(())
        }
    }

    fn compile_block(&mut self, static_scope: &[Vec<CowString<'static>>],
                      args: &[CowString<'static>], body: &RDatum)
            -> Result<CodeGenContext, CompileError>
    {
        debug!("compile_block({:?})", body);
        let mut ctx = CodeGenContext {
            code: Vec::new(),
            link_size: 0
        };
        for expr in body.iter() {
            if let Ok(e) = expr {
                try!(self.compile_expr(static_scope, args, &mut ctx, &e));
            } else {
                return Err(CompileError { kind: CompileErrorKind::DottedBody });
            }
        }

        ctx.code.push(Inst::Return);

        return Ok(ctx);
    }

    fn compile_if(&mut self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                  ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        debug!("compile_if({:?})", tail);
        let exprs:Vec<RDatum> = match tail.iter().collect() {
            Ok(e) => e,
            Err(()) => return Err(CompileError { kind: CompileErrorKind::BadIfSyntax })
        };

        if exprs.len() == 2 || exprs.len() == 3 {
            let cond = &exprs[0];
            let then_expr = &exprs[1];

            try!(self.compile_expr(static_scope, args, ctx, cond));

            let cond_jump_pc = ctx.code.len();
            // placeholder to replace with JumpIfFalse
            ctx.code.push(Inst::Nop);

            try!(self.compile_expr(static_scope, args, ctx, then_expr));

            let jump_pc = ctx.code.len();
            // push placeholder to replace with Jump
            ctx.code.push(Inst::Nop);

            ctx.code[cond_jump_pc] = Inst::JumpIfFalse(ctx.code.len());

            if exprs.len() == 3 {
                let else_expr = &exprs[2];

                try!(self.compile_expr(static_scope, args, ctx, else_expr));
            } else {
                ctx.code.push(Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::Undefined))));
            }

            // Currently code[ctx.code.len()] is out of range, but Return will be pushed at the
            // end of code anyway
            ctx.code[jump_pc] = Inst::Jump(ctx.code.len());

            Ok(())
        } else {
            Err(CompileError { kind: CompileErrorKind::BadIfSyntax })
        }
    }

    fn compile_lambda(&mut self, static_scope: &[Vec<CowString<'static>>],
                      args: &[CowString<'static>], ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        debug!("compile_lambda({:?})", tail);
        if let &Datum::Cons(ref cur_args, ref body) = tail {
            let new_scope = {
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

            let block_ctx = try!(self.compile_block(
                    new_scope.as_slice(),
                    new_args.as_slice(),
                    body.borrow().deref()
            ));

            ctx.code.push(Inst::PushArg(MemRef::Closure(
                    Rc::new(block_ctx.code),
                    block_ctx.link_size
            )));

            return Ok(());
        } else {
            return Err(CompileError { kind: CompileErrorKind::BadLambdaSyntax })
        }
    }

    fn compile_expr(&mut self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                    ctx: &mut CodeGenContext, datum: &RDatum)
            -> Result<(), CompileError>
    {
        debug!("compile_expr({:?})", datum);
        match datum {
            &Datum::Cons(ref h, ref t) =>
                if let &Datum::Sym(ref n) = h.borrow().deref() {
                    match self.global_env.get(n) {
                        Some(&EnvVar::Syntax(Syntax::Lambda)) =>
                            self.compile_lambda(static_scope, args, ctx, t.borrow().deref()),
                        Some(&EnvVar::Syntax(Syntax::If)) =>
                            self.compile_if(static_scope, args, ctx, t.borrow().deref()),
                        _ =>
                            self.compile_call(static_scope, args, ctx, datum)
                    }
                } else {
                    self.compile_call(static_scope, args, ctx, datum)
                },
            &Datum::Nil => Err(CompileError { kind: CompileErrorKind::NullEval }),
            &Datum::Sym(ref sym) => {
                if let Some(i) = range(0, args.len()).find(|&i| args[i] == *sym) {
                    ctx.code.push(Inst::PushArg(MemRef::Arg(i)));
                    return Ok(());
                }

                // (0, static_scope[-1]), (1, static_scope[-2]), (2, static_scope[-3]), ...
                for (i, up_args) in static_scope.iter().rev().enumerate() {
                    for (j, arg) in up_args.iter().enumerate() {
                        if *arg == *sym {
                            if ctx.link_size < i+1 {
                                ctx.link_size = i+1;
                            }
                            ctx.code.push(Inst::PushArg(MemRef::UpValue(i, j)));
                            return Ok(());
                        }
                    }
                }

                match self.global_env.get(sym) {
                    Some(data) => match data {
                        &EnvVar::Syntax(_) =>
                            Err(CompileError { kind: CompileErrorKind::SyntaxReference }),
                        &EnvVar::PrimFunc(ref name, ref func) => {
                            ctx.code.push(Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc(
                                                name.clone(),
                                                func.clone()
                            )))));
                            Ok(())
                        },
                        &EnvVar::Procedure(ref code) => {
                            ctx.code.push(Inst::PushArg(MemRef::Closure(code.clone(), 0)));
                            Ok(())
                        }
                    },
                    None => 
                        Err(CompileError { kind: CompileErrorKind::UnboundVariable })
                }
            },
            _ => {
                ctx.code.push(Inst::PushArg(MemRef::Const(datum.clone())));
                Ok(())
            }
        }
    }
}

impl fmt::Show for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Syntax::Lambda => write!(f, "lambda"),
            Syntax::If => write!(f, "if")
        }
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
