use std::string::CowString;
use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;

use error::{CompileError, CompileErrorKind};
use datum::Datum;
use runtime::{Inst, MemRef, RDatum, RuntimeData};
use primitive::PrimFunc;

/// Syntax variables
#[derive(Copy, Clone, PartialEq)]
pub enum Syntax {
    /// `lambda`
    Lambda,

    /// `if`
    If,

    /// `let`
    Let,

    /// `let*`
    LetStar,

    /// `letrec`
    LetRec,

    /// `letrec*`
    LetRecStar,

    /// `set!`
    Set,

    /// `quote`
    Quote
}

/// Environment variables in the global environment
pub enum EnvVar {
    /// Syntax variables
    Syntax(Syntax),

    /// Primitive functions
    PrimFunc(&'static str, &'static (PrimFunc + 'static)),

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
    pub fn compile(&self, datum: &RDatum) -> Result<Vec<Inst>, CompileError> {
        let mut ctx = CodeGenContext {
            code: Vec::new(),
            link_size: 0
        };
        try!(self.compile_expr(&[], &[], &mut ctx, datum));
        ctx.code.push(Inst::Return);
        return Ok(ctx.code);
    }

    fn compile_call(&self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                    ctx: &mut CodeGenContext, datum: &RDatum)
            -> Result<(), CompileError>
    {
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

    fn compile_block(&self, static_scope: &[Vec<CowString<'static>>],
                     args: &[CowString<'static>], var_arg: bool, body: &RDatum)
            -> Result<CodeGenContext, CompileError>
    {
        let mut ctx = CodeGenContext {
            code: Vec::new(),
            link_size: 0
        };

        if body == &Datum::Nil {
            return Err(CompileError { kind: CompileErrorKind::EmptyBody });
        }

        if var_arg {
            // The last arg is variable argument list
            ctx.code.push(Inst::RollArgs(args.len()-1));
        }

        let mut first = true;

        for expr in body.iter() {
            if first {
                first = false;
            } else {
                ctx.code.push(Inst::DropArg);
            }

            if let Ok(e) = expr {
                try!(self.compile_expr(static_scope, args, &mut ctx, &e));
            } else {
                return Err(CompileError { kind: CompileErrorKind::DottedBody });
            }
        }

        ctx.code.push(Inst::Return);

        return Ok(ctx);
    }

    fn compile_if(&self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                  ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        let exprs:Vec<RDatum> = match tail.iter().collect() {
            Ok(e) => e,
            Err(()) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
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
            Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn get_form(&self, form: &RDatum)
            -> Result<(Vec<CowString<'static>>, Vec<RDatum>, RDatum), CompileError>
    {
        if let &Datum::Cons(ref ptr) = form {
            let (ref binding_form, ref body) = *ptr.borrow();
            let mut syms = Vec::new();
            let mut exprs = Vec::new();
            for b in binding_form.iter() {
                match b {
                    Ok(datum) => {
                        let binding:Vec<RDatum> = match datum.iter().collect() {
                            Ok(v) => v,
                            Err(()) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
                        };
                        match binding.as_slice() {
                            [Datum::Sym(ref sym), ref expr] => {
                                syms.push(sym.clone());
                                exprs.push(expr.clone());
                            },
                            _ => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
                        };
                    },
                    Err(()) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
                }
            }
            Ok((syms, exprs, body.clone()))
        } else {
            Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn compile_let(&self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                   ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        let (syms, exprs, body) = try!(self.get_form(tail));
        for expr in exprs.iter() {
            try!(self.compile_expr(static_scope, args, ctx, expr));
        }
        ctx.code.push(Inst::PushFrame(syms.len()));

        let new_scope = {
            let mut nenv = static_scope.to_vec();
            nenv.push(args.to_vec());
            nenv
        };

        if body == Datum::Nil {
            return Err(CompileError { kind: CompileErrorKind::EmptyBody });
        }

        for expr in body.iter() {
            match expr {
                Ok(e) => try!(self.compile_expr(new_scope.as_slice(), syms.as_slice(), ctx, &e)),
                Err(()) => return Err(CompileError { kind: CompileErrorKind::DottedBody })
            }
        }

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_let_star(&self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                        ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        let (syms, exprs, body) = try!(self.get_form(tail));

        ctx.code.push(Inst::PushFrame(0));

        let new_scope = {
            let mut nenv = static_scope.to_vec();
            nenv.push(args.to_vec());
            nenv
        };

        for (i, expr) in exprs.iter().enumerate() {
            try!(self.compile_expr(new_scope.as_slice(), &syms[0..i], ctx, expr));
        }

        ctx.code.push(Inst::SetArgSize(syms.len()));

        if body == Datum::Nil {
            return Err(CompileError { kind: CompileErrorKind::EmptyBody });
        }

        for expr in body.iter() {
            match expr {
                Ok(e) => try!(self.compile_expr(new_scope.as_slice(), syms.as_slice(), ctx, &e)),
                Err(()) => return Err(CompileError { kind: CompileErrorKind::DottedBody })
            }
        }

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_letrec(&self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                      ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        let (syms, exprs, body) = try!(self.get_form(tail));

        ctx.code.push(Inst::PushFrame(0));

        for _ in 0..exprs.len() {
            ctx.code.push(Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::Undefined))));
        }

        let new_scope = {
            let mut nenv = static_scope.to_vec();
            nenv.push(args.to_vec());
            nenv
        };

        for (i, expr) in exprs.iter().enumerate() {
            try!(self.compile_expr(new_scope.as_slice(), syms.as_slice(), ctx, expr));
            ctx.code.push(Inst::PopArg(MemRef::Arg(i)));
        }

        ctx.code.push(Inst::SetArgSize(syms.len()));

        if body == Datum::Nil {
            return Err(CompileError { kind: CompileErrorKind::EmptyBody });
        }

        for expr in body.iter() {
            match expr {
                Ok(e) => try!(self.compile_expr(new_scope.as_slice(), syms.as_slice(), ctx, &e)),
                Err(()) => return Err(CompileError { kind: CompileErrorKind::DottedBody })
            }
        }

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_lambda(&self, static_scope: &[Vec<CowString<'static>>],
                      args: &[CowString<'static>], ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        if let &Datum::Cons(ref ptr) = tail {
            let (ref cur_args, ref body) = *ptr.borrow();
            let new_scope = {
                let mut nenv = static_scope.to_vec();
                nenv.push(args.to_vec());
                nenv
            };

            let (new_args, var_arg) = {
                let mut nargs = Vec::new();
                let mut var_arg = false;
                let mut iter = cur_args.clone();

                loop {
                    let (val, next) = match iter {
                        Datum::Cons(ref ptr) => ptr.borrow().clone(),
                        Datum::Sym(ref s) => {
                            nargs.push(s.clone());
                            var_arg = true;
                            break;
                        },
                        Datum::Nil => {
                            break;
                        }
                        _ => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
                    };
                    match val {
                        Datum::Sym(ref s) => {
                            nargs.push(s.clone())
                        },
                        _ => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
                    }
                    iter = next;
                }

                (nargs, var_arg)
            };

            let block_ctx = try!(self.compile_block(
                    new_scope.as_slice(),
                    new_args.as_slice(),
                    var_arg,
                    body
            ));

            ctx.code.push(Inst::PushArg(MemRef::Closure(
                    Rc::new(block_ctx.code),
                    block_ctx.link_size
            )));

            return Ok(());
        } else {
            return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn find_var(&self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                ctx: &mut CodeGenContext, sym: &CowString<'static>)
            -> Result<MemRef, CompileError>
    {
        if let Some(i) = range(0, args.len()).find(|&i| args[i] == *sym) {
            return Ok(MemRef::Arg(i));
        }

        // (0, static_scope[-1]), (1, static_scope[-2]), (2, static_scope[-3]), ...
        for (i, up_args) in static_scope.iter().rev().enumerate() {
            for (j, arg) in up_args.iter().enumerate() {
                if *arg == *sym {
                    if ctx.link_size < i+1 {
                        ctx.link_size = i+1;
                    }
                    return Ok(MemRef::UpValue(i, j));
                }
            }
        }

        match self.global_env.get(sym) {
            Some(data) => match data {
                &EnvVar::Syntax(_) =>
                    Err(CompileError { kind: CompileErrorKind::SyntaxReference }),
                &EnvVar::PrimFunc(ref name, func) => {
                    Ok(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc(
                                        name.clone(),
                                        func
                    ))))
                },
                &EnvVar::Procedure(ref code) => {
                    Ok(MemRef::Closure(code.clone(), 0))
                }
            },
            None => 
                Err(CompileError { kind: CompileErrorKind::UnboundVariable })
        }
    }

    fn compile_set(&self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                   ctx: &mut CodeGenContext, formal: &RDatum)
            -> Result<(), CompileError>
    {
        let assignment:Vec<RDatum> = match formal.iter().collect() {
            Ok(v) => v,
            Err(()) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        };
        match assignment.as_slice() {
            [Datum::Sym(ref sym), ref expr] => {
                try!(self.compile_expr(static_scope, args, ctx, expr));
                let ptr = try!(self.find_var(static_scope, args, ctx, sym));
                ctx.code.push(Inst::PopArg(ptr));
                ctx.code.push(Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::Undefined))));
                Ok(())
            },
            _ =>
                return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn compile_quote(&self, ctx: &mut CodeGenContext, items: &RDatum)
            -> Result<(), CompileError>
    {
        let mut iter = items.iter();
        match iter.next() {
            Some(Ok(v)) => {
                match iter.next() {
                    None => {
                        ctx.code.push(Inst::PushArg(MemRef::Const(v.clone())));
                        Ok(())
                    },
                    Some(_) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
                }
            },
            _ => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn compile_expr(&self, static_scope: &[Vec<CowString<'static>>], args: &[CowString<'static>],
                    ctx: &mut CodeGenContext, datum: &RDatum)
            -> Result<(), CompileError>
    {
        match datum {
            &Datum::Cons(ref ptr) =>
                if let (Datum::Sym(ref n), ref t) = *ptr.borrow() {
                    match self.global_env.get(n) {
                        Some(&EnvVar::Syntax(ref syn)) => match syn {
                            &Syntax::Lambda =>
                                self.compile_lambda(static_scope, args, ctx, t),
                            &Syntax::If =>
                                self.compile_if(static_scope, args, ctx, t),
                            &Syntax::Let =>
                                self.compile_let(static_scope, args, ctx, t),
                            &Syntax::LetStar =>
                                self.compile_let_star(static_scope, args, ctx, t),
                            &Syntax::LetRec | &Syntax::LetRecStar =>
                                self.compile_letrec(static_scope, args, ctx, t),
                            &Syntax::Set =>
                                self.compile_set(static_scope, args, ctx, t),
                            &Syntax::Quote =>
                                self.compile_quote(ctx, t)
                        },
                        _ => self.compile_call(static_scope, args, ctx, datum)
                    }
                } else {
                    self.compile_call(static_scope, args, ctx, datum)
                },
            &Datum::Nil => Err(CompileError { kind: CompileErrorKind::NullEval }),
            &Datum::Sym(ref sym) => {
                let ptr = try!(self.find_var(static_scope, args, ctx, sym));
                ctx.code.push(Inst::PushArg(ptr));
                return Ok(());
            },
            _ => {
                ctx.code.push(Inst::PushArg(MemRef::Const(datum.clone())));
                Ok(())
            }
        }
    }
}

impl fmt::Debug for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Syntax::Lambda => write!(f, "lambda"),
            Syntax::If => write!(f, "if"),
            Syntax::Let => write!(f, "let"),
            Syntax::LetStar => write!(f, "let*"),
            Syntax::LetRec => write!(f, "letrec"),
            Syntax::LetRecStar => write!(f, "letrec*"),
            Syntax::Set => write!(f, "set!"),
            Syntax::Quote => write!(f, "quote")
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
    use number::Number;
    use super::Compiler;

    #[test]
    fn test_simple_expr() {
        let env = libbase();
        let compiler = Compiler::new(&env);
        let expected = Ok(vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", &PRIM_ADD)))),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(1 ,0)))),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(2 ,0)))),
            Inst::Call(2),
            Inst::Return
        ]);
        let code = compiler.compile(&list![sym!("+"), num!(1), num!(2)]);
        assert_eq!(expected, code);
    }

    #[test]
    fn test_nested_expr() {
        let env = libbase();
        let compiler = Compiler::new(&env);
        let expected = Ok(vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", &PRIM_ADD)))),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(3, 0)))),
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", &PRIM_ADD)))),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(1, 0)))),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(2, 0)))),
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
        let compiler = Compiler::new(&env);

        let f = vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", &PRIM_ADD)))),
            Inst::PushArg(MemRef::Arg(0)),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(2, 0)))),
            Inst::Call(2),
            Inst::Return
        ];
        let expected = Ok(vec![
            Inst::PushArg(MemRef::Closure(Rc::new(f), 0)),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(1, 0)))),
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
        let compiler = Compiler::new(&env);

        let f = vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", &PRIM_ADD)))),
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
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(2, 0)))),
            Inst::Call(1),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(3, 0)))),
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
