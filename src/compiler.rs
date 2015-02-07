use std::string::CowString;
use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::num::FromPrimitive;

use error::{CompileError, CompileErrorKind};
use datum::Datum;
use runtime::{Inst, MemRef, RDatum, RuntimeData};
use primitive::PrimFunc;

/// Syntax variables
#[derive(Copy, Clone, PartialEq, FromPrimitive)]
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

#[derive(Copy)]
pub struct SyntaxIter {
    index: usize
}

impl Iterator for SyntaxIter {
    type Item = Syntax;

    fn next(&mut self) -> Option<Syntax> {
        let res = FromPrimitive::from_uint(self.index);
        self.index += 1;
        res
    }
}

impl Syntax {
    pub fn iter() -> SyntaxIter {
        SyntaxIter { index: 0 }
    }

    pub fn name(&self) -> &'static str {
        match self {
            &Syntax::Lambda => "lambda",
            &Syntax::If => "if",
            &Syntax::Let => "let",
            &Syntax::LetStar => "let*",
            &Syntax::LetRec => "letrec",
            &Syntax::LetRecStar => "letrec*",
            &Syntax::Set => "set!",
            &Syntax::Quote => "quote"
        }
    }
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

struct LexicalContext {
    static_scope: Vec<Vec<CowString<'static>>>,
    args: Vec<CowString<'static>>
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
        let env = LexicalContext {
            static_scope: Vec::new(),
            args: Vec::new()
        };
        try!(self.compile_expr(&env, &mut ctx, datum));
        ctx.code.push(Inst::Return);
        return Ok(ctx.code);
    }

    fn compile_app(&self, env: &LexicalContext, ctx: &mut CodeGenContext, datum: &RDatum)
            -> Result<(), CompileError>
    {
        let (callee, c_args) = match datum {
            &Datum::Cons(ref ptr) => ptr.borrow().clone(),
            _ => return Err(CompileError { kind: CompileErrorKind::NullEval })
        };

        if let Datum::Sym(ref s) = callee {
            match self.compile_ref(env, ctx, s) {
                Ok(ptr) => ctx.code.push(Inst::PushArg(ptr)),
                Err(e) => match e.kind {
                    CompileErrorKind::SyntaxReference(syn) => {
                        return match syn {
                            Syntax::Lambda =>
                                self.compile_lambda(env, ctx, &c_args),
                            Syntax::If =>
                                self.compile_if(env, ctx, &c_args),
                            Syntax::Let =>
                                self.compile_let(env, ctx, &c_args),
                            Syntax::LetStar =>
                                self.compile_let_star(env, ctx, &c_args),
                            Syntax::LetRec | Syntax::LetRecStar =>
                                self.compile_letrec(env, ctx, &c_args),
                            Syntax::Set =>
                                self.compile_set(env, ctx, &c_args),
                            Syntax::Quote =>
                                self.compile_quote(ctx, &c_args)
                        };
                    },
                    _ => return Err(e)
                }
            }
        } else {
            try!(self.compile_expr(env, ctx, &callee));
        }

        let mut arg_count = 0;
        for d in c_args.iter() {
            match d {
                Ok(d) => {
                    try!(self.compile_expr(env, ctx, &d));
                    arg_count += 1;
                },
                Err(()) => return Err(CompileError { kind: CompileErrorKind::DottedEval })
            }
        }

        ctx.code.push(Inst::Call(arg_count));
        Ok(())
    }

    fn compile_body(&self, env: &LexicalContext, ctx: &mut CodeGenContext, body: &RDatum)
            -> Result<(), CompileError>
    {
        if body == &Datum::Nil {
            return Err(CompileError { kind: CompileErrorKind::EmptyBody });
        }

        let mut first = true;

        for expr in body.iter() {
            if first {
                first = false;
            } else {
                ctx.code.push(Inst::DropArg);
            }

            if let Ok(e) = expr {
                try!(self.compile_expr(env, ctx, &e));
            } else {
                return Err(CompileError { kind: CompileErrorKind::DottedBody });
            }
        }

        Ok(())
    }

    fn compile_block(&self, env: &LexicalContext, var_arg: bool, body: &RDatum)
            -> Result<CodeGenContext, CompileError>
    {
        let mut ctx = CodeGenContext {
            code: Vec::new(),
            link_size: 0
        };

        if var_arg {
            // The last arg is variable argument list
            ctx.code.push(Inst::RollArgs(env.args.len()-1));
        }

        try!(self.compile_body(env, &mut ctx, body));

        ctx.code.push(Inst::Return);

        return Ok(ctx);
    }

    fn compile_if(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        let exprs:Vec<RDatum> = match tail.iter().collect() {
            Ok(e) => e,
            Err(()) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        };

        if exprs.len() == 2 || exprs.len() == 3 {
            let cond = &exprs[0];
            let then_expr = &exprs[1];

            try!(self.compile_expr(env, ctx, cond));

            let cond_jump_pc = ctx.code.len();
            // placeholder to replace with JumpIfFalse
            ctx.code.push(Inst::Nop);

            try!(self.compile_expr(env, ctx, then_expr));

            let jump_pc = ctx.code.len();
            // push placeholder to replace with Jump
            ctx.code.push(Inst::Nop);

            ctx.code[cond_jump_pc] = Inst::JumpIfFalse(ctx.code.len());

            if exprs.len() == 3 {
                let else_expr = &exprs[2];

                try!(self.compile_expr(env, ctx, else_expr));
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

    fn compile_let(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        let (syms, exprs, body) = try!(self.get_form(tail));
        for expr in exprs.iter() {
            try!(self.compile_expr(env, ctx, expr));
        }
        ctx.code.push(Inst::PushFrame(syms.len()));

        let new_env = LexicalContext {
            static_scope: env.static_scope.clone() + &[env.args.clone()],
            args: syms
        };

        try!(self.compile_body(&new_env, ctx, &body));

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_let_star(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        let (syms, exprs, body) = try!(self.get_form(tail));

        ctx.code.push(Inst::PushFrame(0));

        let mut new_env = LexicalContext {
            static_scope: env.static_scope.clone() + &[env.args.clone()],
            args: Vec::new()
        };

        for (i, expr) in exprs.iter().enumerate() {
            new_env.args = syms[0..i].to_vec();
            try!(self.compile_expr(&new_env, ctx, expr));
        }

        ctx.code.push(Inst::SetArgSize(syms.len()));

        new_env.args = syms;
        try!(self.compile_body(&new_env, ctx, &body));

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_letrec(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        let (syms, exprs, body) = try!(self.get_form(tail));

        ctx.code.push(Inst::PushFrame(0));

        for _ in 0..exprs.len() {
            ctx.code.push(Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::Undefined))));
        }

        let arg_size = syms.len();
        let new_env = LexicalContext {
            static_scope: env.static_scope.clone() + &[env.args.clone()],
            args: syms
        };

        for (i, expr) in exprs.iter().enumerate() {
            try!(self.compile_expr(&new_env, ctx, expr));
            ctx.code.push(Inst::PopArg(MemRef::Arg(i)));
        }

        ctx.code.push(Inst::SetArgSize(arg_size));

        try!(self.compile_body(&new_env, ctx, &body));

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_lambda(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        if let &Datum::Cons(ref ptr) = tail {
            let (ref cur_args, ref body) = *ptr.borrow();
            let new_scope = env.static_scope.clone() + &[env.args.clone()];

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

            let new_env = LexicalContext {
                static_scope: new_scope,
                args: new_args
            };
            let block_ctx = try!(self.compile_block(&new_env, var_arg, body));

            ctx.code.push(Inst::PushArg(MemRef::Closure(
                    Rc::new(block_ctx.code),
                    block_ctx.link_size
            )));

            return Ok(());
        } else {
            return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn compile_ref(&self, env: &LexicalContext, ctx: &mut CodeGenContext, sym: &CowString<'static>)
            -> Result<MemRef, CompileError>
    {
        let ptr = self.find_var(env, sym);
        match ptr {
            Ok(MemRef::UpValue(i, _)) => {
                if ctx.link_size < i+1 {
                    ctx.link_size = i+1;
                }
            },
            _ => ()
        };
        return ptr;
    }

    fn find_var(&self, env: &LexicalContext, sym: &CowString<'static>)
            -> Result<MemRef, CompileError>
    {
        if let Some(i) = (0..env.args.len()).find(|&i| env.args[i] == *sym) {
            return Ok(MemRef::Arg(i));
        }

        // (0, static_scope[-1]), (1, static_scope[-2]), (2, static_scope[-3]), ...
        for (i, up_args) in env.static_scope.iter().rev().enumerate() {
            for (j, arg) in up_args.iter().enumerate() {
                if *arg == *sym {
                    return Ok(MemRef::UpValue(i, j));
                }
            }
        }

        match self.global_env.get(sym) {
            Some(data) => match data {
                &EnvVar::Syntax(ref s) =>
                    Err(CompileError { kind: CompileErrorKind::SyntaxReference(s.clone()) }),
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

    fn compile_set(&self, env: &LexicalContext, ctx: &mut CodeGenContext, formal: &RDatum)
            -> Result<(), CompileError>
    {
        let assignment:Vec<RDatum> = match formal.iter().collect() {
            Ok(v) => v,
            Err(()) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        };
        match assignment.as_slice() {
            [Datum::Sym(ref sym), ref expr] => {
                try!(self.compile_expr(env, ctx, expr));
                let ptr = try!(self.compile_ref(env, ctx, sym));
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

    fn compile_expr(&self, env: &LexicalContext, ctx: &mut CodeGenContext, datum: &RDatum)
            -> Result<(), CompileError>
    {
        match datum {
            &Datum::Cons(_) =>
                self.compile_app(env, ctx, datum),
            &Datum::Nil => Err(CompileError { kind: CompileErrorKind::NullEval }),
            &Datum::Sym(ref sym) => {
                let ptr = try!(self.compile_ref(env, ctx, sym));
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
        write!(f, "{}", self.name())
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
