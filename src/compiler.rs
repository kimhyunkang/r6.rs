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

    /// `define`
    Define,

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
            &Syntax::Define => "define",
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

#[derive(Clone)]
struct LexicalContext {
    static_scope: Vec<Vec<CowString<'static>>>,
    args: Vec<CowString<'static>>
}

impl LexicalContext {
    fn update_arg(&self, args: Vec<CowString<'static>>) -> LexicalContext {
        LexicalContext {
            static_scope: self.static_scope.clone() + &[self.args.clone()],
            args: args
        }
    }

    fn push_arg(&mut self, arg: CowString<'static>) {
        self.args.push(arg);
    }
}

enum Def {
    Proc(RDatum, Vec<RDatum>),
    Expr(RDatum),
    Void
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
                            Syntax::Define =>
                                return Err(CompileError {
                                    kind: CompileErrorKind::DefineContext
                                }),
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

    fn parse_define(&self, env: &LexicalContext, def: &RDatum)
            -> Result<Option<(CowString<'static>, Def)>, CompileError>
    {
        let list: Vec<RDatum> = match def.iter().collect() {
            Ok(l) => l,
            _ => return Ok(None)
        };

        if list.len() == 0 {
            return Ok(None);
        }

        if let Datum::Sym(ref sym) = list[0] {
            if let Err(e) = self.find_var(env, sym) {
                if let CompileErrorKind::SyntaxReference(Syntax::Define) = e.kind {
                    match &list[1..] {
                        [Datum::Sym(ref v)] =>
                            return Ok(Some((v.clone(), Def::Void))),
                        [Datum::Sym(ref v), ref e] =>
                            return Ok(Some((v.clone(), Def::Expr(e.clone())))),
                        [Datum::Cons(ref form), ..] => {
                            let pair = form.borrow();
                            if let Datum::Sym(ref v) = pair.0 {
                                return Ok(Some((
                                        v.clone(),
                                        Def::Proc(pair.1.clone(), list[2..].to_vec())
                                )));
                            } else {
                                return Err(CompileError {
                                    kind: CompileErrorKind::BadSyntax
                                })
                            }
                        },
                        _ =>
                            return Err(CompileError {
                                kind: CompileErrorKind::BadSyntax
                            })
                    }
                }
            }
        }

        Ok(None)
    }

    fn compile_body(&self, env: &LexicalContext, ctx: &mut CodeGenContext, body: &RDatum)
            -> Result<(), CompileError>
    {
        let res: Result<Vec<RDatum>, ()> = body.iter().collect();
        match res {
            Ok(exprs) => self.compile_exprs(env, ctx, exprs.as_slice()),
            Err(_) => Err(CompileError {
                kind: CompileErrorKind::DottedBody
            })
        }
    }

    fn compile_exprs(&self, env: &LexicalContext, ctx: &mut CodeGenContext, body: &[RDatum])
            -> Result<(), CompileError>
    {
        if body.is_empty() {
            return Err(CompileError { kind: CompileErrorKind::EmptyBody });
        }

        let mut def_vars = Vec::new();
        let mut defs = Vec::new();

        for expr in body.iter() {
            match try!(self.parse_define(env, &expr)) {
                Some((var, def)) => {
                    def_vars.push(var);
                    defs.push(def);
                },
                None => break
            }
        }

        let mut mod_env = env.clone();
        for var in def_vars.iter() {
            mod_env.push_arg(var.clone());
            ctx.code.push(Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::Undefined))));
        }

        for (i, def) in defs.iter().enumerate() {
            match def {
                &Def::Proc(ref formals, ref body) => {
                    let proc_ctx = try!(self.compile_proc(&mod_env, formals, body));
                    ctx.code.push(Inst::PushArg(MemRef::Closure(
                            Rc::new(proc_ctx.code),
                            proc_ctx.link_size
                    )));
                    ctx.code.push(Inst::PopArg(MemRef::Arg(env.args.len() + i)));
                },
                &Def::Expr(ref expr) => {
                    try!(self.compile_expr(&mod_env, ctx, expr));
                    ctx.code.push(Inst::PopArg(MemRef::Arg(env.args.len() + i)));
                },
                &Def::Void => ()
            }
        }

        ctx.code.push(Inst::SetArgSize(mod_env.args.len()));

        if def_vars.len() == body.len() {
            return Err(CompileError {
                kind: CompileErrorKind::EmptyBody
            });
        }

        let mut first = true;

        for expr in body[def_vars.len() ..].iter() {
            if first {
                first = false;
            } else {
                ctx.code.push(Inst::DropArg);
            }

            try!(self.compile_expr(&mod_env, ctx, &expr));
        }

        Ok(())
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
            ctx.code.push(Inst::DropArg);

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

        let new_env = env.update_arg(syms);

        try!(self.compile_body(&new_env, ctx, &body));

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_let_star(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        let (syms, exprs, body) = try!(self.get_form(tail));

        ctx.code.push(Inst::PushFrame(0));

        let mut new_env = env.update_arg(Vec::new());

        for (i, expr) in exprs.iter().enumerate() {
            new_env.args = syms[0..i].to_vec();
            try!(self.compile_expr(&new_env, ctx, expr));
        }

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

        let new_env = env.update_arg(syms);

        for (i, expr) in exprs.iter().enumerate() {
            try!(self.compile_expr(&new_env, ctx, expr));
            ctx.code.push(Inst::PopArg(MemRef::Arg(i)));
        }

        try!(self.compile_body(&new_env, ctx, &body));

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_lambda(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &RDatum)
            -> Result<(), CompileError>
    {
        if let &Datum::Cons(ref ptr) = tail {
            let (ref cur_args, ref body) = *ptr.borrow();
            let res: Result<Vec<RDatum>, ()> = body.iter().collect();
            match res {
                Ok(exprs) => {
                    let block_ctx = try!(self.compile_proc(env, cur_args, exprs.as_slice()));

                    ctx.code.push(Inst::PushArg(MemRef::Closure(
                            Rc::new(block_ctx.code),
                            block_ctx.link_size
                    )));

                    return Ok(());
                },
                Err(()) =>
                    Err(CompileError { kind: CompileErrorKind::DottedBody })
            }
        } else {
            return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn compile_proc(&self, env: &LexicalContext, formals: &RDatum, body: &[RDatum])
            -> Result<CodeGenContext, CompileError>
    {
        let (new_args, var_arg) = {
            let mut nargs = Vec::new();
            let mut var_arg = false;
            let mut iter = formals.clone();

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

        let mut ctx = CodeGenContext {
            code: Vec::new(),
            link_size: 0
        };

        if var_arg {
            // The last arg is variable argument list
            ctx.code.push(Inst::RollArgs(new_args.len()-1));
        }

        let new_env = env.update_arg(new_args);

        try!(self.compile_exprs(&new_env, &mut ctx, body));

        ctx.code.push(Inst::Return);

        return Ok(ctx);
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
            Inst::SetArgSize(1),
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
            Inst::SetArgSize(1),
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", &PRIM_ADD)))),
            Inst::PushArg(MemRef::UpValue(0, 0)),
            Inst::PushArg(MemRef::Arg(0)),
            Inst::Call(2),
            Inst::Return
        ];
        let g = vec![
            Inst::SetArgSize(1),
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
