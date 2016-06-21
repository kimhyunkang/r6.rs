use std::borrow::Cow;
use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;

use num::FromPrimitive;

use error::{CompileError, CompileErrorKind};
use datum::Datum;
use primitive::{PRIM_APPEND, PRIM_CONS, PRIM_LIST, PRIM_VECTOR};
use runtime::{SimpleDatum, Inst, MemRef, PrimFuncPtr, RDatum};

/// Syntax variables
enum_from_primitive! {
    #[derive(Copy, Clone, PartialEq)]
    pub enum Syntax {
        Lambda = 0, // `lambda`
        If = 1, // `if`
        Let = 2, // `let`
        LetStar = 3, // `let*`
        LetRec = 4, // `letrec`
        LetRecStar = 5, // `letrec*`
        Define = 6, // `define`
        Set = 7, // `set!`
        Quote = 8, // `quote`
        Quasiquote = 9, // `quasiquote`
        Unquote = 10, // `unquote`
        UnquoteSplicing = 11, // `unquote`
        Cond = 12, // `cond`
        Case = 13, // `case`
        And = 14, // `and`
        Or = 15, // `or`
    }
}

#[derive(Clone, Copy)]
pub struct SyntaxIter {
    index: usize
}

impl Iterator for SyntaxIter {
    type Item = Syntax;

    fn next(&mut self) -> Option<Syntax> {
        let res = Syntax::from_usize(self.index);
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
            &Syntax::Quote => "quote",
            &Syntax::Quasiquote => "quasiquote",
            &Syntax::Unquote => "unquote",
            &Syntax::UnquoteSplicing => "unquote-splicing",
            &Syntax::Cond => "cond",
            &Syntax::Case => "case",
            &Syntax::And => "and",
            &Syntax::Or => "or"
        }
    }
}

/// Environment variables in the global environment
pub enum EnvVar {
    /// Syntax variables
    Syntax(Syntax),

    /// Primitive functions
    PrimFunc(PrimFuncPtr),

    /// Compiled library functions
    Procedure(Rc<Vec<Inst>>)
}

/// Compiler compiles Datum into a bytecode evaluates it
pub struct Compiler<'g> {
    /// Global environment
    global_env: &'g HashMap<Cow<'static, str>, EnvVar>
}

struct CodeGenContext {
    code: Vec<Inst>,
    link_size: usize
}

#[derive(Clone)]
struct LexicalContext {
    static_scope: Vec<Vec<Cow<'static, str>>>,
    args: Vec<Cow<'static, str>>
}

impl LexicalContext {
    fn update_arg(&self, args: Vec<Cow<'static, str>>) -> LexicalContext {
        let mut scope = self.static_scope.clone();
        scope.push(self.args.clone());

        LexicalContext {
            static_scope: scope,
            args: args
        }
    }

    fn push_arg(&mut self, arg: Cow<'static, str>) {
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
    pub fn new<'a>(global_env: &'a HashMap<Cow<'static, str>, EnvVar>) -> Compiler<'a> {
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
            &Datum::Cons(ref ptr) => ptr.as_ref().clone(),
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
                                self.compile_quote(ctx, &c_args),
                            Syntax::Quasiquote =>
                                self.compile_quasiquote(env, ctx, &c_args),
                            Syntax::Unquote | Syntax::UnquoteSplicing =>
                                return Err(CompileError {
                                    kind: CompileErrorKind::UnquoteContext
                                }),
                            Syntax::Cond =>
                                self.compile_cond(env, ctx, &c_args),
                            Syntax::Case =>
                                self.compile_case(env, ctx, &c_args),
                            Syntax::And =>
                                self.compile_and(env, ctx, &c_args),
                            Syntax::Or =>
                                self.compile_or(env, ctx, &c_args)
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

    fn get_syntax_name(&self, env: &LexicalContext, item: &RDatum) -> Option<Syntax> {
        if let &Datum::Sym(ref sym) = item {
            if let Err(e) = self.find_var(env, sym) {
                if let CompileErrorKind::SyntaxReference(syntax) = e.kind {
                    return Some(syntax);
                }
            }
        }

        return None;
    }

    fn parse_define(&self, env: &LexicalContext, def: &RDatum)
            -> Result<Option<(Cow<'static, str>, Def)>, CompileError>
    {
        let list: Vec<RDatum> = match def.iter().collect() {
            Ok(l) => l,
            _ => return Ok(None)
        };

        if list.len() == 0 {
            return Ok(None);
        }

        if let Some(Syntax::Define) = self.get_syntax_name(env, &list[0]) {
            match &list[1..] {
                &[Datum::Sym(ref v)] =>
                    Ok(Some((v.clone(), Def::Void))),
                &[Datum::Sym(ref v), ref e] =>
                    Ok(Some((v.clone(), Def::Expr(e.clone())))),
                &[Datum::Cons(ref form), ..] => {
                    if let Datum::Sym(ref v) = form.0 {
                        Ok(Some((
                            v.clone(),
                            Def::Proc(form.1.clone(), list[2..].to_vec())
                        )))
                    } else {
                        Err(CompileError {
                            kind: CompileErrorKind::BadSyntax
                        })
                    }
                },
                _ => Err(CompileError { kind: CompileErrorKind::BadSyntax })
            }
        } else {
            Ok(None)
        }
    }

    fn compile_body(&self, env: &LexicalContext, ctx: &mut CodeGenContext, body: &RDatum)
            -> Result<(), CompileError>
    {
        let res: Result<Vec<RDatum>, ()> = body.iter().collect();
        match res {
            Ok(exprs) => self.compile_exprs(env, ctx, exprs.as_ref()),
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
            ctx.code.push(Inst::PushArg(MemRef::Undefined));
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
                ctx.code.push(Inst::PushArg(MemRef::Undefined));
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
            -> Result<(Vec<Cow<'static, str>>, Vec<RDatum>, RDatum), CompileError>
    {
        if let &Datum::Cons(ref ptr) = form {
            let (ref binding_form, ref body) = *ptr.as_ref();
            let mut syms = Vec::new();
            let mut exprs = Vec::new();
            for b in binding_form.iter() {
                match b {
                    Ok(datum) => {
                        let binding:Vec<RDatum> = match datum.iter().collect() {
                            Ok(v) => v,
                            Err(()) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
                        };
                        if let &[Datum::Sym(ref sym), ref expr] = binding.as_slice() {
                            syms.push(sym.clone());
                            exprs.push(expr.clone());
                        } else {
                            return Err(CompileError { kind: CompileErrorKind::BadSyntax });
                        }
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
            ctx.code.push(Inst::PushArg(MemRef::Undefined));
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
            let (ref cur_args, ref body) = *ptr.as_ref();
            let res: Result<Vec<RDatum>, ()> = body.iter().collect();
            match res {
                Ok(exprs) => {
                    let block_ctx = try!(self.compile_proc(env, cur_args, exprs.as_ref()));

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
                    Datum::Cons(ref ptr) => ptr.as_ref().clone(),
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

    fn compile_ref(&self, env: &LexicalContext, ctx: &mut CodeGenContext, sym: &Cow<'static, str>)
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

    fn find_var(&self, env: &LexicalContext, sym: &Cow<'static, str>)
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
                &EnvVar::PrimFunc(ref fptr) =>
                    Ok(MemRef::PrimFunc(fptr.clone())),
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
        if let &[Datum::Sym(ref sym), ref expr] = assignment.as_slice() {
            try!(self.compile_expr(env, ctx, expr));
            let ptr = try!(self.compile_ref(env, ctx, sym));
            ctx.code.push(Inst::PopArg(ptr));
            ctx.code.push(Inst::PushArg(MemRef::Undefined));
            Ok(())
        } else {
            Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn compile_quote(&self, ctx: &mut CodeGenContext, items: &RDatum)
            -> Result<(), CompileError>
    {
        let mut iter = items.iter();
        match iter.next() {
            Some(Ok(v)) => {
                match iter.next() {
                    None => self.rec_quote(ctx, &v),
                    Some(_) => Err(CompileError { kind: CompileErrorKind::BadSyntax })
                }
            },
            _ => Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn rec_quote(&self, ctx: &mut CodeGenContext, v: &RDatum) -> Result<(), CompileError> {
        match v {
            &Datum::Cons(ref pair) => {
                ctx.code.push(Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("cons", &PRIM_CONS))));
                try!(self.rec_quote(ctx, &pair.0));
                try!(self.rec_quote(ctx, &pair.1));
                ctx.code.push(Inst::Call(2));
            },
            &Datum::Vector(ref v) => {
                ctx.code.push(Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("vector", &PRIM_VECTOR))));
                for e in v.iter() {
                    try!(self.rec_quote(ctx, e));
                }
                ctx.code.push(Inst::Call(v.len()));
            },
            _ => match SimpleDatum::from_datum(v.clone()) {
                Some(c) => {
                    ctx.code.push(Inst::PushArg(MemRef::Const(c)));
                },
                None => return Err(CompileError { kind: CompileErrorKind::NotImplemented })
            }
        }

        Ok(())
    }

    fn compile_quasiquote(&self, env: &LexicalContext, ctx: &mut CodeGenContext, items: &RDatum)
            -> Result<(), CompileError>
    {
        let mut iter = items.iter();
        match iter.next() {
            Some(Ok(v)) => {
                match iter.next() {
                    None => self.rec_quasiquote(0, env, ctx, &v),
                    Some(_) => Err(CompileError { kind: CompileErrorKind::BadSyntax })
                }
            },
            _ => Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn get_syntax1(&self, v: &RDatum) -> Option<(Syntax, RDatum)> {
        let res: Result<Vec<RDatum>, ()> = v.iter().collect();
        let list = match res {
            Ok(list) => list,
            Err(_) => return None
        };

        if let &[Datum::Sym(ref ptr), ref arg] = list.as_slice() {
            match ptr.as_ref() {
                "quasiquote" => Some((Syntax::Quasiquote, arg.clone())),
                "unquote" => Some((Syntax::Unquote, arg.clone())),
                "unquote-splicing" => Some((Syntax::UnquoteSplicing, arg.clone())),
                _ => None
            }
        } else {
            None
        }
    }

    fn rec_quasiquote(&self, qq_level: usize, env: &LexicalContext, ctx: &mut CodeGenContext, v: &RDatum)
            -> Result<(), CompileError>
    {
        match self.get_syntax1(v) {
            Some((Syntax::Quasiquote, arg)) => {
                ctx.code.push(Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("list", &PRIM_LIST))));
                ctx.code.push(Inst::PushArg(MemRef::Const(SimpleDatum::Sym(Cow::Borrowed("quasiquote")))));
                try!(self.rec_quasiquote(qq_level+1, env, ctx, &arg));
                ctx.code.push(Inst::Call(2));
            },
            Some((Syntax::Unquote, arg)) => {
                if qq_level == 0 {
                    try!(self.compile_expr(env, ctx, &arg));
                } else {
                    ctx.code.push(Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("list", &PRIM_LIST))));
                    ctx.code.push(Inst::PushArg(MemRef::Const(SimpleDatum::Sym(Cow::Borrowed("unquote")))));
                    try!(self.rec_quasiquote(qq_level-1, env, ctx, &arg));
                    ctx.code.push(Inst::Call(2));
                }
            },
            _ => {
                match v {
                    &Datum::Cons(ref pair) => {
                        if let Some((Syntax::UnquoteSplicing, arg)) = self.get_syntax1(&pair.0) {
                            if qq_level == 0 {
                                ctx.code.push(Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("append", &PRIM_APPEND))));
                                try!(self.compile_expr(env, ctx, &arg));
                            } else {
                                ctx.code.push(Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("cons", &PRIM_CONS))));
                                try!(self.rec_quasiquote(qq_level-1, env, ctx, &pair.0));
                            }
                        } else {
                            ctx.code.push(Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("cons", &PRIM_CONS))));
                            try!(self.rec_quasiquote(qq_level, env, ctx, &pair.0));
                        }

                        try!(self.rec_quasiquote(qq_level, env, ctx, &pair.1));
                        ctx.code.push(Inst::Call(2));
                    },
                    &Datum::Vector(ref v) => {
                        ctx.code.push(Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("vector", &PRIM_VECTOR))));
                        for e in v.iter() {
                            try!(self.rec_quasiquote(qq_level, env, ctx, e));
                        }
                        ctx.code.push(Inst::Call(v.len()));
                    },
                    _ => match SimpleDatum::from_datum(v.clone()) {
                        Some(c) => {
                            ctx.code.push(Inst::PushArg(MemRef::Const(c)));
                        },
                        None => return Err(CompileError { kind: CompileErrorKind::NotImplemented })
                    }
                }
            }
        }

        Ok(())
    }

    fn is_sym(&self, datum: &RDatum, sym: &str) -> bool {
        if let &Datum::Sym(ref s) = datum {
            s.as_ref() == sym
        } else {
            false
        }
    }

    fn get_else_clause(&self, clauses: &mut Vec<RDatum>)
            -> Result<Option<Vec<RDatum>>, CompileError>
    {
        let else_exprs = match clauses.last() {
            None => return Err(CompileError { kind: CompileErrorKind::BadSyntax }),
            Some(last_clause) => match last_clause {
                &Datum::Cons(ref pair) =>  {
                    if self.is_sym(&pair.0, "else") {
                        let exprs: Vec<RDatum> = match pair.1.iter().collect() {
                            Ok(v) => v,
                            Err(_) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
                        };
                        Some(exprs)
                    } else {
                        None
                    }
                },
                _ => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
            }
        };

        if else_exprs.is_some() {
            clauses.pop();
        }

        Ok(else_exprs)
    }

    fn compile_cond(&self, env: &LexicalContext, ctx: &mut CodeGenContext, preds: &RDatum)
            -> Result<(), CompileError>
    {
        let mut placeholders = Vec::new();
        let mut clauses: Vec<RDatum> = match preds.iter().collect() {
            Ok(v) => v,
            Err(_) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        };

        let else_exprs = try!(self.get_else_clause(&mut clauses));

        for clause in clauses.into_iter() {
            let terms: Vec<RDatum> = match clause.iter().collect() {
                Ok(v) => v,
                Err(_) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
            };

            if terms.len() < 2 {
                return Err(CompileError { kind: CompileErrorKind::BadSyntax });
            }

            try!(self.compile_expr(env, ctx, &terms[0]));

            // placeholder for JumpIfFalse
            let jump_inst = ctx.code.len();
            ctx.code.push(Inst::Nop);

            if self.is_sym(&terms[1], "=>") {
                if terms.len() != 3 {
                    return Err(CompileError { kind: CompileErrorKind::BadSyntax });
                }

                try!(self.compile_expr(env, ctx, &terms[2]));
                ctx.code.push(Inst::SwapArg);
                ctx.code.push(Inst::Call(1));
            } else {
                ctx.code.push(Inst::DropArg);
                try!(self.compile_exprs(env, ctx, &terms[1..]));
            }

            // placeholder for Jump: this jumps to the end of cond expr
            placeholders.push(ctx.code.len());
            ctx.code.push(Inst::Nop);

            // JumpIfFalse jumps here
            let jump_pos = ctx.code.len();
            ctx.code[jump_inst] = Inst::JumpIfFalse(jump_pos);

            ctx.code.push(Inst::DropArg);
        }

        if let Some(exprs) = else_exprs {
            try!(self.compile_exprs(env, ctx, &exprs));
        }

        let pos = ctx.code.len();
        for inst in placeholders.into_iter() {
            ctx.code[inst] = Inst::Jump(pos);
        }

        Ok(())
    }

    fn compile_case(&self, env: &LexicalContext, ctx: &mut CodeGenContext, preds: &RDatum)
            -> Result<(), CompileError>
    {
        let mut placeholders = Vec::new();
        let mut clauses: Vec<RDatum> = match preds.iter().collect() {
            Ok(v) => v,
            Err(_) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        };

        if clauses.len() < 2 {
            return Err(CompileError { kind: CompileErrorKind::BadSyntax });
        }

        let expr = clauses.remove(0);

        try!(self.compile_expr(env, ctx, &expr));

        let else_exprs = try!(self.get_else_clause(&mut clauses));

        for clause in clauses.into_iter() {
            let mut case_placeholders = Vec::new();

            let terms: Vec<RDatum> = match clause.iter().collect() {
                Ok(v) => v,
                Err(_) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
            };

            if terms.len() < 2 {
                return Err(CompileError { kind: CompileErrorKind::BadSyntax });
            }

            let cases: Vec<RDatum> = match terms[0].iter().collect() {
                Ok(v) => v,
                Err(_) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
            };

            for case in cases.iter() {
                try!(self.rec_quote(ctx, case));

                ctx.code.push(Inst::Eqv);

                // placeholder for JumpIfNotFalse
                case_placeholders.push(ctx.code.len());
                ctx.code.push(Inst::Nop);

                ctx.code.push(Inst::DropArg);
                ctx.code.push(Inst::DropArg);
            }

            // Case not matches: Jump to next case
            let no_match_jump_pos = ctx.code.len();
            ctx.code.push(Inst::Nop);

            // Case matches: JumpIfNotFalse jumps here
            let match_case = ctx.code.len();
            for pos in case_placeholders.into_iter() {
                ctx.code[pos] = Inst::JumpIfNotFalse(match_case);
            }

            ctx.code.push(Inst::DropArg);
            ctx.code.push(Inst::DropArg);
            ctx.code.push(Inst::DropArg);

            try!(self.compile_exprs(env, ctx, &terms[1..]));

            // placeholder for Jump: to the end of case expression
            placeholders.push(ctx.code.len());
            ctx.code.push(Inst::Nop);

            // Next case: no_match_jump comes here
            let next_case = ctx.code.len();
            ctx.code[no_match_jump_pos] = Inst::Jump(next_case);
        }

        if let Some(exprs) = else_exprs {
            try!(self.compile_exprs(env, ctx, &exprs));
        }

        let pos = ctx.code.len();
        for inst in placeholders.into_iter() {
            ctx.code[inst] = Inst::Jump(pos);
        }

        Ok(())
    }

    fn compile_and(&self, env: &LexicalContext, ctx: &mut CodeGenContext, preds: &RDatum)
            -> Result<(), CompileError>
    {
        let mut placeholders = Vec::new();
        let exprs: Vec<RDatum> = match preds.iter().collect() {
            Ok(v) => v,
            Err(_) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        };
        if exprs.is_empty() {
            ctx.code.push(Inst::PushArg(MemRef::Const(SimpleDatum::Bool(true))));
            return Ok(());
        }

        try!(self.compile_expr(env, ctx, &exprs[0]));

        for expr in exprs[1..].iter() {
            placeholders.push(ctx.code.len());
            // placeholder for JumpIfFalse
            ctx.code.push(Inst::Nop);
            // Drop the value if the test fails
            ctx.code.push(Inst::DropArg);
            try!(self.compile_expr(env, ctx, &expr));
        }

        let jump_pc = ctx.code.len();
        for pc in placeholders.into_iter() {
            ctx.code[pc] = Inst::JumpIfFalse(jump_pc);
        }

        Ok(())
    }

    fn compile_or(&self, env: &LexicalContext, ctx: &mut CodeGenContext, preds: &RDatum)
            -> Result<(), CompileError>
    {
        let mut placeholders = Vec::new();
        let exprs: Vec<RDatum> = match preds.iter().collect() {
            Ok(v) => v,
            Err(_) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
        };
        if exprs.is_empty() {
            ctx.code.push(Inst::PushArg(MemRef::Const(SimpleDatum::Bool(false))));
            return Ok(());
        }

        try!(self.compile_expr(env, ctx, &exprs[0]));

        for expr in exprs[1..].iter() {
            placeholders.push(ctx.code.len());
            // placeholder for JumpIfNotFalse
            ctx.code.push(Inst::Nop);
            // Drop the value if the test fails
            ctx.code.push(Inst::DropArg);
            try!(self.compile_expr(env, ctx, &expr));
        }

        let jump_pc = ctx.code.len();
        for pc in placeholders.into_iter() {
            ctx.code[pc] = Inst::JumpIfNotFalse(jump_pc);
        }

        Ok(())
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
                Ok(())
            },
            &Datum::Vector(ref v) => {
                ctx.code.push(Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("vector", &PRIM_VECTOR))));
                for e in v.iter() {
                    try!(self.rec_quote(ctx, e));
                }
                ctx.code.push(Inst::Call(v.len()));
                Ok(())
            },
            _ => match SimpleDatum::from_datum(datum.clone()) {
                Some(c) => {
                    ctx.code.push(Inst::PushArg(MemRef::Const(c)));
                    Ok(())
                },
                None => {
                    Err(CompileError { kind: CompileErrorKind::BadSyntax })
                }
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
    use runtime::{Inst, MemRef, PrimFuncPtr, SimpleDatum};
    use base::libbase;
    use primitive::{PRIM_ADD, PRIM_CONS};
    use number::Number;
    use super::Compiler;

    #[test]
    fn test_simple_expr() {
        let env = libbase();
        let compiler = Compiler::new(&env);
        let expected = Ok(vec![
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1 ,0)))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2 ,0)))),
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
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(3, 0)))),
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1, 0)))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
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
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Arg(0)),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
            Inst::Call(2),
            Inst::Return
        ];
        let expected = Ok(vec![
            Inst::PushArg(MemRef::Closure(Rc::new(f), 0)),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1, 0)))),
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
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
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
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
            Inst::Call(1),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(3, 0)))),
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

    #[test]
    fn test_quote() {
        let env = libbase();
        let compiler = Compiler::new(&env);

        let expected = Ok(vec![
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("cons", &PRIM_CONS))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1, 0)))),
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("cons", &PRIM_CONS))),
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("cons", &PRIM_CONS))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("cons", &PRIM_CONS))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(3, 0)))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Nil)),
            Inst::Call(2),
            Inst::Call(2),
            Inst::PushArg(MemRef::Const(SimpleDatum::Nil)),
            Inst::Call(2),
            Inst::Call(2),
            Inst::Return
        ]);

        let code = compiler.compile(&list![sym!("quote"), list![num!(1), list![num!(2), num!(3)]]]);
        assert_eq!(expected, code)
    }
}
