use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Debug;
use std::ops::Deref;
use std::rc::Rc;

use num::FromPrimitive;
use immutable_map::TreeMap;

use error::{CompileError, CompileErrorKind};
use datum::{cons, Datum, TryConv, SimpleDatum};
use primitive::{PRIM_APPEND, PRIM_CONS, PRIM_LIST, PRIM_VECTOR};
use runtime::{Inst, MemRef, PrimFuncPtr, RDatum, RuntimeData};
use syntax::CompiledMacro;

/// Syntax variables
enum_from_primitive! {
    #[derive(Copy, Clone, PartialEq)]
    pub enum PrimitiveSyntax {
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
        SyntaxRules = 16, // `syntax-rules`
        LetSyntax = 17, // `let-syntax`
    }
}

#[derive(Clone, Copy)]
pub struct PrimitiveSyntaxIter {
    index: usize
}

#[derive(Debug, Clone, PartialEq)]
pub enum Syntax {
    Primitive(PrimitiveSyntax),
    Macro(Rc<CompiledMacro>)
}

struct Binding<T> {
    sym: Cow<'static, str>,
    expr: Datum<T>
}

impl<T> Binding<T> {
    fn new(sym: Cow<'static, str>, expr: Datum<T>) -> Binding<T> {
        Binding { sym, expr }
    }
}

impl Iterator for PrimitiveSyntaxIter {
    type Item = PrimitiveSyntax;

    fn next(&mut self) -> Option<PrimitiveSyntax> {
        let res = PrimitiveSyntax::from_usize(self.index);
        self.index += 1;
        res
    }
}

impl PrimitiveSyntax {
    pub fn iter() -> PrimitiveSyntaxIter {
        PrimitiveSyntaxIter { index: 0 }
    }

    pub fn name(&self) -> &'static str {
        match self {
            &PrimitiveSyntax::Lambda => "lambda",
            &PrimitiveSyntax::If => "if",
            &PrimitiveSyntax::Let => "let",
            &PrimitiveSyntax::LetStar => "let*",
            &PrimitiveSyntax::LetRec => "letrec",
            &PrimitiveSyntax::LetRecStar => "letrec*",
            &PrimitiveSyntax::Define => "define",
            &PrimitiveSyntax::Set => "set!",
            &PrimitiveSyntax::Quote => "quote",
            &PrimitiveSyntax::Quasiquote => "quasiquote",
            &PrimitiveSyntax::Unquote => "unquote",
            &PrimitiveSyntax::UnquoteSplicing => "unquote-splicing",
            &PrimitiveSyntax::Cond => "cond",
            &PrimitiveSyntax::Case => "case",
            &PrimitiveSyntax::And => "and",
            &PrimitiveSyntax::Or => "or",
            &PrimitiveSyntax::SyntaxRules => "syntax-rules",
            &PrimitiveSyntax::LetSyntax => "let-syntax"
        }
    }
}

/// Compiler compiles Datum into a bytecode evaluates it
pub struct Compiler {
    /// Syntax environment
    syntax_env: HashMap<Cow<'static, str>, PrimitiveSyntax>,
}

struct CodeGenContext {
    code: Vec<Inst>,
    link_size: usize
}

#[derive(Clone)]
struct LexicalContext<'g> {
    /// Current global environment
    global_env: &'g HashMap<Cow<'static, str>, Rc<RefCell<RDatum>>>,
    syntax_env: TreeMap<Cow<'static, str>, Rc<CompiledMacro>>,
    static_scope: Vec<Vec<Cow<'static, str>>>,
    args: Vec<Cow<'static, str>>
}

impl<'g> LexicalContext<'g> {
    fn update_arg(&self, args: Vec<Cow<'static, str>>) -> LexicalContext<'g> {
        let mut scope = self.static_scope.clone();
        scope.push(self.args.clone());

        LexicalContext {
            global_env: self.global_env,
            syntax_env: self.syntax_env.clone(),
            static_scope: scope,
            args
        }
    }

    fn push_arg(&mut self, arg: Cow<'static, str>) {
        self.args.push(arg);
    }
}

enum Def<T> {
    Proc(Datum<T>, Vec<Datum<T>>),
    Expr(Datum<T>),
    Void
}

fn to_list<T: Clone>(datum: &Datum<T>) -> Result<Vec<Datum<T>>, CompileError> {
    datum.iter().collect::<Result<Vec<Datum<T>>, ()>>().map_err(|_| CompileError { kind: CompileErrorKind::BadSyntax })
}

fn to_exprs<T: Clone>(datum: &Datum<T>) -> Result<Vec<Datum<T>>, CompileError> {
    datum.iter().collect::<Result<Vec<Datum<T>>, ()>>().map_err(|_| CompileError { kind: CompileErrorKind::DottedBody })
}

impl Compiler {
    /// Creates a new compiler with given environment
    pub fn new(syntax_env: HashMap<Cow<'static, str>, PrimitiveSyntax>) -> Compiler {
        Compiler { syntax_env }
    }

    /// Compiles the datum into a bytecode evaluates it
    pub fn compile<T>(&self,
                      global_env: &HashMap<Cow<'static, str>, Rc<RefCell<RDatum>>>,
                      datum: &Datum<T>)
            -> Result<Vec<Inst>, CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let mut ctx = CodeGenContext {
            code: Vec::new(),
            link_size: 0
        };
        let env = LexicalContext {
            global_env,
            syntax_env: TreeMap::new(),
            static_scope: Vec::new(),
            args: Vec::new()
        };
        self.compile_expr(&env, &mut ctx, true, datum)?;
        ctx.code.push(Inst::Return);
        return Ok(ctx.code);
    }

    fn compile_app<T>(&self,
                      env: &LexicalContext,
                      ctx: &mut CodeGenContext,
                      tail_ctx: bool,
                      datum: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let (callee, c_args) = match datum {
            &Datum::Cons(ref ptr) => ptr.as_ref().clone(),
            _ => return Err(CompileError { kind: CompileErrorKind::NullEval })
        };

        if let Datum::Sym(ref s) = callee {
            match self.compile_ref(env, ctx, s) {
                Ok(ptr) => ctx.code.push(Inst::PushArg(ptr)),
                Err(e) => match e.kind {
                    CompileErrorKind::SyntaxReference(Syntax::Primitive(syn)) => {
                        return match syn {
                            PrimitiveSyntax::Lambda =>
                                self.compile_lambda(env, ctx, &c_args),
                            PrimitiveSyntax::If =>
                                self.compile_if(env, ctx, tail_ctx, &c_args),
                            PrimitiveSyntax::Let =>
                                self.compile_let(env, ctx, &c_args),
                            PrimitiveSyntax::LetStar =>
                                self.compile_let_star(env, ctx, &c_args),
                            PrimitiveSyntax::LetRec | PrimitiveSyntax::LetRecStar =>
                                self.compile_letrec(env, ctx, &c_args),
                            PrimitiveSyntax::Define =>
                                self.compile_define_toplevel(env, ctx, &datum),
                            PrimitiveSyntax::Set =>
                                self.compile_set(env, ctx, &c_args),
                            PrimitiveSyntax::Quote =>
                                self.compile_quote(ctx, &c_args),
                            PrimitiveSyntax::Quasiquote =>
                                self.compile_quasiquote(env, ctx, &c_args),
                            PrimitiveSyntax::Unquote | PrimitiveSyntax::UnquoteSplicing =>
                                return Err(CompileError {
                                    kind: CompileErrorKind::UnquoteContext
                                }),
                            PrimitiveSyntax::Cond =>
                                self.compile_cond(env, ctx, tail_ctx, &c_args),
                            PrimitiveSyntax::Case =>
                                self.compile_case(env, ctx, tail_ctx, &c_args),
                            PrimitiveSyntax::And =>
                                self.compile_and(env, ctx, tail_ctx, &c_args),
                            PrimitiveSyntax::Or =>
                                self.compile_or(env, ctx, tail_ctx, &c_args),
                            PrimitiveSyntax::SyntaxRules =>
                                return Err(CompileError {
                                    kind: CompileErrorKind::SyntaxRulesContext
                                }),
                            PrimitiveSyntax::LetSyntax =>
                                self.compile_let_syntax(env, ctx, tail_ctx, &c_args),
                        };
                    },
                    CompileErrorKind::SyntaxReference(Syntax::Macro(syn)) => {
                        let expanded = syn.transform(&datum)?;
                        return self.compile_expr(env, ctx, tail_ctx, &expanded);
                    },
                    _ => return Err(e)
                }
            }
        } else {
            self.compile_expr(env, ctx, false, &callee)?;
        }

        let mut arg_count = 0;
        for d in c_args.iter() {
            match d {
                Ok(d) => {
                    self.compile_expr(env, ctx, false, &d)?;
                    arg_count += 1;
                },
                Err(()) => return Err(CompileError { kind: CompileErrorKind::DottedEval })
            }
        }

        if tail_ctx {
            ctx.code.push(Inst::TailCall);
        } else {
            ctx.code.push(Inst::Call(arg_count));
        }
        Ok(())
    }

    fn get_syntax_name<T>(&self, env: &LexicalContext, item: &Datum<T>) -> Option<PrimitiveSyntax> {
        if let &Datum::Sym(ref sym) = item {
            if let Err(e) = self.find_var(env, sym) {
                if let CompileErrorKind::SyntaxReference(Syntax::Primitive(syntax)) = e.kind {
                    return Some(syntax);
                }
            }
        }

        return None;
    }

    fn compile_define_toplevel<T>(&self,
                                  env: &LexicalContext,
                                  ctx: &mut CodeGenContext,
                                  expr: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        if let Some((var, def)) = self.parse_define(env, &expr)? {
            ctx.code.push(Inst::PushFrame(1));
            ctx.code.push(Inst::PushArg(MemRef::Undefined));
            let new_env = env.update_arg(vec![var.clone()]);

            match def {
                Def::Proc(formals, body) => {
                    let proc_ctx = self.compile_proc(&new_env, &formals, &body)?;
                    ctx.code.push(Inst::PushArg(MemRef::Closure(
                        Rc::new(proc_ctx.code),
                        proc_ctx.link_size,
                        Some(expr.try_conv()?)
                    )));
                },
                Def::Expr(expr) => {
                    self.compile_expr(&new_env, ctx, false, &expr)?;
                },
                Def::Void => {
                    ctx.code.push(Inst::PushArg(MemRef::Undefined));
                }
            }

            ctx.code.push(Inst::PopArg(MemRef::Arg(0)));
            ctx.code.push(Inst::PushArg(MemRef::Arg(0)));
            ctx.code.push(Inst::PopFrame);
            ctx.code.push(Inst::PopGlobal(var));
            ctx.code.push(Inst::PushArg(MemRef::Undefined));
            Ok(())
        } else {
            Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn parse_define<T: Clone+Debug>(&self, env: &LexicalContext, def: &Datum<T>)
            -> Result<Option<(Cow<'static, str>, Def<T>)>, CompileError>
    {
        let list: Vec<Datum<T>> = match def.iter().collect() {
            Ok(l) => l,
            _ => return Ok(None)
        };

        if list.len() == 0 {
            return Ok(None);
        }

        if let Some(PrimitiveSyntax::Define) = self.get_syntax_name(env, &list[0]) {
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

    fn compile_body<T>(&self, env: &LexicalContext, ctx: &mut CodeGenContext, body: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let res: Result<Vec<Datum<T>>, ()> = body.iter().collect();
        match res {
            Ok(exprs) => self.compile_exprs(env, ctx, false, exprs.as_ref()),
            Err(_) => Err(CompileError {
                kind: CompileErrorKind::DottedBody
            })
        }
    }

    fn compile_exprs<T>(&self,
                        env: &LexicalContext,
                        ctx: &mut CodeGenContext,
                        tail_ctx: bool,
                        body: &[Datum<T>])
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        if body.is_empty() {
            return Err(CompileError { kind: CompileErrorKind::EmptyBody });
        }

        let mut def_vars = Vec::new();
        let mut defs = Vec::new();
        let mut srcs = Vec::new();

        for expr in body.iter() {
            match self.parse_define(env, &expr)? {
                Some((var, def)) => {
                    def_vars.push(var);
                    defs.push(def);
                    srcs.push(expr);
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
                    let proc_ctx = self.compile_proc(&mod_env, formals, body)?;
                    ctx.code.push(Inst::PushArg(MemRef::Closure(
                            Rc::new(proc_ctx.code),
                            proc_ctx.link_size,
                            Some(srcs[i].try_conv()?)
                    )));
                    ctx.code.push(Inst::PopArg(MemRef::Arg(env.args.len() + i)));
                },
                &Def::Expr(ref expr) => {
                    self.compile_expr(&mod_env, ctx, false, expr)?;
                    ctx.code.push(Inst::PopArg(MemRef::Arg(env.args.len() + i)));
                },
                &Def::Void => ()
            }
        }

        if def_vars.len() == body.len() {
            return Err(CompileError {
                kind: CompileErrorKind::EmptyBody
            });
        }

        for idx in def_vars.len() .. body.len() {
            if idx != def_vars.len() {
                ctx.code.push(Inst::DropArg(1));
            }

            let tail_expr = tail_ctx && idx == body.len() - 1;
            self.compile_expr(&mod_env, ctx, tail_expr, &body[idx])?;
        }

        Ok(())
    }

    fn compile_if<T>(&self,
                     env: &LexicalContext,
                     ctx: &mut CodeGenContext,
                     tail_ctx: bool,
                     tail: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let exprs = to_list(tail)?;

        if exprs.len() == 2 || exprs.len() == 3 {
            let cond = &exprs[0];
            let then_expr = &exprs[1];

            self.compile_expr(env, ctx, false, cond)?;

            let cond_jump_pc = ctx.code.len();
            // placeholder to replace with JumpIfFalse
            ctx.code.push(Inst::Nop);
            ctx.code.push(Inst::DropArg(1));

            self.compile_expr(env, ctx, tail_ctx, then_expr)?;

            let jump_pc = ctx.code.len();
            // push placeholder to replace with Jump
            ctx.code.push(Inst::Nop);

            ctx.code[cond_jump_pc] = Inst::JumpIfFalse(ctx.code.len());
            ctx.code.push(Inst::DropArg(1));

            if exprs.len() == 3 {
                let else_expr = &exprs[2];

                self.compile_expr(env, ctx, tail_ctx, else_expr)?;
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

    fn get_form<T: Clone+Debug>(&self, form: &Datum<T>)
            -> Result<(Vec<Binding<T>>, Datum<T>), CompileError>
    {
        if let &Datum::Cons(ref ptr) = form {
            let (ref binding_form, ref body) = *ptr.as_ref();
            let mut bindings = Vec::new();
            for b in binding_form.iter() {
                match b {
                    Ok(datum) => {
                        let binding:Vec<Datum<T>> = match datum.iter().collect() {
                            Ok(v) => v,
                            Err(()) => return Err(CompileError {
                                kind: CompileErrorKind::BadSyntax
                            })
                        };
                        if let &[Datum::Sym(ref sym), ref expr] = binding.as_slice() {
                            bindings.push(Binding::new(sym.clone(), expr.clone()));
                        } else {
                            return Err(CompileError { kind: CompileErrorKind::BadSyntax });
                        }
                    },
                    Err(()) => return Err(CompileError { kind: CompileErrorKind::BadSyntax })
                }
            }
            Ok((bindings, body.clone()))
        } else {
            Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn compile_let<T>(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let (bindings, body) = self.get_form(tail)?;
        for binding in &bindings {
            self.compile_expr(env, ctx, false, &binding.expr)?;
        }
        ctx.code.push(Inst::PushFrame(bindings.len()));

        let syms = bindings.into_iter().map(|b| b.sym).collect();
        let new_env = env.update_arg(syms);

        self.compile_body(&new_env, ctx, &body)?;

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_let_star<T>(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let (bindings, body) = self.get_form(tail)?;

        ctx.code.push(Inst::PushFrame(0));

        let mut new_env = env.update_arg(Vec::new());

        let syms: Vec<Cow<'static, str>> = bindings.iter().map(|b| b.sym.clone()).collect();
        for (i, binding) in bindings.iter().enumerate() {
            new_env.args = syms[0..i].to_vec();
            self.compile_expr(&new_env, ctx, false, &binding.expr)?;
        }

        ctx.code.push(Inst::SetArgSize(syms.len()));

        new_env.args = syms;
        self.compile_body(&new_env, ctx, &body)?;

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_letrec<T>(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let (bindings, body) = self.get_form(tail)?;
        let syms = bindings.iter().map(|b| b.sym.clone()).collect();

        ctx.code.push(Inst::PushFrame(0));

        for _ in 0..bindings.len() {
            ctx.code.push(Inst::PushArg(MemRef::Undefined));
        }

        let new_env = env.update_arg(syms);

        for (i, binding) in bindings.iter().enumerate() {
            self.compile_expr(&new_env, ctx, false, &binding.expr)?;
            ctx.code.push(Inst::PopArg(MemRef::Arg(i)));
        }

        ctx.code.push(Inst::SetArgSize(bindings.len()));

        self.compile_body(&new_env, ctx, &body)?;

        ctx.code.push(Inst::PopFrame);

        Ok(())
    }

    fn compile_lambda<T>(&self, env: &LexicalContext, ctx: &mut CodeGenContext, tail: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        if let &Datum::Cons(ref ptr) = tail {
            let (ref cur_args, ref body) = *ptr.as_ref();
            let res: Result<Vec<Datum<T>>, ()> = body.iter().collect();
            let expr = cons(Datum::Sym(Cow::Borrowed("lambda")), tail.try_conv()?);
            match res {
                Ok(exprs) => {
                    let block_ctx = self.compile_proc(env, cur_args, exprs.as_ref())?;

                    ctx.code.push(Inst::PushArg(MemRef::Closure(
                            Rc::new(block_ctx.code),
                            block_ctx.link_size,
                            Some(expr)
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

    fn compile_proc<T>(&self, env: &LexicalContext, formals: &Datum<T>, body: &[Datum<T>])
            -> Result<CodeGenContext, CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
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

        self.compile_exprs(&new_env, &mut ctx, true, body)?;

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

        match env.global_env.get(sym) {
            Some(data) => match data.borrow().deref() {
                &Datum::Ext(RuntimeData::PrimFunc(ref fptr)) =>
                    Ok(MemRef::PrimFunc(fptr.clone())),
                _ =>
                    Ok(MemRef::Global(data.clone()))
            },
            None => {
                if let Some(syntax) = env.syntax_env.get(sym) {
                    Err(CompileError { kind: CompileErrorKind::SyntaxReference(Syntax::Macro(syntax.clone())) })
                } else if let Some(syntax) = self.syntax_env.get(sym) {
                    Err(CompileError { kind: CompileErrorKind::SyntaxReference(Syntax::Primitive(syntax.clone())) })
                } else {
                    Err(CompileError { kind: CompileErrorKind::UnboundVariable(sym.clone()) })
                }
            }
        }
    }

    fn compile_set<T>(&self, env: &LexicalContext, ctx: &mut CodeGenContext, formal: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let assignment = to_list(formal)?;
        if let &[Datum::Sym(ref sym), ref expr] = assignment.as_slice() {
            self.compile_expr(env, ctx, false, expr)?;
            let ptr = self.compile_ref(env, ctx, sym)?;
            ctx.code.push(Inst::PopArg(ptr));
            ctx.code.push(Inst::PushArg(MemRef::Undefined));
            Ok(())
        } else {
            Err(CompileError { kind: CompileErrorKind::BadSyntax })
        }
    }

    fn compile_quote<T: Clone+Debug>(&self, ctx: &mut CodeGenContext, items: &Datum<T>)
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

    fn rec_quote<T: Clone+Debug>(&self, ctx: &mut CodeGenContext, v: &Datum<T>)
            -> Result<(), CompileError>
    {
        match v {
            &Datum::Cons(ref pair) => {
                ctx.code.push(
                    Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("cons", &PRIM_CONS)))
                );
                self.rec_quote(ctx, &pair.0)?;
                self.rec_quote(ctx, &pair.1)?;
                ctx.code.push(Inst::Call(2));
            },
            &Datum::Vector(ref v) => {
                ctx.code.push(
                    Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("vector", &PRIM_VECTOR)))
                );
                for e in v.iter() {
                    self.rec_quote(ctx, e)?;
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

    fn compile_quasiquote<T>(&self,
                             env: &LexicalContext,
                             ctx: &mut CodeGenContext,
                             items: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
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

    fn get_syntax1<T: Clone+Debug>(&self, v: &Datum<T>) -> Option<(PrimitiveSyntax, Datum<T>)> {
        let res: Result<Vec<Datum<T>>, ()> = v.iter().collect();
        let list = match res {
            Ok(list) => list,
            Err(_) => return None
        };

        if let &[Datum::Sym(ref ptr), ref arg] = list.as_slice() {
            match ptr.as_ref() {
                "quasiquote" => Some((PrimitiveSyntax::Quasiquote, arg.clone())),
                "unquote" => Some((PrimitiveSyntax::Unquote, arg.clone())),
                "unquote-splicing" => Some((PrimitiveSyntax::UnquoteSplicing, arg.clone())),
                _ => None
            }
        } else {
            None
        }
    }

    fn rec_quasiquote<T>(&self,
                         qq_level: usize,
                         env: &LexicalContext,
                         ctx: &mut CodeGenContext,
                         v: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        match self.get_syntax1(v) {
            Some((PrimitiveSyntax::Quasiquote, arg)) => {
                ctx.code.push(
                    Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("list", &PRIM_LIST)))
                );
                ctx.code.push(
                    Inst::PushArg(MemRef::Const(SimpleDatum::Sym(Cow::Borrowed("quasiquote"))))
                );
                self.rec_quasiquote(qq_level+1, env, ctx, &arg)?;
                ctx.code.push(Inst::Call(2));
            },
            Some((PrimitiveSyntax::Unquote, arg)) => {
                if qq_level == 0 {
                    self.compile_expr(env, ctx, false, &arg)?;
                } else {
                    ctx.code.push(
                        Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("list", &PRIM_LIST)))
                    );
                    ctx.code.push(
                        Inst::PushArg(MemRef::Const(SimpleDatum::Sym(Cow::Borrowed("unquote"))))
                    );
                    self.rec_quasiquote(qq_level-1, env, ctx, &arg)?;
                    ctx.code.push(Inst::Call(2));
                }
            },
            _ => {
                match v {
                    &Datum::Cons(ref pair) => {
                        if let Some((PrimitiveSyntax::UnquoteSplicing, arg)) = self.get_syntax1(&pair.0) {
                            if qq_level == 0 {
                                ctx.code.push(Inst::PushArg(
                                    MemRef::PrimFunc(PrimFuncPtr::new("append", &PRIM_APPEND))
                                ));
                                self.compile_expr(env, ctx, false, &arg)?;
                            } else {
                                ctx.code.push(Inst::PushArg(
                                    MemRef::PrimFunc(PrimFuncPtr::new("cons", &PRIM_CONS))
                                ));
                                self.rec_quasiquote(qq_level-1, env, ctx, &pair.0)?;
                            }
                        } else {
                            ctx.code.push(Inst::PushArg(
                                MemRef::PrimFunc(PrimFuncPtr::new("cons", &PRIM_CONS))
                            ));
                            self.rec_quasiquote(qq_level, env, ctx, &pair.0)?;
                        }

                        self.rec_quasiquote(qq_level, env, ctx, &pair.1)?;
                        ctx.code.push(Inst::Call(2));
                    },
                    &Datum::Vector(ref v) => {
                        ctx.code.push(Inst::PushArg(
                            MemRef::PrimFunc(PrimFuncPtr::new("vector", &PRIM_VECTOR))
                        ));
                        for e in v.iter() {
                            self.rec_quasiquote(qq_level, env, ctx, e)?;
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

    fn is_sym<T>(&self, datum: &Datum<T>, sym: &str) -> bool {
        if let &Datum::Sym(ref s) = datum {
            s.as_ref() == sym
        } else {
            false
        }
    }

    fn get_else_clause<T: Clone+Debug>(&self, clauses: &mut Vec<Datum<T>>)
            -> Result<Option<Vec<Datum<T>>>, CompileError>
    {
        let else_exprs = match clauses.last() {
            None => return Err(CompileError { kind: CompileErrorKind::BadSyntax }),
            Some(last_clause) => match last_clause {
                &Datum::Cons(ref pair) =>  {
                    if self.is_sym(&pair.0, "else") {
                        let exprs = to_list(&pair.1)?;
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

    fn compile_cond<T>(&self,
                       env: &LexicalContext,
                       ctx: &mut CodeGenContext,
                       tail_ctx: bool,
                       preds: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let mut placeholders = Vec::new();
        let mut clauses = to_list(preds)?;

        let else_exprs = self.get_else_clause(&mut clauses)?;

        for clause in &clauses {
            let terms = to_list(&clause)?;

            if terms.len() < 2 {
                return Err(CompileError { kind: CompileErrorKind::BadSyntax });
            }

            self.compile_expr(env, ctx, false, &terms[0])?;

            // placeholder for JumpIfFalse
            let jump_inst = ctx.code.len();
            ctx.code.push(Inst::Nop);

            if self.is_sym(&terms[1], "=>") {
                if terms.len() != 3 {
                    return Err(CompileError { kind: CompileErrorKind::BadSyntax });
                }

                self.compile_expr(env, ctx, false, &terms[2])?;
                ctx.code.push(Inst::SwapArg);
                if tail_ctx {
                    ctx.code.push(Inst::TailCall);
                } else {
                    ctx.code.push(Inst::Call(1));
                }
            } else {
                ctx.code.push(Inst::DropArg(1));
                self.compile_exprs(env, ctx, tail_ctx, &terms[1..])?;
            }

            // placeholder for Jump: this jumps to the end of cond expr
            placeholders.push(ctx.code.len());
            ctx.code.push(Inst::Nop);

            // JumpIfFalse jumps here
            let jump_pos = ctx.code.len();
            ctx.code[jump_inst] = Inst::JumpIfFalse(jump_pos);

            ctx.code.push(Inst::DropArg(1));
        }

        if let Some(exprs) = else_exprs {
            self.compile_exprs(env, ctx, tail_ctx, &exprs)?;
        }

        let pos = ctx.code.len();
        for inst in placeholders.into_iter() {
            ctx.code[inst] = Inst::Jump(pos);
        }

        Ok(())
    }

    fn compile_case<T>(&self,
                       env: &LexicalContext,
                       ctx: &mut CodeGenContext,
                       tail_ctx: bool,
                       preds: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let mut placeholders = Vec::new();
        let mut clauses = to_list(preds)?;

        if clauses.len() < 2 {
            return Err(CompileError { kind: CompileErrorKind::BadSyntax });
        }

        let expr = clauses.remove(0);

        self.compile_expr(env, ctx, false, &expr)?;

        let else_exprs = self.get_else_clause(&mut clauses)?;

        for clause in &clauses {
            let mut case_placeholders = Vec::new();

            let terms = to_list(clause)?;

            if terms.len() < 2 {
                return Err(CompileError { kind: CompileErrorKind::BadSyntax });
            }

            let cases = to_list(&terms[0])?;

            for case in cases.iter() {
                self.rec_quote(ctx, case)?;

                ctx.code.push(Inst::Eqv);

                // placeholder for JumpIfNotFalse
                case_placeholders.push(ctx.code.len());
                ctx.code.push(Inst::Nop);

                ctx.code.push(Inst::DropArg(2));
            }

            // Case not matches: Jump to next case
            let no_match_jump_pos = ctx.code.len();
            ctx.code.push(Inst::Nop);

            // Case matches: JumpIfNotFalse jumps here
            let match_case = ctx.code.len();
            for pos in case_placeholders.into_iter() {
                ctx.code[pos] = Inst::JumpIfNotFalse(match_case);
            }

            ctx.code.push(Inst::DropArg(3));

            self.compile_exprs(env, ctx, tail_ctx, &terms[1..])?;

            // placeholder for Jump: to the end of case expression
            placeholders.push(ctx.code.len());
            ctx.code.push(Inst::Nop);

            // Next case: no_match_jump comes here
            let next_case = ctx.code.len();
            ctx.code[no_match_jump_pos] = Inst::Jump(next_case);
        }

        if let Some(exprs) = else_exprs {
            self.compile_exprs(env, ctx, tail_ctx, &exprs)?;
        }

        let pos = ctx.code.len();
        for inst in placeholders.into_iter() {
            ctx.code[inst] = Inst::Jump(pos);
        }

        Ok(())
    }

    fn compile_and<T>(&self,
                      env: &LexicalContext,
                      ctx: &mut CodeGenContext,
                      tail_ctx: bool,
                      preds: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let mut placeholders = Vec::new();
        let exprs = to_list(preds)?;
        if exprs.is_empty() {
            ctx.code.push(Inst::PushArg(MemRef::Const(SimpleDatum::Bool(true))));
            return Ok(());
        }

        self.compile_expr(env, ctx, false, &exprs[0])?;

        for expr in exprs[1..].iter() {
            placeholders.push(ctx.code.len());
            // placeholder for JumpIfFalse
            ctx.code.push(Inst::Nop);
            // Drop the value if the test fails
            ctx.code.push(Inst::DropArg(1));
            self.compile_expr(env, ctx, tail_ctx, &expr)?;
        }

        let jump_pc = ctx.code.len();
        for pc in placeholders.into_iter() {
            ctx.code[pc] = Inst::JumpIfFalse(jump_pc);
        }

        Ok(())
    }

    fn compile_or<T>(&self,
                     env: &LexicalContext,
                     ctx: &mut CodeGenContext,
                     tail_ctx: bool,
                     preds: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let mut placeholders = Vec::new();
        let exprs = to_list(preds)?;
        if exprs.is_empty() {
            ctx.code.push(Inst::PushArg(MemRef::Const(SimpleDatum::Bool(false))));
            return Ok(());
        }

        self.compile_expr(env, ctx, false, &exprs[0])?;

        for expr in exprs[1..].iter() {
            placeholders.push(ctx.code.len());
            // placeholder for JumpIfNotFalse
            ctx.code.push(Inst::Nop);
            // Drop the value if the test fails
            ctx.code.push(Inst::DropArg(1));
            self.compile_expr(env, ctx, tail_ctx, &expr)?;
        }

        let jump_pc = ctx.code.len();
        for pc in placeholders.into_iter() {
            ctx.code[pc] = Inst::JumpIfNotFalse(jump_pc);
        }

        Ok(())
    }

    fn compile_let_syntax<T>(&self,
                             env: &LexicalContext,
                             ctx: &mut CodeGenContext,
                             tail_ctx: bool,
                             datum: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        let (bindings, body) = self.get_form(datum)?;
        let mut new_env = env.clone();

        for binding in bindings {
            let syntax_rules = self.compile_syntax_rules(env, &binding.expr)?;
            new_env.syntax_env = new_env.syntax_env.insert(binding.sym, Rc::new(syntax_rules));
        }

        let exprs = to_exprs(&body)?;
        self.compile_exprs(&new_env, ctx, tail_ctx, &exprs)
    }

    fn compile_syntax_rules<T>(&self, env: &LexicalContext, datum: &Datum<T>)
            -> Result<CompiledMacro, CompileError>
        where T: Clone + Debug
    {
        let rules = to_list(datum)?;
        if rules.len() < 2 {
            return Err(CompileError { kind: CompileErrorKind::BadSyntax });
        }

        if let Some(PrimitiveSyntax::SyntaxRules) = self.get_syntax_name(env, &rules[0]) {
        } else {
            return Err(CompileError { kind: CompileErrorKind::BadSyntax });
        }

        let literals = to_list(&rules[1])?;
        let mut vars = HashSet::new();
        for literal in literals {
            if let Datum::Sym(sym) = literal {
                if vars.contains(&sym) {
                    return Err(CompileError { kind: CompileErrorKind::DuplicateVars })
                }

                vars.insert(sym);
            } else {
                return Err(CompileError { kind: CompileErrorKind::BadSyntax });
            }
        }

        let mut syntax_rules = Vec::new();
        for rule in &rules[2..] {
            let form = to_list(rule)?;
            if form.len() != 2 {
                return Err(CompileError { kind: CompileErrorKind::BadSyntax });
            }
            syntax_rules.push((form[0].clone(), form[1].clone()));
        }

        CompiledMacro::compile(&vars, &syntax_rules).map_err(|e| e.into())
    }

    fn compile_expr<T>(&self,
                       env: &LexicalContext,
                       ctx: &mut CodeGenContext,
                       tail_ctx: bool,
                       datum: &Datum<T>)
            -> Result<(), CompileError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        match datum {
            &Datum::Cons(_) =>
                self.compile_app(env, ctx, tail_ctx, datum),
            &Datum::Nil => Err(CompileError { kind: CompileErrorKind::NullEval }),
            &Datum::Sym(ref sym) => {
                let ptr = self.compile_ref(env, ctx, sym)?;
                ctx.code.push(Inst::PushArg(ptr));
                Ok(())
            },
            &Datum::Vector(ref v) => {
                ctx.code.push(
                    Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("vector", &PRIM_VECTOR)))
                );
                for e in v.iter() {
                    self.rec_quote(ctx, e)?;
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

impl Debug for PrimitiveSyntax {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;
    use std::rc::Rc;
    use datum::{Datum, SimpleDatum};
    use runtime::{Inst, MemRef, PrimFuncPtr};
    use base::{base_syntax, libbase};
    use primitive::{PRIM_ADD, PRIM_CONS};
    use number::Number;
    use super::Compiler;

    #[test]
    fn test_simple_expr() {
        let global = libbase();
        let syntax = base_syntax();
        let compiler = Compiler::new(syntax);
        let expected = Ok(vec![
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1 ,0)))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2 ,0)))),
            Inst::TailCall,
            Inst::Return
        ]);
        let code = compiler.compile::<()>(&global, &list![sym!("+"), num!(1), num!(2)]);
        assert_eq!(expected, code);
    }

    #[test]
    fn test_nested_expr() {
        let global = libbase();
        let syntax = base_syntax();
        let compiler = Compiler::new(syntax);
        let expected = Ok(vec![
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(3, 0)))),
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1, 0)))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
            Inst::Call(2),
            Inst::TailCall,
            Inst::Return
        ]);
        let code = compiler.compile::<()>(&global,
                                          &list![sym!("+"),
                                                 num!(3),
                                                 list![sym!("+"), num!(1), num!(2)]
                                                ]);
        assert_eq!(expected, code);
    }

    #[test]
    fn test_lambda() {
        let global = libbase();
        let syntax = base_syntax();
        let compiler = Compiler::new(syntax);
        let lambda: Datum<()> = list![sym!("lambda"), list![sym!("x")], list![sym!("+"), sym!("x"), num!(2)]];

        let f = vec![
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Arg(0)),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
            Inst::TailCall,
            Inst::Return
        ];
        let expected = Ok(vec![
            Inst::PushArg(MemRef::Closure(Rc::new(f), 0, Some(lambda.clone()))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1, 0)))),
            Inst::TailCall,
            Inst::Return
        ]);

        let code = compiler.compile::<()>(&global, &list![lambda, num!(1)]);
        assert_eq!(expected, code);
    }

    #[test]
    fn test_upvalue() {
        let global = libbase();
        let syntax = base_syntax();
        let compiler = Compiler::new(syntax);
        let f_src: Datum<()> = list![sym!("lambda"), list![sym!("y")], list![sym!("+"), sym!("x"), sym!("y")]];
        let g_src: Datum<()> = list![sym!("lambda"), list![sym!("x")], f_src.clone()];
        let f = vec![
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::UpValue(0, 0)),
            Inst::PushArg(MemRef::Arg(0)),
            Inst::TailCall,
            Inst::Return
        ];
        let g = vec![
            Inst::PushArg(MemRef::Closure(Rc::new(f), 1, Some(f_src))),
            Inst::Return
        ];
        let expected = Ok(vec![
            Inst::PushArg(MemRef::Closure(Rc::new(g), 0, Some(g_src.clone()))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
            Inst::Call(1),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(3, 0)))),
            Inst::TailCall,
            Inst::Return
        ]);

        // ((
        //   (lambda (x)            # = g
        //     (lambda (y) (+ x y)) # = f
        //   ) 2) 3)
        let code = compiler.compile::<()>(&global, &list![list![g_src, num!(2)], num!(3)]);
        assert_eq!(expected, code)
    }

    #[test]
    fn test_quote() {
        let global = libbase();
        let syntax = base_syntax();
        let compiler = Compiler::new(syntax);

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

        let code = compiler.compile::<()>(&global,
                                          &list![
                                                 sym!("quote"),
                                                 list![num!(1), list![num!(2), num!(3)]]
                                          ]);
        assert_eq!(expected, code)
    }
}
