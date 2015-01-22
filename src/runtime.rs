use std::rc::Rc;
use std::cell::RefCell;
use std::mem;
use std::fmt;

use error::{RuntimeErrorKind, RuntimeError};
use datum::Datum;
use compiler::PrimSyntax;

#[derive(Clone)]
pub enum RuntimeData {
    PrimFunc(&'static str, Rc<fn(&[RDatum]) -> Result<RDatum, RuntimeError>>),
    PrimSyntax(PrimSyntax),
    Closure(Rc<Vec<Inst>>, Option<StaticLink>)
}

#[derive(Show, Copy)]
pub enum DatumType {
    Sym,
    Bool,
    Char,
    Num,
    List,
    Callable,
    Syntax
}

impl DatumType {
    fn get_type(datum: &RDatum) -> DatumType {
        match datum {
            &Datum::Sym(_) => DatumType::Sym,
            &Datum::Bool(_) => DatumType::Bool,
            &Datum::Char(_) => DatumType::Char,
            &Datum::Num(_) => DatumType::Num,
            &Datum::Nil => DatumType::List,
            &Datum::Cons(_, _) => DatumType::List,
            &Datum::Ext(RuntimeData::PrimFunc(_, _)) => DatumType::Callable,
            &Datum::Ext(RuntimeData::Closure(_, _)) => DatumType::Callable,
            &Datum::Ext(RuntimeData::PrimSyntax(_)) => DatumType::Syntax,
        }
    }
}

impl fmt::Show for RuntimeData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &RuntimeData::PrimFunc(name, _) => name.fmt(f),
            &RuntimeData::PrimSyntax(name) => name.fmt(f),
            &RuntimeData::Closure(ref code, ref static_link) =>
                write!(f, "<procedure {:?}: {:?}>", static_link, code)
        }
    }
}

impl PartialEq for RuntimeData {
    fn eq(&self, other: &RuntimeData) -> bool {
        match self {
            &RuntimeData::PrimFunc(ref n0, _) =>
                if let &RuntimeData::PrimFunc(ref n1, _) = other {
                    *n0 == *n1
                } else {
                    false
                },
            &RuntimeData::PrimSyntax(ref n0) =>
                if let &RuntimeData::PrimSyntax(ref n1) = other {
                    *n0 == *n1
                } else {
                    false
                },
            &RuntimeData::Closure(ref c0, _) =>
                if let &RuntimeData::Closure(ref c1, _) = other {
                    *c0 == *c1
                } else {
                    false
                },
        }
    }
}

pub type RDatum = Datum<RuntimeData>;

pub trait DatumCast {
    fn unwrap(datum: &RDatum) -> Result<Self, RuntimeError>;
    fn wrap(&self) -> RDatum;
}

impl DatumCast for isize {
    fn unwrap(datum: &RDatum) -> Result<isize, RuntimeError> {
        match datum {
            &Datum::Num(n) => Ok(n),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected Num, but received {:?}", DatumType::get_type(datum))
            })
        }
    }

    fn wrap(&self) -> RDatum{
        Datum::Num(*self)
    }
}

#[derive(Clone, Show, PartialEq)]
pub enum MemRef {
    RetVal,
    Arg(usize),
    UpValue(usize, usize),
    Const(RDatum),
    Closure(Rc<Vec<Inst>>, usize),
}

#[derive(Clone, Show, PartialEq)]
pub enum Inst {
    PushArg(MemRef),
    Call(usize),
    DropArg(usize),
    Return
}

#[derive(Show)]
pub struct HeapClosure {
    args: Vec<RDatum>,
    static_link: Option<StaticLink>
}

#[derive(Show)]
pub enum ScopePtr {
    // Stack(n) refers to the n-th element of the main call stack
    // if n == runtime.call_stack.len(), this refers to the runtime.frame
    Stack(usize),

    // refers to the heap closure
    Heap(HeapClosure)
}

pub type StaticLink = Rc<RefCell<ScopePtr>>;

#[derive(Show)]
pub struct StackFrame {
    code: Rc<Vec<Inst>>,
    pc: usize,
    stack_bottom: usize,
    arg_size: usize,
    static_link: Option<StaticLink>,
    self_link: StaticLink
}

pub struct Runtime {
    ret_val: RDatum,
    arg_stack: Vec<RDatum>,
    call_stack: Vec<StackFrame>,
    frame: StackFrame
}

impl Runtime {
    pub fn new(code: Vec<Inst>) -> Runtime {
        Runtime {
            ret_val: Datum::Nil,
            arg_stack: Vec::new(),
            call_stack: Vec::new(),
            frame: StackFrame {
                code: Rc::new(code),
                pc: 0,
                stack_bottom: 0,
                arg_size: 0,
                static_link: None,
                self_link: Rc::new(RefCell::new(ScopePtr::Stack(0)))
            }
        }
    }

    fn fetch(&self) -> Inst {
        self.frame.code[self.frame.pc].clone()
    }

    pub fn get_stack_val(&self, idx: usize) -> RDatum {
        self.arg_stack[self.frame.stack_bottom + idx].clone()
    }

    fn up_scope(&self, link: Option<StaticLink>) -> Option<StaticLink> {
        match link {
            None => None,
            Some(link) => match *link.borrow() {
                ScopePtr::Heap(ref data) => data.static_link.clone(),
                ScopePtr::Stack(n) => if n == self.call_stack.len() {
                        self.frame.static_link.clone()
                    } else {
                        self.call_stack[n].static_link.clone()
                    },
            }
        }
    }

    fn get_upvalue(&self, link_cnt: usize, arg_idx: usize) -> RDatum {
        let mut link = self.frame.static_link.clone();
        for _ in range(0, link_cnt) {
            link = self.up_scope(link);
        }
        match link {
            None => panic!("get_upvalue({:?}, {:?}) failed!", link_cnt, arg_idx),
            Some(link) => match *link.borrow() {
                ScopePtr::Heap(ref data) => data.args[arg_idx].clone(),
                ScopePtr::Stack(n) => {
                    let frame_ref = if n == self.call_stack.len() {
                        &self.frame
                    } else {
                        &self.call_stack[n]
                    };
                    let bot = frame_ref.stack_bottom;
                    self.arg_stack[bot + arg_idx].clone()
                }
            }
        }
    }

    fn fetch_mem(&self, ptr: MemRef) -> RDatum {
        match ptr {
            MemRef::RetVal => self.ret_val.clone(),
            MemRef::Arg(idx) => self.get_stack_val(idx),
            MemRef::UpValue(i, j) => self.get_upvalue(i, j),
            MemRef::Const(val) => val.clone(),
            MemRef::Closure(code, _) => Datum::Ext(RuntimeData::Closure(
                    code.clone(),
                    Some(self.frame.self_link.clone())
            ))
        }
    }

    fn pop_call_stack(&mut self) -> bool {
        match self.call_stack.pop() {
            None => false,
            Some(f) => {
                let bottom = self.frame.stack_bottom;
                let top = bottom + self.frame.arg_size;
                let heap = HeapClosure {
                    args: self.arg_stack[bottom .. top].to_vec(),
                    static_link: self.frame.static_link.clone()
                };
                *self.frame.self_link.borrow_mut() = ScopePtr::Heap(heap);
                self.frame = f;
                true
            }
        }
    }

    fn push_call_stack(&mut self, code: Rc<Vec<Inst>>, arg_size: usize,
                       static_link: Option<StaticLink>)
    {
        let idx = self.call_stack.len();
        let stack_bottom = self.arg_stack.len() - arg_size; 
        let new_frame = StackFrame {
            code: code.clone(),
            pc: 0,
            stack_bottom: stack_bottom,
            arg_size: arg_size,
            static_link: static_link,
            self_link: Rc::new(RefCell::new(ScopePtr::Stack(idx+1)))
        };

        self.call_stack.push(new_frame);
        mem::swap(&mut self.frame, self.call_stack.last_mut().unwrap());
    }

    pub fn push_stack(&mut self, val: RDatum) {
        self.arg_stack.push(val)
    }

    pub fn pop_stack(&mut self) -> Option<RDatum> {
        self.arg_stack.pop()
    }

    fn step(&mut self) -> bool {
        debug!("STEP");
        let value = self.fetch();
        for (i, frame) in self.call_stack.iter().enumerate() {
            debug!("call_stack[{:?}]: {:?}", i, frame);
        }
        debug!("frame:          {:?}", self.frame);
        for (i, val) in self.arg_stack.iter().enumerate() {
            debug!("stack[{:?}]: {:?}", i, val);
        }

        debug!("fetch: {:?}", value);
        match value {
            Inst::Call(n) => {
                let top = self.arg_stack.len();
                let datum = self.arg_stack[top - n - 1].clone();
                match datum {
                    Datum::Ext(RuntimeData::PrimFunc(_, f)) => {
                        self.push_call_stack(Rc::new(Vec::new()), n, None);
                        let res = match (*f)(&self.arg_stack[top - n ..]) {
                            Ok(x) => x,
                            Err(e) => panic!(e)
                        };
                        self.pop_call_stack();
                        self.arg_stack.truncate(top - n - 1);
                        self.push_stack(res);
                        self.frame.pc += 1;
                        true
                    }, 
                    Datum::Ext(RuntimeData::Closure(code, static_link)) => {
                        self.push_call_stack(code, n, static_link);
                        true
                    },
                    _ => {
                        panic!("Not callable")
                    }
                }
            },
            Inst::PushArg(ptr) => {
                let val = self.fetch_mem(ptr);
                self.arg_stack.push(val);
                self.frame.pc += 1;
                true
            },
            Inst::DropArg(n) => {
                for _ in range(0, n) {
                    self.pop_stack();
                }
                true
            },
            Inst::Return => {
                let n = self.frame.arg_size;
                let top = self.arg_stack.len();
                let res = self.pop_call_stack();
                let retval = match self.arg_stack.pop() {
                    Some(val) => val,
                    None => panic!("arg_stack empty!")
                };
                self.arg_stack.truncate(top - n - 2);
                self.push_stack(retval);
                if res {
                    self.frame.pc += 1;
                }
                res
            }
        }
    }

    pub fn run(&mut self) -> RDatum {
        while self.step() {
            ()
        }

        return self.arg_stack.pop().unwrap()
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use super::{Inst, MemRef, Runtime, RuntimeData};
    use datum::Datum;
    use primitive::PRIM_ADD;

    #[test]
    fn test_runtime() {
        let code = vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", Rc::new(PRIM_ADD))))),
            Inst::PushArg(MemRef::Const(Datum::Num(1))),
            Inst::PushArg(MemRef::Const(Datum::Num(2))),
            Inst::Call(2),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(3));
    }

    #[test]
    fn test_nested_call() {
        let code = vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", Rc::new(PRIM_ADD))))),
            Inst::PushArg(MemRef::Const(Datum::Num(3))),
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", Rc::new(PRIM_ADD))))),
            Inst::PushArg(MemRef::Const(Datum::Num(1))),
            Inst::PushArg(MemRef::Const(Datum::Num(2))),
            Inst::Call(2),
            Inst::Call(2),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(6));
    }

    #[test]
    fn test_lambda() {
        let f = vec![
            Inst::PushArg(MemRef::Const(Datum::Ext(RuntimeData::PrimFunc("+", Rc::new(PRIM_ADD))))),
            Inst::PushArg(MemRef::Arg(0)),
            Inst::PushArg(MemRef::Const(Datum::Num(2))),
            Inst::Call(2),
            Inst::Return
        ];
        let code = vec![
            Inst::PushArg(MemRef::Closure(Rc::new(f), 0)),
            Inst::PushArg(MemRef::Const(Datum::Num(1))),
            Inst::Call(1),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(3));
    }

    #[test]
    fn test_closure() {
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

        // ((
        //   (lambda (x)            # = g
        //     (lambda (y) (+ x y)) # = f
        //   ) 2) 3)
        let code = vec![
            Inst::PushArg(MemRef::Closure(Rc::new(g), 0)),
            Inst::PushArg(MemRef::Const(Datum::Num(2))),
            Inst::Call(1),
            Inst::PushArg(MemRef::Const(Datum::Num(3))),
            Inst::Call(1),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(5));
    }
}
