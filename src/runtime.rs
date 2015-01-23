use std::rc::Rc;
use std::cell::RefCell;
use std::mem;
use std::fmt;

use error::{RuntimeErrorKind, RuntimeError};
use datum::Datum;

/// RuntimeData contains runtime values not representable in standard syntax
#[derive(Clone)]
pub enum RuntimeData {
    /// Primitive Function
    PrimFunc(&'static str, Rc<fn(&[RDatum]) -> Result<RDatum, RuntimeError>>),

    /// Compiled Closure
    Closure(Closure),

    /// Undefined value
    Undefined
}

/// Compiled closure object 
#[derive(Show, Clone, PartialEq)]
pub struct Closure {
    // Pointer to the bytecode
    code: Rc<Vec<Inst>>,
    // The lexical environment directly enclosing the code
    static_link: Option<StaticLink>
}

impl Closure {
    pub fn new(code: Rc<Vec<Inst>>, static_link: Option<StaticLink>) -> Closure {
        Closure {
            code: code,
            static_link: static_link
        }
    }
}

/// Type representation of RDatum
#[derive(Show, Copy)]
pub enum DatumType {
    Sym,
    Bool,
    Char,
    Num,
    List,
    Callable,
    Undefined
}

impl DatumType {
    /// Get the type of datum
    fn get_type(datum: &RDatum) -> DatumType {
        match datum {
            &Datum::Sym(_) => DatumType::Sym,
            &Datum::Bool(_) => DatumType::Bool,
            &Datum::Char(_) => DatumType::Char,
            &Datum::Num(_) => DatumType::Num,
            &Datum::Nil => DatumType::List,
            &Datum::Cons(_, _) => DatumType::List,
            &Datum::Ext(RuntimeData::PrimFunc(_, _)) => DatumType::Callable,
            &Datum::Ext(RuntimeData::Closure(_)) => DatumType::Callable,
            &Datum::Ext(RuntimeData::Undefined) => DatumType::Undefined
        }
    }
}

impl fmt::Show for RuntimeData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &RuntimeData::PrimFunc(name, _) =>
                write!(f, "<primitive: {:?}>", name),
            &RuntimeData::Closure(ref closure) =>
                write!(f, "<procedure {:?}: {:?}>", closure.static_link, closure.code),
            &RuntimeData::Undefined =>
                write!(f, "<undefined>")
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
            &RuntimeData::Closure(ref c0) =>
                if let &RuntimeData::Closure(ref c1) = other {
                    *c0 == *c1
                } else {
                    false
                },
            &RuntimeData::Undefined =>
                if let &RuntimeData::Undefined = other {
                    true
                } else {
                    false
                },
        }
    }
}

/// RDatum contains RuntimeData in addition to normal Datum
pub type RDatum = Datum<RuntimeData>;

/// Types with implementing DatumCast trait can cast from/to Datum
pub trait DatumCast {
    /// Casts Datum into Self, possibly raising error
    fn unwrap(datum: &RDatum) -> Result<Self, RuntimeError>;
    /// Casts Self into Datum
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

/// Pointer referring to memory locations in the VM
#[derive(Clone, Show, PartialEq)]
pub enum MemRef {
    RetVal,
    Arg(usize),
    UpValue(usize, usize),
    Const(RDatum),
    Closure(Rc<Vec<Inst>>, usize),
}

/// The instruction of the bytecode
#[derive(Clone, Show, PartialEq)]
pub enum Inst {
    Nop,
    PushArg(MemRef),
    Call(usize),
    Return,
    PushFrame(usize),
    PopFrame,
    DropArg(usize),
    Jump(usize),
    JumpIfFalse(usize),
}

/// When the enclosing lexical env goes out of scope of the closure, the env is copied into heap
/// memory. HeapClosure represents the env in heap memory
#[derive(Show, PartialEq)]
pub struct HeapClosure {
    args: Vec<RDatum>,
    static_link: Option<StaticLink>
}

/// ScopePtr points to the directly enclosing lexical env of the frame. It might be live in stack,
/// or residing in heap
#[derive(Show, PartialEq)]
pub enum ScopePtr {
    // Stack(n) refers to the n-th element of the main call stack
    // if n == runtime.call_stack.len(), this refers to the runtime.frame
    Stack(usize),

    // refers to the heap environment
    Heap(HeapClosure)
}

/// Shared link to the ScopePtr
pub type StaticLink = Rc<RefCell<ScopePtr>>;

/// StackFrame represents frame in the main stack
#[derive(Show)]
pub struct StackFrame {
    // Current running code
    closure: Closure,

    // Program counter
    pc: usize,

    // Bottom of the current frame
    stack_bottom: usize,

    // Number of function arguments of the current frame
    arg_size: usize,

    // Pointer link to this frame. When this frame is out of scope, other closures enclosed by
    // this scope loses reference to upvalues. To prevent such situation, when the frame is out of
    // scope, the VM copies this frame into a newly allocated heap memory. However, doing that
    // requires searching entire stack and heap memory looking for the pointers pointing to this
    // frame. To avoid that, VM just changes self_link pointing to ClosureHeap when the frame goes
    // out of scope.
    self_link: StaticLink
}

/// The virtual machine running the bytecode
pub struct Runtime {
    ret_val: RDatum,
    arg_stack: Vec<RDatum>,
    call_stack: Vec<StackFrame>,
    frame: StackFrame
}

impl Runtime {
    /// Create the new virtual machine with given code
    pub fn new(code: Vec<Inst>) -> Runtime {
        Runtime {
            ret_val: Datum::Nil,
            arg_stack: Vec::new(),
            call_stack: Vec::new(),
            frame: StackFrame {
                closure: Closure { code: Rc::new(code), static_link: None},
                pc: 0,
                stack_bottom: 0,
                arg_size: 0,
                self_link: Rc::new(RefCell::new(ScopePtr::Stack(0)))
            }
        }
    }

    fn fetch(&self) -> Inst {
        self.frame.closure.code[self.frame.pc].clone()
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
                        self.frame.closure.static_link.clone()
                    } else {
                        self.call_stack[n].closure.static_link.clone()
                    },
            }
        }
    }

    fn get_upvalue(&self, link_cnt: usize, arg_idx: usize) -> RDatum {
        let mut link = self.frame.closure.static_link.clone();
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
                Closure {
                    code: code.clone(),
                    static_link: Some(self.frame.self_link.clone())
                }
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
                    static_link: self.frame.closure.static_link.clone()
                };
                *self.frame.self_link.borrow_mut() = ScopePtr::Heap(heap);
                self.frame = f;
                true
            }
        }
    }

    fn push_call_stack(&mut self, arg_size: usize, closure: Closure) {
        let idx = self.call_stack.len();
        let stack_bottom = self.arg_stack.len() - arg_size; 
        let new_frame = StackFrame {
            closure: closure,
            pc: 0,
            stack_bottom: stack_bottom,
            arg_size: arg_size,
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
            Inst::Nop => {
                self.frame.pc += 1;
                true
            },
            Inst::Call(n) => {
                let top = self.arg_stack.len();
                let datum = self.arg_stack[top - n - 1].clone();
                match datum {
                    Datum::Ext(RuntimeData::PrimFunc(_, f)) => {
                        let dummy_closure = Closure {
                            code: Rc::new(Vec::new()),
                            static_link: None
                        };
                        self.push_call_stack(n, dummy_closure);
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
                    Datum::Ext(RuntimeData::Closure(closure)) => {
                        self.push_call_stack(n, closure);
                        true
                    },
                    _ => {
                        panic!("Not callable")
                    }
                }
            },
            Inst::PushFrame(n) => {
                let new_closure = Closure {
                    code: self.frame.closure.code.clone(),
                    static_link: Some(self.frame.self_link.clone())
                };
                let idx = self.call_stack.len();

                let new_frame = StackFrame {
                    closure: new_closure,
                    pc: self.frame.pc+1,
                    stack_bottom: self.arg_stack.len() - n,
                    arg_size: n,
                    self_link: Rc::new(RefCell::new(ScopePtr::Stack(idx)))
                };

                self.call_stack.push(new_frame);
                mem::swap(&mut self.frame, self.call_stack.last_mut().unwrap());
                true
            },
            Inst::PopFrame => {
                let pc = self.frame.pc;
                self.pop_call_stack();
                self.frame.pc = pc+1;
                true
            },
            Inst::Jump(pc) => {
                self.frame.pc = pc;
                true
            },
            Inst::JumpIfFalse(pc) => match self.arg_stack.pop() {
                Some(Datum::Bool(false)) => {
                    self.frame.pc = pc;
                    true
                },
                Some(_) => {
                    self.frame.pc += 1;
                    true
                },
                None =>
                    panic!("Stack empty!")
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
