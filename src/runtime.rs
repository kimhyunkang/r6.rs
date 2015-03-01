use std::rc::Rc;
use std::cell::RefCell;
use std::mem;
use std::fmt;
use std::ops::DerefMut;

use number::Number;
use real::Real;
use error::{RuntimeErrorKind, RuntimeError};
use datum::{Datum, DatumType, Object};
use primitive::PrimFunc;

use log::LogLevel;

/// Compiled closure object 
#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    // Pointer to the bytecode
    code: Rc<Vec<Inst>>,
    // The lexical environment directly enclosing the code
    static_link: Option<StaticLink>
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<procedure: [{:?}] {:?}>", self.static_link, self.code)
    }
}

impl Object for Closure {
    fn get_closure(&self) -> Option<&Closure> {
        Some(self)
    }

    fn obj_eq(&self, rhs: &Object) -> bool {
        if let Some(r) = rhs.get_closure() {
            self == r
        } else {
            false
        }
    }

    fn get_type(&self) -> DatumType {
        DatumType::Callable
    }
}

impl Closure {
    pub fn new(code: Rc<Vec<Inst>>, static_link: Option<StaticLink>) -> Closure {
        Closure {
            code: code,
            static_link: static_link
        }
    }
}

pub struct NativeProc {
    name: &'static str,
    code: &'static (PrimFunc + 'static)
}

impl NativeProc {
    pub fn new(name: &'static str, code: &'static (PrimFunc + 'static)) -> NativeProc {
        NativeProc { name: name, code: code }
    }
}

impl fmt::Display for NativeProc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<primitive: {}>", self.name)
    }
}

impl PartialEq for NativeProc {
    fn eq(&self, rhs: &NativeProc) -> bool {
        self.name == rhs.name
    }
}

impl Object for NativeProc {
    fn get_primfunc(&self) -> Option<&NativeProc> {
        Some(self)
    }

    fn obj_eq(&self, rhs: &Object) -> bool {
        if let Some(r) = rhs.get_primfunc() {
            self == r
        } else {
            false
        }
    }

    fn get_type(&self) -> DatumType {
        DatumType::Callable
    }
}

/// RDatum contains RuntimeData in addition to normal Datum
pub type RDatum = Datum;

/// Types with implementing DatumCast trait can cast from/to Datum
pub trait DatumCast {
    /// Casts Datum into Self, possibly raising error
    fn unwrap(datum: RDatum) -> Result<Self, RuntimeError>;
    /// Casts Self into Datum
    fn wrap(self) -> RDatum;
}

impl DatumCast for Number {
    fn unwrap(datum: RDatum) -> Result<Number, RuntimeError> {
        match datum {
            Datum::Num(n) => Ok(n),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected Num, but received {:?}", DatumType::get_type(&datum))
            })
        }
    }

    fn wrap(self) -> RDatum{
        Datum::Num(self)
    }
}

impl DatumCast for Real {
    fn unwrap(datum: RDatum) -> Result<Real, RuntimeError> {
        match datum {
            Datum::Num(Number::Real(n)) => Ok(n),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected Real, but received {:?}", DatumType::get_type(&datum))
            })
        }
    }

    fn wrap(self) -> RDatum {
        Datum::Num(Number::Real(self))
    }
}

impl DatumCast for bool {
    fn unwrap(datum: RDatum) -> Result<bool, RuntimeError> {
        match datum {
            Datum::Bool(b) => Ok(b),
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidType,
                desc: format!("expected Bool, but received {:?}", DatumType::get_type(&datum))
            })
        }
    }

    fn wrap(self) -> RDatum{
        Datum::Bool(self)
    }
}

impl DatumCast for (RDatum, RDatum) {
    fn unwrap(datum: RDatum) -> Result<(RDatum, RDatum), RuntimeError> {
        if let Datum::Ptr(ref ptr) = datum {
            if let Some(pair) = ptr.get_pair() {
                return Ok(pair.clone());
            }
        }

        Err(RuntimeError {
            kind: RuntimeErrorKind::InvalidType,
            desc: format!("expected Pair, but received {:?}", DatumType::get_type(&datum))
        })
    }

    fn wrap(self) -> RDatum {
        Datum::Ptr(Rc::new(Box::new(self)))
    }
}

/// Pointer referring to memory locations in the VM
#[derive(Clone, Debug, PartialEq)]
pub enum MemRef {
    RetVal,
    Arg(usize),
    UpValue(usize, usize),
    Const(RDatum),
    Closure(Rc<Vec<Inst>>, usize),
}

/// The instruction of the bytecode
#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    /// no-op
    Nop,
    /// push value of given pointer to the stack
    PushArg(MemRef),
    /// pop value from the stack and copy to the given pointer
    PopArg(MemRef),
    /// pop value from the stack and remove it
    DropArg,
    /// roll args [n..] into a list
    RollArgs(usize),
    /// call the function in (stack_top - n)
    Call(usize),
    /// pop the call stack frame, and return to the call site
    Return,
    /// push the call stack without jumping, and move stack_bottom to (stack_top - n)
    PushFrame(usize),
    /// set arg_size to n, letting PopFrame or Return pop the correct number of arg_stack
    SetArgSize(usize),
    /// pop the call stack without jumping
    PopFrame,
    /// jump to the given pc
    Jump(usize),
    /// jump to the given pc if current stack top is `#f`
    JumpIfFalse(usize),
}

/// When the enclosing lexical env goes out of scope of the closure, the env is copied into heap
/// memory. HeapClosure represents the env in heap memory
#[derive(Debug, PartialEq)]
pub struct HeapClosure {
    args: Vec<RDatum>,
    static_link: Option<StaticLink>
}

/// ScopePtr points to the directly enclosing lexical env of the frame. It might be live in stack,
/// or residing in heap
#[derive(Debug, PartialEq)]
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
#[derive(Debug)]
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

    fn set_upvalue(&mut self, link_cnt: usize, arg_idx: usize, val: RDatum) {
        let mut link = self.frame.closure.static_link.clone();
        for _ in range(0, link_cnt) {
            link = self.up_scope(link);
        }
        match link {
            None => panic!("get_upvalue({:?}, {:?}) failed!", link_cnt, arg_idx),
            Some(link) => match link.borrow_mut().deref_mut() {
                &mut ScopePtr::Heap(ref mut data) => {
                    data.args[arg_idx] = val;
                },
                &mut ScopePtr::Stack(n) => {
                    let frame_ref = if n == self.call_stack.len() {
                        &self.frame
                    } else {
                        &self.call_stack[n]
                    };
                    let bot = frame_ref.stack_bottom;
                    self.arg_stack[bot + arg_idx] = val;
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
            MemRef::Closure(code, _) => Datum::Ptr(Rc::new(box Closure {
                code: code.clone(),
                static_link: Some(self.frame.self_link.clone())
            }))
        }
    }

    fn write_mem(&mut self, ptr: MemRef, val: RDatum) {
        match ptr {
            MemRef::RetVal => {
                self.ret_val = val;
            },
            MemRef::Arg(idx) => {
                self.arg_stack[self.frame.stack_bottom + idx] = val;
            },
            MemRef::UpValue(i, j) => self.set_upvalue(i, j, val),
            MemRef::Const(_) => panic!("Cannot write to read-only memory"),
            MemRef::Closure(_, _) => panic!("Cannot write to instruction memory")
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
        let inst = self.fetch();

        if log_enabled!(LogLevel::Debug) {
            debug!("Arg Stack:");
            for (i, arg) in self.arg_stack.iter().enumerate() {
                debug!("  [{:?}]: {:?}", i, arg);
            }
            debug!("Call Stack:");
            for (i, frame) in self.call_stack.iter().enumerate() {
                debug!("  [{:?}]: {:?}", i, frame);
            }
            debug!("  [top]: {:?}", self.frame);
            debug!("Fetch: {:?}", inst);
        }

        match inst {
            Inst::Nop => {
                self.frame.pc += 1;
                true
            },
            Inst::Call(n) => {
                let top = self.arg_stack.len();
                let datum = self.arg_stack[top - n - 1].clone();
                if let Datum::Ptr(ref ptr) = datum {
                    if let Some(native) = ptr.get_primfunc() {
                        let args = if n == 0 {
                            Vec::new()
                        } else {
                            self.arg_stack.split_off(top-n)
                        };
                        let res = match native.code.call(args) {
                            Ok(x) => x,
                            Err(e) => panic!(e)
                        };
                        match self.arg_stack.pop() {
                            None => panic!("arg_stack size mismatch"),
                            Some(_) => ()
                        };
                        self.push_stack(res);
                        self.frame.pc += 1;
                        return true;
                    }

                    if let Some(closure) = ptr.get_closure() {
                        self.push_call_stack(n, closure.clone());
                        return true;
                    }

                    panic!("Not callable");
                } else {
                    panic!("Not callable");
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
                    self_link: Rc::new(RefCell::new(ScopePtr::Stack(idx+1)))
                };

                self.call_stack.push(new_frame);
                mem::swap(&mut self.frame, self.call_stack.last_mut().unwrap());
                true
            },
            Inst::SetArgSize(n) => {
                self.frame.arg_size = n;
                self.frame.pc += 1;
                true
            }
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
            Inst::JumpIfFalse(pc) => match self.arg_stack.last() {
                Some(&Datum::Bool(false)) => {
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
            Inst::PopArg(ptr) => {
                let val = match self.arg_stack.pop() {
                    Some(x) => x,
                    None => panic!("Stack empty!")
                };
                self.write_mem(ptr, val);
                self.frame.pc += 1;
                true
            },
            Inst::DropArg => {
                self.arg_stack.pop();
                self.frame.pc += 1;
                true
            },
            Inst::RollArgs(n) => {
                let vararg_start = self.frame.stack_bottom + n;
                let list: RDatum = self.arg_stack[vararg_start ..].iter().map(Clone::clone).collect();
                self.arg_stack.truncate(vararg_start);
                self.push_stack(list);
                self.frame.arg_size = n+1;
                self.frame.pc += 1;
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
    use super::{Inst, MemRef, Runtime, NativeProc};
    use datum::Datum;
    use primitive::PRIM_ADD;
    use number::Number;

    #[test]
    fn test_runtime() {
        let code = vec![
            Inst::PushArg(MemRef::Const(Datum::Ptr(Rc::new(box NativeProc::new("+", &PRIM_ADD))))),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(1, 0)))),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(2, 0)))),
            Inst::Call(2),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(Number::new_int(3, 0)));
    }

    #[test]
    fn test_nested_call() {
        let code = vec![
            Inst::PushArg(MemRef::Const(Datum::Ptr(Rc::new(box NativeProc::new("+", &PRIM_ADD))))),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(3, 0)))),
            Inst::PushArg(MemRef::Const(Datum::Ptr(Rc::new(box NativeProc::new("+", &PRIM_ADD))))),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(1, 0)))),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(2, 0)))),
            Inst::Call(2),
            Inst::Call(2),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(Number::new_int(6, 0)));
    }

    #[test]
    fn test_lambda() {
        let f = vec![
            Inst::PushArg(MemRef::Const(Datum::Ptr(Rc::new(box NativeProc::new("+", &PRIM_ADD))))),
            Inst::PushArg(MemRef::Arg(0)),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(2, 0)))),
            Inst::Call(2),
            Inst::Return
        ];
        let code = vec![
            Inst::PushArg(MemRef::Closure(Rc::new(f), 0)),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(1, 0)))),
            Inst::Call(1),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(Number::new_int(3, 0)));
    }

    #[test]
    fn test_closure() {
        let f = vec![
            Inst::PushArg(MemRef::Const(Datum::Ptr(Rc::new(box NativeProc::new("+", &PRIM_ADD))))),
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
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(2, 0)))),
            Inst::Call(1),
            Inst::PushArg(MemRef::Const(Datum::Num(Number::new_int(3, 0)))),
            Inst::Call(1),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(Number::new_int(5, 0)));
    }
}
