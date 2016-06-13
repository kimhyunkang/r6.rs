use std::rc::Rc;
use std::borrow::Cow;
use std::cell::RefCell;
use std::mem;
use std::fmt;
use std::ops::DerefMut;

use cast::DatumCast;
use eqv::DatumEqv;
use number::Number;
use datum::Datum;
use primitive::PrimFunc;

use log::LogLevel;

#[derive(Clone)]
pub struct PrimFuncPtr {
    name: &'static str,
    function: &'static (PrimFunc + 'static)
}

impl PrimFuncPtr {
    pub fn new(name: &'static str, function: &'static (PrimFunc + 'static)) -> PrimFuncPtr {
        PrimFuncPtr { name: name, function: function }
    }
}

impl PartialEq for PrimFuncPtr {
    fn eq(&self, other: &PrimFuncPtr) -> bool {
        (self.function as *const PrimFunc) == (other.function as *const PrimFunc)
    }
}

impl DatumEqv for PrimFuncPtr {
    fn eqv(&self, other: &PrimFuncPtr) -> bool {
        self.name == other.name && (self.function as *const PrimFunc) == (other.function as *const PrimFunc)
    }
}

impl fmt::Debug for PrimFuncPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<primitive function {}", self.name)
    }
}

/// RuntimeData contains runtime values not representable in standard syntax
#[derive(Clone, PartialEq)]
pub enum RuntimeData {
    /// Primitive Function
    PrimFunc(PrimFuncPtr),

    /// Compiled Closure
    Closure(Closure),

    /// Undefined value
    Undefined
}

/// Compiled closure object
#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    // Pointer to the bytecode
    code: Rc<Vec<Inst>>,
    // The lexical environment directly enclosing the code
    static_link: Option<StaticLink>
}

impl DatumEqv for Closure {
    fn eqv(&self, other: &Closure) -> bool {
        self.code.eqv(&other.code) && self.static_link.eqv(&other.static_link)
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

/// Type representation of RDatum
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DatumType {
    Sym,
    Bool,
    Char,
    String,
    Vector,
    Bytes,
    Num,
    Pair,
    Null,
    Callable,
    Undefined
}

impl DatumType {
    /// Get the type of datum
    pub fn get_type(datum: &RDatum) -> DatumType {
        match datum {
            &Datum::Sym(_) => DatumType::Sym,
            &Datum::Bool(_) => DatumType::Bool,
            &Datum::Char(_) => DatumType::Char,
            &Datum::String(_) => DatumType::String,
            &Datum::Vector(_) => DatumType::Vector,
            &Datum::Bytes(_) => DatumType::Bytes,
            &Datum::Num(_) => DatumType::Num,
            &Datum::Nil => DatumType::Null,
            &Datum::Cons(_) => DatumType::Pair,
            &Datum::Ext(RuntimeData::PrimFunc(_)) => DatumType::Callable,
            &Datum::Ext(RuntimeData::Closure(_)) => DatumType::Callable,
            &Datum::Ext(RuntimeData::Undefined) => DatumType::Undefined
        }
    }
}

impl DatumEqv for RuntimeData {
    fn eqv(&self, other: &RuntimeData) -> bool {
        match self {
            &RuntimeData::Closure(ref self_v) => if let &RuntimeData::Closure(ref other_v) = other {
                    self_v.eqv(other_v)
                } else {
                    false
                },
            &RuntimeData::PrimFunc(ref self_v) => if let &RuntimeData::PrimFunc(ref other_v) = other {
                    self_v.eqv(other_v)
                } else {
                    false
                },
            &RuntimeData::Undefined => if let &RuntimeData::Undefined = other {
                    true
                } else {
                    false
                },
        }
    }
}

impl fmt::Debug for RuntimeData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &RuntimeData::PrimFunc(ref func_ptr) =>
                write!(f, "<primitive: {:?}>", func_ptr.name),
            &RuntimeData::Closure(ref closure) =>
                write!(f, "<procedure {:?}: {:?}>", closure.static_link, closure.code),
            &RuntimeData::Undefined =>
                write!(f, "<undefined>")
        }
    }
}

/// RDatum contains RuntimeData in addition to normal Datum
pub type RDatum = Datum<RuntimeData>;

/// Pointer referring to memory locations in the VM
#[derive(Clone, Debug, PartialEq)]
pub enum MemRef {
    RetVal,
    Arg(usize),
    UpValue(usize, usize),
    Const(SimpleDatum),
    Undefined,
    PrimFunc(PrimFuncPtr),
    Closure(Rc<Vec<Inst>>, usize)
}

#[derive(Clone, Debug, PartialEq)]
pub enum SimpleDatum {
    Sym(Cow<'static, str>),
    Bool(bool),
    Char(char),
    String(Rc<String>),
    Bytes(Rc<Vec<u8>>),
    Num(Number),
    Nil
}

impl SimpleDatum {
    pub fn from_datum<T>(datum: Datum<T>) -> Option<SimpleDatum> {
        match datum {
            Datum::Sym(s) => Some(SimpleDatum::Sym(s)),
            Datum::Bool(b) => Some(SimpleDatum::Bool(b)),
            Datum::Char(c) => Some(SimpleDatum::Char(c)),
            Datum::String(s) => Some(SimpleDatum::String(s)),
            Datum::Bytes(v) => Some(SimpleDatum::Bytes(v)),
            Datum::Num(n) => Some(SimpleDatum::Num(n)),
            Datum::Nil => Some(SimpleDatum::Nil),
            _ => None
        }
    }
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
    /// swap two top values of the stack
    SwapArg,
    /// roll args [n..] into a list
    RollArgs(usize),
    /// pop pair(h, t) from the top of the list, then push h, then push t
    Uncons,
    /// compare two top values of the stack with `eqv?` operator
    Eqv,
    /// compare top of the stack with given pointer, and push `#t` if the type equals
    Type(DatumType),
    /// call the function in (stack_top - n)
    Call(usize),
    /// call the function at the bottom of the current frame
    CallSplicing,
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
    /// jump to the given pc if current stack top is not `#f`
    JumpIfNotFalse(usize),
    /// throw error if current stack top is `#f`
    ThrowIfFalse(&'static str)
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
            // Return instruction removes current closure with the args
            // However, there isn't a calling closure at the top program because we directly inject
            // the code here.
            // That's why we inject a dummy value at the bottom of the arg stack
            arg_stack: vec![Datum::Ext(RuntimeData::Undefined)],
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
        for _ in 0 .. link_cnt {
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
        for _ in 0 .. link_cnt {
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
            MemRef::Const(val) => DatumCast::wrap(val),
            MemRef::Undefined => Datum::Ext(RuntimeData::Undefined),
            MemRef::PrimFunc(ptr) => Datum::Ext(RuntimeData::PrimFunc(ptr)),
            MemRef::Closure(code, _) => Datum::Ext(RuntimeData::Closure(
                Closure {
                    code: code.clone(),
                    static_link: Some(self.frame.self_link.clone())
                }
            ))
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
            MemRef::Undefined => panic!("Cannot write to undefined memory address"),
            MemRef::PrimFunc(_) => panic!("Cannot write to code area"),
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

    fn call(&mut self, n: usize) -> bool {
        let top = self.arg_stack.len();
        let datum = self.arg_stack[top - n - 1].clone();
        match datum {
            Datum::Ext(RuntimeData::PrimFunc(fptr)) => {
                let args = if n == 0 {
                    Vec::new()
                } else {
                    self.arg_stack.split_off(top-n)
                };
                let res = match fptr.function.call(args) {
                    Ok(x) => x,
                    Err(e) => panic!("Error in primitive function <{}>: {:?}", fptr.name, e)
                };
                match self.arg_stack.pop() {
                    None => panic!("arg_stack size mismatch"),
                    Some(_) => ()
                };
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
            Inst::Type(typeinfo) => {
                let val = {
                    let arg = self.arg_stack.last().expect("arg_stack empty!");
                    DatumType::get_type(arg) == typeinfo
                };
                self.arg_stack.push(Datum::Bool(val));
                self.frame.pc += 1;
                true
            },
            Inst::Call(n) => self.call(n),
            Inst::CallSplicing => {
                let n_args = self.arg_stack.len() - self.frame.stack_bottom;
                if n_args == 0 {
                    panic!("Call args empty");
                }
                self.call(n_args - 1)
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
            Inst::JumpIfNotFalse(pc) => match self.arg_stack.last() {
                Some(&Datum::Bool(false)) => {
                    self.frame.pc += 1;
                    true
                },
                Some(_) => {
                    self.frame.pc = pc;
                    true
                },
                None =>
                    panic!("Stack empty!")
            },
            Inst::ThrowIfFalse(msg) => {
                match self.arg_stack.last() {
                    Some(&Datum::Bool(false)) => panic!(msg),
                    Some(_) => {
                        self.frame.pc += 1;
                        true
                    },
                    None =>
                        panic!("Stack empty!")
                }
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
            Inst::SwapArg => {
                let y = self.arg_stack.pop().expect("Stack empty!");
                let x = self.arg_stack.pop().expect("Stack empty!");
                self.arg_stack.push(y);
                self.arg_stack.push(x);
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
            Inst::Eqv => {
                let y = self.arg_stack.pop().expect("arg_stack empty!");
                let x = self.arg_stack.pop().expect("arg_stack empty!");
                self.arg_stack.push(Datum::Bool(x.eqv(&y)));
                self.frame.pc += 1;
                true
            },
            Inst::Uncons => {
                let arg = self.arg_stack.pop().expect("arg_stack empty!");
                if let Datum::Cons(pair) = arg {
                    self.arg_stack.push(pair.0.clone());
                    self.arg_stack.push(pair.1.clone());
                    self.frame.pc += 1;
                    true
                } else {
                    panic!("top of the stack is not a pair");
                }
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
    use super::{Inst, MemRef, PrimFuncPtr, Runtime, SimpleDatum};
    use datum::Datum;
    use primitive::PRIM_ADD;
    use number::Number;

    #[test]
    fn test_runtime() {
        let code = vec![
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1, 0)))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
            Inst::Call(2),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(Number::new_int(3, 0)));
    }

    #[test]
    fn test_nested_call() {
        let code = vec![
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(3, 0)))),
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1, 0)))),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
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
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
            Inst::PushArg(MemRef::Arg(0)),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
            Inst::Call(2),
            Inst::Return
        ];
        let code = vec![
            Inst::PushArg(MemRef::Closure(Rc::new(f), 0)),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1, 0)))),
            Inst::Call(1),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(Number::new_int(3, 0)));
    }

    #[test]
    fn test_closure() {
        let f = vec![
            Inst::PushArg(MemRef::PrimFunc(PrimFuncPtr::new("+", &PRIM_ADD))),
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
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
            Inst::Call(1),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(3, 0)))),
            Inst::Call(1),
            Inst::Return
        ];

        let mut runtime = Runtime::new(code);
        assert_eq!(runtime.run(), Datum::Num(Number::new_int(5, 0)));
    }
}
