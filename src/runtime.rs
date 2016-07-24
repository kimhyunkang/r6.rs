use std::rc::Rc;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::fmt;
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};

use cast::DatumCast;
use compiler::{Compiler, Syntax};
use datum::TryConv;
use eqv::DatumEqv;
use error::{CompileError, CompileErrorKind, RuntimeError, RuntimeErrorKind};
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

impl TryConv<(), CompileError> for RuntimeData {
    fn try_conv(&self) -> Result<(), CompileError> {
        Err(CompileError { kind: CompileErrorKind::InvalidDatum(format!("{:?}", self)) })
    }
}

/// Compiled closure object
#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    // Pointer to the bytecode
    pub code: Rc<Vec<Inst>>,
    // The lexical environment directly enclosing the code
    pub static_link: Option<StaticLink>,
    // Source code
    source: Option<Rc<Datum<()>>>
}

impl DatumEqv for Closure {
    fn eqv(&self, other: &Closure) -> bool {
        self.code.eqv(&other.code) && self.static_link.eqv(&other.static_link)
    }
}

impl Closure {
    pub fn new(code: Rc<Vec<Inst>>, static_link: Option<StaticLink>, source: Option<Datum<()>>) -> Closure {
        Closure {
            code: code,
            static_link: static_link,
            source: source.map(|x| Rc::new(x))
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

impl fmt::Display for RuntimeData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &RuntimeData::PrimFunc(ref func_ptr) =>
                write!(f, "<primitive: {:?}>", func_ptr.name),
            &RuntimeData::Closure(ref closure) => {
                try!(write!(f, "<procedure"));
                match closure.source {
                    None => write!(f, ">"),
                    Some(ref ptr) => write!(f, ": {:?}>", ptr.deref())
                }
            },
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
    Global(Rc<RefCell<RDatum>>),
    Const(SimpleDatum),
    Undefined,
    PrimFunc(PrimFuncPtr),
    Closure(Rc<Vec<Inst>>, usize, Option<Datum<()>>)
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
    /// pop value from the stack and push to the new global variable
    PopGlobal(Cow<'static, str>),
    /// pop value from the stack and remove it
    DropArg(usize),
    /// swap two top values of the stack
    SwapArg,
    /// roll args [n..] into a list
    RollArgs(usize),
    /// pop pair(h, t) from the top of the list, then push h, then push t
    Uncons,
    /// compare two top values of the stack with `eqv?` operator
    Eqv,
    /// compare two top values of the stack with `equal?` operator
    Equal,
    /// compare top of the stack with given pointer, and push `#t` if the type equals
    Type(DatumType),
    /// call the function in (stack_top - n)
    Call(usize),
    /// call the function in (stack_top - n)
    TailCall,
    /// call the function at the bottom of the current frame
    CallSplicing,
    /// pop the call stack frame, and return to the call site
    Return,
    /// push the call stack without jumping, and move stack_bottom to (stack_top - n)
    PushFrame(usize),
    /// update the argument size of the current frame, letting the PopFrame correctly deallocate
    /// the stack variables
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
pub struct HeapClosure {
    args: Vec<RDatum>,
    static_link: Option<StaticLink>
}

impl HeapClosure {
    fn debug_fmt(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
        try!(write!(f, "HeapClosure(args: ["));
        let mut first = true;
        for arg in self.args.iter() {
            if first {
                first = false;
            } else {
                try!(write!(f, ", "))
            }

            if let &Datum::Ext(RuntimeData::Closure(Closure {code: _, ref source, static_link: Some(ref static_link)})) = arg {
                try!(write!(f, "Ext(Closure(source: {:?}, static_link: ", source));
                try!(
                    match static_link.borrow().deref() {
                        &ScopePtr::Stack(n) => write!(f, "Stack({})", n),
                        &ScopePtr::Heap(ref heap_closure) =>
                            if depth == 0 {
                                write!(f, "HeapClosure(..)")
                            } else {
                                heap_closure.debug_fmt(f, depth-1)
                            }
                    }
                );
                try!(write!(f, "))"));
            } else {
                try!(write!(f, "{:?}", arg));
            }
        }
        try!(write!(f, "], static_link: "));
        try!(
            match &self.static_link {
                &None => write!(f, "None"),
                &Some(ref ptr) => match ptr.borrow().deref() {
                    &ScopePtr::Stack(n) => write!(f, "Stack({})", n),
                    &ScopePtr::Heap(ref heap_closure) =>
                        if depth == 0 {
                            write!(f, "HeapClosure(..)")
                        } else {
                            heap_closure.debug_fmt(f, depth-1)
                        }
                }
            }
        );
        write!(f, ")")
    }
}

impl PartialEq for HeapClosure {
    fn eq(&self, other: &HeapClosure) -> bool {
        (self as *const HeapClosure) == (other as *const HeapClosure)
    }
}

impl fmt::Debug for HeapClosure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.debug_fmt(f, 10)
    }
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
    frame: StackFrame,
    global: HashMap<Cow<'static, str>, Rc<RefCell<RDatum>>>,
    compiler: Compiler
}

fn runtime_panic(msg: String) -> RuntimeError {
    RuntimeError {
        kind: RuntimeErrorKind::Panic,
        desc: msg
    }
}

impl Runtime {
    /// Create the new virtual machine with given code
    pub fn new(base: HashMap<Cow<'static, str>, Rc<RefCell<RDatum>>>, base_syntax: HashMap<Cow<'static, str>, Syntax>) -> Runtime {
        Runtime {
            ret_val: Datum::Nil,
            arg_stack: Vec::new(),
            call_stack: Vec::new(),
            frame: StackFrame {
                closure: Closure { code: Rc::new(Vec::new()), static_link: None, source: None },
                pc: 0,
                stack_bottom: 0,
                arg_size: 0,
                self_link: Rc::new(RefCell::new(ScopePtr::Stack(0)))
            },
            global: base,
            compiler: Compiler::new(base_syntax)
        }
    }

    pub fn load_main(&mut self, code: Vec<Inst>, source: Option<Datum<()>>) {
        let closure = Closure {
            code: Rc::new(code),
            static_link: None,
            source: source.map(Rc::new)
        };

        self.arg_stack = vec![Datum::Ext(RuntimeData::Closure(closure.clone()))];
        self.call_stack = Vec::new();
        self.frame = StackFrame {
            closure: closure,
            pc: 0,
            stack_bottom: 1,
            arg_size: 0,
            self_link: Rc::new(RefCell::new(ScopePtr::Stack(0)))
        }
    }

    pub fn eval<T>(&mut self, datum: &Datum<T>) -> Result<RDatum, RuntimeError>
        where T: Clone + Debug + TryConv<(), CompileError>
    {
        debug!("eval {:?}", datum);

        let code = match self.compiler.compile(&self.global, datum) {
            Ok(c) => c,
            Err(e) => return Err(RuntimeError {
                kind: RuntimeErrorKind::CompileError,
                desc: format!("{:?}", e.kind)
            })
        };

        let src: Datum<()> = try!(datum.try_conv());
        self.load_main(code, Some(src));
        self.run()
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

    fn get_upvalue(&self, link_cnt: usize, arg_idx: usize) -> Result<RDatum, RuntimeError> {
        let mut link_res = self.frame.closure.static_link.clone();
        for _ in 0 .. link_cnt {
            link_res = self.up_scope(link_res);
        }
        if let Some(link) = link_res {
            match *link.borrow() {
                ScopePtr::Heap(ref data) => Ok(data.args[arg_idx].clone()),
                ScopePtr::Stack(n) => {
                    let frame_ref = if n == self.call_stack.len() {
                        &self.frame
                    } else {
                        &self.call_stack[n]
                    };
                    let bot = frame_ref.stack_bottom;
                    Ok(self.arg_stack[bot + arg_idx].clone())
                }
            }
        } else {
            Err(runtime_panic(format!("get_upvalue({:?}, {:?}) failed!", link_cnt, arg_idx)))
        }
    }

    fn set_upvalue(&mut self, link_cnt: usize, arg_idx: usize, val: RDatum) -> Result<(), RuntimeError> {
        let mut link_res = self.frame.closure.static_link.clone();
        for _ in 0 .. link_cnt {
            link_res = self.up_scope(link_res);
        }
        if let Some(link) = link_res {
            match link.borrow_mut().deref_mut() {
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
            Ok(())
        } else {
            Err(runtime_panic(format!("set_upvalue({:?}, {:?}) failed!", link_cnt, arg_idx)))
        }
    }

    fn fetch_mem(&self, ptr: MemRef) -> Result<RDatum, RuntimeError> {
        let val = match ptr {
            MemRef::RetVal => self.ret_val.clone(),
            MemRef::Arg(idx) => self.get_stack_val(idx),
            MemRef::UpValue(i, j) => try!(self.get_upvalue(i, j)),
            MemRef::Const(val) => DatumCast::wrap(val),
            MemRef::Global(data) => data.borrow().clone(),
            MemRef::Undefined => Datum::Ext(RuntimeData::Undefined),
            MemRef::PrimFunc(ptr) => Datum::Ext(RuntimeData::PrimFunc(ptr)),
            MemRef::Closure(code, _, src) => Datum::Ext(RuntimeData::Closure(
                Closure {
                    code: code.clone(),
                    static_link: Some(self.frame.self_link.clone()),
                    source: src.map(Rc::new)
                }
            ))
        };

        Ok(val)
    }

    fn write_mem(&mut self, ptr: MemRef, val: RDatum) -> Result<(), RuntimeError> {
        match ptr {
            MemRef::RetVal => {
                self.ret_val = val;
            },
            MemRef::Arg(idx) => {
                self.arg_stack[self.frame.stack_bottom + idx] = val;
            },
            MemRef::UpValue(i, j) => return self.set_upvalue(i, j, val),
            MemRef::Global(ptr) => { *(ptr.borrow_mut()) = val },
            MemRef::Const(_) => return Err(runtime_panic("Cannot write to read-only memory".to_string())),
            MemRef::Undefined => return Err(runtime_panic("Cannot write to undefined memory address".to_string())),
            MemRef::PrimFunc(_) => return Err(runtime_panic("Cannot write to code area".to_string())),
            MemRef::Closure(_, _, _) => return Err(runtime_panic("Cannot write to instruction memory".to_string()))
        }

        Ok(())
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

    pub fn pop_stack(&mut self) -> Result<RDatum, RuntimeError> {
        match self.arg_stack.pop() {
            Some(x) => Ok(x),
            None => Err(runtime_panic("arg_stack empty!".to_string()))
        }
    }

    pub fn peek_stack(&self) -> Result<&RDatum, RuntimeError> {
        match self.arg_stack.last() {
            Some(x) => Ok(x),
            None => Err(runtime_panic("arg_stack empty!".to_string()))
        }
    }

    pub fn top_is_false(&self) -> Result<bool, RuntimeError> {
        match try!(self.peek_stack()) {
            &Datum::Bool(false) => Ok(true),
            _ => Ok(false)
        }
    }

    fn call(&mut self, n: usize) -> Result<(), RuntimeError> {
        let top = self.arg_stack.len();
        let datum = self.arg_stack[top - n - 1].clone();
        match datum {
            Datum::Ext(RuntimeData::PrimFunc(fptr)) => {
                let args = if n == 0 {
                    Vec::new()
                } else {
                    self.arg_stack.split_off(top-n)
                };
                let res = try!(fptr.function.call(args));
                try!(self.pop_stack());
                self.push_stack(res);
                self.frame.pc += 1;
            },
            Datum::Ext(RuntimeData::Closure(closure)) => {
                self.push_call_stack(n, closure);
            },
            _ => {
                return Err(runtime_panic(format!("{:?} is not callable", datum)))
            }
        }

        Ok(())
    }

    fn tail_call(&mut self) -> Result<(), RuntimeError> {
        let n = self.frame.arg_size;
        let cur_bottom = self.frame.stack_bottom;
        let new_bottom = cur_bottom + n;
        let mut args = if new_bottom+1 == self.arg_stack.len() {
            Vec::new()
        } else {
            self.arg_stack.split_off(new_bottom+1)
        };
        let datum = try!(self.pop_stack());

        match datum {
            Datum::Ext(RuntimeData::PrimFunc(ref fptr)) => {
                let res = try!(fptr.function.call(args));
                self.push_stack(res);
                self.frame.pc += 1;
            },
            Datum::Ext(RuntimeData::Closure(ref closure)) => {
                let heap = HeapClosure {
                    args: self.arg_stack[cur_bottom .. new_bottom].to_vec(),
                    static_link: self.frame.closure.static_link.clone()
                };
                *self.frame.self_link.borrow_mut() = ScopePtr::Heap(heap);

                self.arg_stack.split_off(cur_bottom-1);
                self.frame.closure = closure.clone();
                self.frame.pc = 0;
                self.frame.arg_size = args.len();
                let idx = self.call_stack.len();
                self.frame.self_link = Rc::new(RefCell::new(ScopePtr::Stack(idx)));
                self.arg_stack.push(datum.clone());
                self.arg_stack.append(&mut args);
            },
            _ => {
                return Err(runtime_panic(format!("{:?} is not callable", datum)))
            }
        }

        Ok(())
    }

    fn return_value(&mut self) -> Result<bool, RuntimeError> {
        let n = self.frame.arg_size;
        let top = self.arg_stack.len();
        let res = self.pop_call_stack();
        let retval = try!(self.pop_stack());
        if top < n+2 {
            return Err(runtime_panic("stack too low".to_string()));
        }
        self.arg_stack.truncate(top - n - 2);
        self.push_stack(retval);
        if res {
            self.frame.pc += 1;
        }
        return Ok(res)
    }

    fn step(&mut self) -> Result<bool, RuntimeError> {
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
            },
            Inst::Type(typeinfo) => {
                let val = {
                    let arg = try!(self.peek_stack());
                    DatumType::get_type(arg) == typeinfo
                };
                self.arg_stack.push(Datum::Bool(val));
                self.frame.pc += 1;
            },
            Inst::Call(n) => try!(self.call(n)),
            Inst::TailCall => try!(self.tail_call()),
            Inst::CallSplicing => {
                let n_args = self.arg_stack.len() - self.frame.stack_bottom;
                if n_args == 0 {
                    return Err(runtime_panic("Call args empty".to_string()));
                }
                try!(self.call(n_args - 1));
                self.frame.arg_size = 0;
            },
            Inst::PushFrame(n) => {
                let new_closure = Closure {
                    code: self.frame.closure.code.clone(),
                    static_link: Some(self.frame.self_link.clone()),
                    source: self.frame.closure.source.clone()
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
            },
            Inst::SetArgSize(n) => {
                self.frame.arg_size = n;
                self.frame.pc += 1;
            },
            Inst::PopFrame => {
                let pc = self.frame.pc;
                let n = self.frame.arg_size;
                let top = self.arg_stack.len();
                self.pop_call_stack();
                let retval = try!(self.pop_stack());
                if top < n+1 {
                    return Err(runtime_panic("stack too low".to_string()));
                }
                self.arg_stack.truncate(top - n - 1);
                self.push_stack(retval);
                self.frame.pc = pc+1;
            },
            Inst::Jump(pc) => {
                self.frame.pc = pc;
            },
            Inst::JumpIfFalse(pc) =>
                if try!(self.top_is_false()) {
                    self.frame.pc = pc;
                } else {
                    self.frame.pc += 1;
                },
            Inst::JumpIfNotFalse(pc) =>
                if try!(self.top_is_false()) {
                    self.frame.pc += 1;
                } else {
                    self.frame.pc = pc;
                },
            Inst::ThrowIfFalse(msg) =>
                if try!(self.top_is_false()) {
                    return Err(runtime_panic(msg.to_string()));
                } else {
                    self.frame.pc += 1;
                },
            Inst::PushArg(ptr) => {
                let val = try!(self.fetch_mem(ptr));
                self.arg_stack.push(val);
                self.frame.pc += 1;
            },
            Inst::PopArg(ptr) => {
                let val = try!(self.pop_stack());
                try!(self.write_mem(ptr, val));
                self.frame.pc += 1;
            },
            Inst::PopGlobal(sym) => {
                let val = try!(self.pop_stack());
                self.global.insert(sym, Rc::new(RefCell::new(val)));
                self.frame.pc += 1;
            },
            Inst::DropArg(n) => {
                if n > self.arg_stack.len() {
                    return Err(runtime_panic("arg_stack too low".to_string()));
                }
                let new_size = self.arg_stack.len() - n;
                self.arg_stack.truncate(new_size);
                self.frame.pc += 1;
            },
            Inst::SwapArg => {
                let y = try!(self.pop_stack());
                let x = try!(self.pop_stack());
                self.arg_stack.push(y);
                self.arg_stack.push(x);
                self.frame.pc += 1;
            },
            Inst::RollArgs(n) => {
                let vararg_start = self.frame.stack_bottom + n;
                let list: RDatum = self.arg_stack[vararg_start ..].iter().map(Clone::clone).collect();
                self.arg_stack.truncate(vararg_start);
                self.push_stack(list);
                self.frame.arg_size = n+1;
                self.frame.pc += 1;
            },
            Inst::Eqv => {
                let n = self.arg_stack.len();
                if n < 2 {
                    return Err(runtime_panic("arg_stack too low!".to_string()));
                }
                let b = {
                    let y = &self.arg_stack[n-1];
                    let x = &self.arg_stack[n-2];
                    x.eqv(y)
                };
                self.arg_stack.push(Datum::Bool(b));
                self.frame.pc += 1;
            },
            Inst::Equal => {
                let n = self.arg_stack.len();
                if n < 2 {
                    return Err(runtime_panic("arg_stack too low!".to_string()));
                }
                let b = {
                    let y = &self.arg_stack[n-1];
                    let x = &self.arg_stack[n-2];
                    x == y
                };
                self.arg_stack.push(Datum::Bool(b));
                self.frame.pc += 1;
            },
            Inst::Uncons => {
                let arg = try!(self.pop_stack());
                if let Datum::Cons(pair) = arg {
                    self.arg_stack.push(pair.0.clone());
                    self.arg_stack.push(pair.1.clone());
                    self.frame.pc += 1;
                } else {
                    return Err(runtime_panic("top of the stack is not a pair".to_string()));
                }
            },
            Inst::Return => return self.return_value()
        }

        Ok(true)
    }

    pub fn run(&mut self) -> Result<RDatum, RuntimeError> {
        loop {
            let cont = try!(self.step());
            if !cont {
                return self.pop_stack();
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::rc::Rc;
    use super::{Inst, MemRef, PrimFuncPtr, Runtime, SimpleDatum};
    use base::libbase;
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

        let mut runtime = Runtime::new(libbase(), HashMap::new());
        runtime.load_main(code, None);
        assert_eq!(runtime.run(), Ok(Datum::Num(Number::new_int(3, 0))));
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

        let mut runtime = Runtime::new(libbase(), HashMap::new());
        runtime.load_main(code, None);
        assert_eq!(runtime.run(), Ok(Datum::Num(Number::new_int(6, 0))));
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
            Inst::PushArg(MemRef::Closure(Rc::new(f), 0, None)),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(1, 0)))),
            Inst::Call(1),
            Inst::Return
        ];

        let mut runtime = Runtime::new(libbase(), HashMap::new());
        runtime.load_main(code, None);
        assert_eq!(runtime.run(), Ok(Datum::Num(Number::new_int(3, 0))));
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
            Inst::PushArg(MemRef::Closure(Rc::new(f), 1, None)),
            Inst::Return
        ];

        // ((
        //   (lambda (x)            # = g
        //     (lambda (y) (+ x y)) # = f
        //   ) 2) 3)
        let code = vec![
            Inst::PushArg(MemRef::Closure(Rc::new(g), 0, None)),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(2, 0)))),
            Inst::Call(1),
            Inst::PushArg(MemRef::Const(SimpleDatum::Num(Number::new_int(3, 0)))),
            Inst::Call(1),
            Inst::Return
        ];

        let mut runtime = Runtime::new(libbase(), HashMap::new());
        runtime.load_main(code, None);
        assert_eq!(runtime.run(), Ok(Datum::Num(Number::new_int(5, 0))));
    }
}
