#![crate_name = "r6"]

//! r6.rs is an attempt to implement R6RS Scheme in Rust language

#![feature(plugin)]
#![feature(box_syntax)]

#![feature(core)]
#![feature(std_misc)]
#![feature(io)]
#![feature(collections)]
#![feature(unicode)]

#![plugin(phf_macros)]
#![plugin(regex_macros)]

// This line should be at the top of the extern link list,
// because some weird compiler bug lets log imported from rustc, not crates.io log
#[macro_use] extern crate log;
#[cfg(test)] extern crate env_logger;

extern crate phf;
extern crate regex;
extern crate unicode;
extern crate num;

macro_rules! list{
    ($($x:expr),*) => (
        vec![$($x),*].into_iter().collect()
    )
}

macro_rules! sym{
    ($e:expr) => (
        Datum::Ptr(Rc::new(box Cow::Borrowed($e)))
    )
}

macro_rules! num{
    ($e:expr) => (
        Datum::Ptr(Rc::new(box Real::Fixnum($e)))
    )
}

macro_rules! nil{
    () => (
        Datum::Nil
    )
}

/// Error values returned from parser, compiler or runtime
pub mod error;
/// Basic datum types
pub mod datum;
pub mod parser;
pub mod lexer;
/// Virtual machine running the bytecode
pub mod runtime;
/// Primitive functions
pub mod primitive;
/// Compiles datum into a bytecode
pub mod compiler;
/// R6RS `base` library
pub mod base;
/// Real part of the numerical tower
pub mod real;
/// Numerical tower
pub mod number;
pub mod num_trait;
