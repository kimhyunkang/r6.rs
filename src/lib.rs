#![crate_name = "r6"]

//! r6.rs is an attempt to implement R6RS Scheme in Rust language

#![feature(plugin)]
#![feature(slicing_syntax)]
#![feature(box_syntax)]
//TODO: Allow unstable items until Rust hits 1.0
#![allow(unstable)]

#[plugin]
extern crate phf_mac;
extern crate phf;
extern crate unicode;
extern crate num;

#[macro_use]
extern crate log;

macro_rules! list{
    ($($x:expr),*) => (
        vec![$($x),*].into_iter().collect()
    )
}

macro_rules! sym{
    ($e:expr) => (
        Datum::Sym(Cow::Borrowed($e))
    )
}

macro_rules! num{
    ($e:expr) => (
        Datum::Num($e)
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
