#![crate_name = "r6"]

#![feature(plugin)]
#![feature(slicing_syntax)]
#![feature(box_syntax)]
//TODO: Allow unstable items until Rust hits 1.0
#![allow(unstable)]

#[plugin]
extern crate phf_mac;
extern crate phf;
extern crate unicode;

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

pub mod error;
pub mod datum;
pub mod parser;
pub mod lexer;
pub mod runtime;
pub mod primitive;
pub mod compiler;
