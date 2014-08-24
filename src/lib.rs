#![crate_name = "r5"]

#![comment = "r5.rs library"]
#![license = "MIT/ASL2"]

#![feature(macro_rules)]
#![feature(globs)]

extern crate num;
#[cfg(test)]
extern crate debug;

pub mod primitive;
pub mod datum;
pub mod bigint_helper;
pub mod real;
pub mod numeric;
pub mod parser;
