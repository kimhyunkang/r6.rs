extern crate r6;
extern crate copperline;

use std::process::exit;

use copperline::{Copperline, Encoding};
use copperline::Error as CopperlineError;

use r6::base::{libbase, base_syntax};
use r6::datum::Datum;
use r6::error::ParserErrorKind;
use r6::parser::Parser;
use r6::runtime::Runtime;

fn read(cl: &mut Copperline) -> Result<Datum<()>, String> {
    let mut input = match cl.read_line(">> ", Encoding::Utf8) {
        Ok(l) => l,
        Err(CopperlineError::EndOfFile) => {
            exit(0);
        },
        Err(e) => {
            println!("{}", e);
            exit(1);
        }
    };

    if let Some(datum) = try!(parse(input.as_bytes())) {
        return Ok(datum)
    }

    loop {
        let line = match cl.read_line(".. ", Encoding::Utf8) {
            Ok(l) => l,
            Err(CopperlineError::EndOfFile) => {
                println!("^D");
                exit(0);
            },
            Err(e) => {
                println!("{}", e);
                exit(1);
            }
        };

        input.push_str("\n");
        input.push_str(&line);

        if let Some(datum) = try!(parse(input.as_bytes())) {
            return Ok(datum)
        }
    }
}

fn parse(input: &[u8]) -> Result<Option<Datum<()>>, String> {
    let mut parser = Parser::new(input);
    match parser.parse_full() {
        Ok(code) => Ok(Some(code)),
        Err(e) => match e.kind {
            ParserErrorKind::UnexpectedEOF => Ok(None),
            _ => Err(e.to_string())
        }
    }
}

fn main() {
    let mut cl = Copperline::new();
    let mut runtime = Runtime::new(libbase(), base_syntax());

    loop {
        match read(&mut cl) {
            Ok(code) => match runtime.eval(&code) {
                Ok(v) => println!("{}", v),
                Err(e) => println!("Error: {}", e)
            },
            Err(e) => {
                println!("Error: {}", e);
            }
        }
    }
}
