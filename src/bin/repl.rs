extern crate r6;
extern crate copperline;

use copperline::{Copperline, Encoding};

use r6::base::{libbase, base_syntax};
use r6::parser::Parser;
use r6::runtime::Runtime;

fn main() {
    let mut cl = Copperline::new();
    let mut runtime = Runtime::new(libbase(), base_syntax());

    loop {
        let line = match cl.read_line("> ", Encoding::Utf8) {
            Ok(l) => l,
            Err(e) => {
                println!("{}", e);
                return;
            }
        };

        let mut parser = Parser::new(line.as_bytes());
        match parser.parse_datum::<()>() {
            Ok(code) => match runtime.eval(&code) {
                Ok(v) => println!("{:?}", v),
                Err(e) => println!("Error: {}", e)
            },
            Err(e) => println!("Error: {}", e)
        };

    }
}
