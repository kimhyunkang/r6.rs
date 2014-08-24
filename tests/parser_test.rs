#![feature(globs)]

extern crate r5;

use std::io::File;
use std::rc::Rc;
use r5::parser::Parser;
use r5::datum::*;

#[test]
fn file_parse_test() {
    let file = File::open(&Path::new("parse_test.scm")).unwrap();
    let mut parser = Parser::new(file);
    let result: Result<LDatum<()>, String> = parser.parse();
    let expected: LDatum<()> = LCons(Rc::new(LIdent("+".to_string())), Rc::new(LNil));
    assert_eq!(result, Ok(expected))
}
