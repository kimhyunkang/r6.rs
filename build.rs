extern crate phf_codegen;

use std::env;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

fn main() {
    codegen_special_token_map();
    codegen_char_map();
}

fn codegen_special_token_map() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("special_token_map.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    write!(&mut file, "static SPECIAL_TOKEN_MAP: phf::Map<&'static str, &'static str> = ").unwrap();
    phf_codegen::Map::new()
        .entry("quote", r##""'""##)
        .entry("quasiquote", r##""`""##)
        .entry("unquote", r##"",""##)
        .entry("unquote-splicing", r##"",@""##)
        .entry("syntax", r##""#'""##)
        .entry("quasisyntax", r##""#`""##)
        .entry("unsyntax", r##""#,""##)
        .entry("unsyntax-splicing", r##""#,@""##)
        .build(&mut file)
        .unwrap();
    write!(&mut file, ";\n").unwrap();
}

fn codegen_char_map() {
    let path = Path::new(&env::var("OUT_DIR").unwrap()).join("char_map.rs");
    let mut file = BufWriter::new(File::create(&path).unwrap());

    write!(&mut file, "static CHAR_MAP: phf::Map<&'static str, char> = ").unwrap();
    phf_codegen::Map::new()
        .entry("nul", "'\\0'")
        .entry("alarm", "'\\x07'")
        .entry("backspace", "'\\x08'")
        .entry("tab", "'\\t'")
        .entry("newline", "'\\n'")
        .entry("linefeed", "'\\n'")
        .entry("vtab", "'\\x0b'")
        .entry("page", "'\\x0c'")
        .entry("return", "'\\r'")
        .entry("esc", "'\\x1b'")
        .entry("space", "' '")
        .entry("delete", "'\\x7f'")
        .build(&mut file)
        .unwrap();
    write!(&mut file, ";\n").unwrap();
}
