extern mod r5;

use r5::runtime::{Runtime};
use r5::parser::Parser;

fn prompt() {
    print("repl > ")
}

fn main() {
    let mut runtime = Runtime::new_std();

    prompt();
    for io::stdin().each_line |line| {
        do io::with_str_reader(line) |rdr| {
            let mut parser = Parser(rdr);
            match parser.parse() {
                Ok(datum) => {
                    match runtime.eval(@datum) {
                        Ok(result) => {
                            result.write(io::stdout());
                            print("\n");
                        },
                        Err(e) => println(fmt!("Error: %s", e.to_str())),
                    }
                }
                Err(e) => {
                    let (line, col) = parser.pos();
                    println(fmt!("Parse Error: %u:%u:%s", line, col, e))
                },
            }
        }
        prompt();
    }
    println("^D");
}
