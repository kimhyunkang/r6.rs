extern mod r5;

use std::io;
use r5::runtime::{Runtime};

fn prompt() {
    print("repl > ")
}

fn main() {
    let mut runtime = Runtime::new_std();
    print("load prelude... ");
    match io::file_reader(&Path("prelude.scm")) {
        Ok(rdr) => match runtime.load(rdr) {
            Ok(_) => println("ok\n"),
            Err(e) => fail!("Error: %s", e.to_str()),
        },
        Err(e) => fail!("failed to open prelude: %s", e)
    };

    prompt();
    for io::stdin().each_line |line| {
        do io::with_str_reader(line) |rdr| {
            match runtime.load(rdr) {
                Ok(result) => {
                    result.write(io::stdout());
                    print("\n");
                },
                Err(e) => println(fmt!("Error: %s", e.to_str())),
            }
        }
        prompt();
    }
    println("^D");
}
