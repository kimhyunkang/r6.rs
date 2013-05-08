all: repl.rs lib
	rustc $< -L .

test: parser-tests
	./parser-tests

lib: r5.rc parser.rs rational.rs complex.rs datum.rs runtime.rs
	rustc $< --lib

parser-tests: r5.rc parser.rs rational.rs complex.rs datum.rs runtime.rs
	rustc $< --test -o $@

clean:
	rm -rf parser-tests *.dSYM *.dylib repl
