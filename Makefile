LIB=libr5-84d4359e9b178dfc-0.0.1.dylib

repl: repl.rs ${LIB}
	rustc $< -L .

test: parser-test eval-test

parser-test: parser-tests
	./parser-tests

eval-test: eval-tests
	./eval-tests

${LIB}: r5.rc parser.rs rational.rs datum.rs runtime.rs macro.rs stack.rs
	rustc $< --lib

parser-tests: r5.rc parser.rs rational.rs datum.rs runtime.rs macro.rs stack.rs
	rustc $< --test -o $@

eval-tests: eval_test.rs ${LIB}
	rustc $< --test -L . -o $@

clean:
	rm -rf *-tests *.dSYM *.dylib repl
