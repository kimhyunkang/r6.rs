LIB=libr5-50612505c34cd7-0.0.1.dylib

repl: repl.rs ${LIB}
	rustc $< -L .

test: parser-test eval-test

parser-test: parser-tests
	./parser-tests

eval-test: eval-tests
	./eval-tests

${LIB}: r5.rc parser.rs rational.rs complex.rs datum.rs runtime.rs
	rustc $< --lib

parser-tests: r5.rc parser.rs rational.rs complex.rs datum.rs runtime.rs
	rustc $< --test -o $@

eval-tests: eval_test.rs ${LIB}
	rustc $< --test -L . -o $@

clean:
	rm -rf *-tests *.dSYM *.dylib repl
