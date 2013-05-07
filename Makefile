test: parser-tests
	./parser-tests

parser-tests: r5.rc parser.rs rational.rs complex.rs
	rustc $< --test -o $@
