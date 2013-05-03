test: parser-tests
	./parser-tests

parser-tests: parser.rs
	rustc $^ --test -o $@
