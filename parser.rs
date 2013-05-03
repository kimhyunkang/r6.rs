mod r5rs {
    #[deriving(Eq)]
    enum LDatum {
        LIdent(~str),
        LString(~str),
        LChar(char),
    }

    struct Parser {
        priv reader: @io::Reader,
        priv buf: char,
        priv has_buf: bool,
        line: uint,
        col: uint,
    }

    pub fn Parser(reader: @io::Reader) -> Parser {
        Parser {
            reader: reader,
            has_buf: false,
            buf: 0 as char,
            line: 1,
            col: 1,
        }
    }

    priv fn id_init(c: char) -> bool {
        match(c) {
            '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~' | 'a' .. 'z' | 'A' .. 'Z' => true,
            _ => false,
        }
    }

    pub impl Parser {
        fn parse(&mut self) -> Result<LDatum, ~str> {
            do result::chain(self.parse_datum()) |v| {
                self.consume_whitespace();

                if(self.eof()) {
                    Ok(v)
                } else {
                    Err(~"trailing input")
                }
            }
        }
    }

    priv impl Parser {
        fn eof(&mut self) -> bool {
            !self.has_buf && self.reader.eof()
        }

        fn consume(&mut self) -> char {
            if(self.has_buf) {
                self.has_buf = false;
            } else {
                self.buf = self.reader.read_char();
            }

            if(self.buf == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }

            self.buf
        }

        fn lookahead(&mut self) -> char {
            if(!self.has_buf) {
                self.has_buf = true;
                self.buf = self.reader.read_char();
            }
            self.buf
        }

        fn consume_whitespace(&mut self) {
            while(!self.eof()) {
                match(self.lookahead()) {
                    ' '| '\t' | '\r' | '\n' => { self.consume(); },
                    _ => break
                }
            }
        }

        fn parse_datum(&mut self) -> Result<LDatum, ~str> {
            self.consume_whitespace();

            if(self.eof()) {
                return Err(~"unexpected EOF");
            }

            let c = self.consume();
            match(c) {
                _ if id_init(c) =>
                    Ok(LIdent(self.parse_ident(c))),
                '"' => {
                        match(self.parse_string()) {
                            Ok(s) => Ok(LString(s)),
                            Err(e) => Err(e),
                        }
                    },
                '#' => {
                        self.parse_sharp()
                    },
                _ =>
                    Err(~"unexpected character: " + str::from_char(c)),
            }
        }

        fn parse_sharp(&mut self) -> Result<LDatum, ~str> {
            if(self.eof()) {
                return Err(~"unexpected EOF");
            }

            let c = self.consume();
            match(c) {
                '\\' =>
                    do result::map(&self.parse_char()) |&c| {
                        LChar(c)
                    },
                _ =>
                    Err(~"unexpected character: " + str::from_char(c)),
            }
        }

        fn parse_char(&mut self) -> Result<char, ~str> {
            if(self.eof()) {
                Err(~"unexpected EOF")
            } else {
                Ok(self.consume())
            }
        }

        fn parse_ident(&mut self, init: char) -> ~str {
            do io::with_str_writer |wr| {
                wr.write_char(init);

                while(!self.eof()) {
                    let c = self.lookahead();

                    match(c) {
                        '0' .. '9' | '+' | '-' | '.' | '@' => wr.write_char(c),
                        _ if id_init(c) => wr.write_char(c),
                        _ => break,
                    }

                    self.consume();
                }
            }
        }

        fn parse_string(&mut self) -> Result<~str, ~str> {
            let mut escape_fail = false;

            let val = do io::with_str_writer |wr| {
                while(!self.eof()) {
                    let c = self.consume();
                    match(c) {
                        '\\' => if self.eof() {
                                    escape_fail = true;
                                } else {
                                    wr.write_char(self.consume());
                                },
                        '"' => break,
                        _ => wr.write_char(c),
                    }
                }
            };

            if(escape_fail) {
                Err(~"unexpected EOF")
            } else {
                Ok(val)
            }
        }
    }

    #[test]
    fn test_parse_ident() {
        let test_src = ~"a3!";

        do io::with_str_reader(test_src) |rdr| {
            let mut parser = Parser(rdr);
            let val = parser.parse();
            assert_eq!(Ok(LIdent(~"a3!")), val);
        }
    }

    #[test]
    fn test_parse_string() {
        let test_src = ~"\"ab\\\"c\"";

        do io::with_str_reader(test_src) |rdr| {
            let mut parser = Parser(rdr);
            let val = parser.parse();
            assert_eq!(Ok(LString(~"ab\"c")), val);
        }
    }

    #[test]
    fn test_parse_char() {
        let test_src = ~"#\\h";

        do io::with_str_reader(test_src) |rdr| {
            let mut parser = Parser(rdr);
            let val = parser.parse();
            assert_eq!(Ok(LChar('h')), val);
        }
    }
}
