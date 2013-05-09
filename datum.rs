use numeric::LNumeric;

#[deriving(Eq)]
pub enum LDatum {
    LIdent(~str),
    LString(~str),
    LChar(char),
    LBool(bool),
    LNum(LNumeric),
    LCons(@LDatum, @LDatum),
    LNil,
}

impl ToStr for LDatum {
    fn to_str(&self) -> ~str {
        do io::with_str_writer |wr| {
            self.write(wr)
        }
    }

}

pub impl LDatum {
    fn to_list(&self) -> Option<~[@LDatum]> {
        match *self {
            LCons(h, t) => datum_to_list(h, t),
            LNil => Some(~[]),
            _ => None,
        }
    }

    fn write(&self, writer: @io::Writer) {
        write_ldatum(writer, self)
    }
}

priv fn datum_to_list(head: @LDatum, tail: @LDatum) -> Option<~[@LDatum]> {
    let mut x: @LDatum = tail;
    let mut list_flag = false;

    let list =
    do vec::build |push| {
        push(head);
        loop {
            match *x {
                LCons(h, t) => {
                    push(h);
                    x = t;
                },
                LNil => break,
                _ => {
                    list_flag = true;
                    break
                },
            }
        }
    };

    if list_flag {
        None
    } else {
        Some(list)
    }
}

priv fn write_ldatum(wr: @io::Writer, &v: &LDatum) {
    match v {
        LIdent(s) => wr.write_str(s),
        LString(s) => wr.write_str(fmt!("%?", copy s)),
        LChar(' ') => wr.write_str("#\\space"),
        LChar('\n') => wr.write_str("#\\newline"),
        LChar(c) => {
            wr.write_str("#\\");
            wr.write_char(c);
        }
        LBool(true) => wr.write_str("#t"),
        LBool(false) => wr.write_str("#f"),
        LNum(f) => wr.write_str(f.to_str()),
        LCons(head, tail) => {
            wr.write_char('(');
            write_ldatum(wr, head);
            write_list(wr, tail);
        },
        LNil => wr.write_str("()"),
    }
}

priv fn write_list(wr: @io::Writer, &v: &LDatum) {
    match v {
        LCons(head, tail) => {
            wr.write_str(", ");
            write_ldatum(wr, head);
            write_list(wr, tail);
        },
        LNil => wr.write_char(')'),
        _ => {
            wr.write_str(" . ");
            write_ldatum(wr, &v);
            wr.write_char(')');
        },
    }
}
