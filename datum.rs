use numeric::LNumeric;
use primitive::PFunc;
use core::hashmap::linear::LinearMap;

pub enum Stack<T> {
    Top(T, @Stack<T>),
    Bot,
}

pub enum LDatum {
    LIdent(@str),
    LString(~str),
    LChar(char),
    LBool(bool),
    LNum(LNumeric),
    LCons(@LDatum, @LDatum),
    LNil,
    LPrim(PFunc),
    LQuote(@LDatum),
    LQQuote(@LDatum),
    LUnquote(@LDatum),
    LProc(~[@str], ~[@LDatum], @Stack<LinearMap<@str, @LDatum>>),
}

fn eq(&lhs: &LDatum, &rhs: &LDatum) -> bool {
    ptr::ref_eq(&lhs, &rhs) ||
    match (lhs, rhs) {
        (LIdent(l), LIdent(r)) => l == r,
        (LString(l), LString(r)) => l == r,
        (LChar(l), LChar(r)) => l == r,
        (LBool(l), LBool(r)) => l == r,
        (LNum(l), LNum(r)) => l == r,
        (LCons(lh, lt), LCons(rh, rt)) => lh == rh && lt == rt,
        (LNil, LNil) => true,
        (LPrim(l), LPrim(r)) => l == r,
        (LQuote(l), LQuote(r)) => l == r,
        (LQQuote(l), LQQuote(r)) => l == r,
        (LUnquote(l), LUnquote(r)) => l == r,
        _ => false,
    }
}

impl Eq for LDatum {
    fn eq(&self, other: &LDatum) -> bool {
        eq(self, other)
    }

    fn ne(&self, other: &LDatum) -> bool {
        !eq(self, other)
    }
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
    let addr = ptr::to_uint(&v);
    match v {
        LIdent(s) => wr.write_str(s),
        LString(s) => wr.write_str(fmt!("%?", copy s)),
        LChar(' ') => wr.write_str("#\\space"),
        LChar('\n') => wr.write_str("#\\newline"),
        LChar(c) => {
            wr.write_str("#\\");
            wr.write_char(c);
        },
        LBool(true) => wr.write_str("#t"),
        LBool(false) => wr.write_str("#f"),
        LNum(f) => wr.write_str(f.to_str()),
        LCons(head, tail) => {
            wr.write_char('(');
            write_ldatum(wr, head);
            write_list(wr, tail);
        },
        LNil => wr.write_str("()"),
        LPrim(f) => {
            wr.write_str("<primitive function ");
            wr.write_str(f.to_str());
            wr.write_char('>');
        },
        LQuote(v) => {
            wr.write_char('\'');
            write_ldatum(wr, v);
        },
        LQQuote(v) => {
            wr.write_char('`');
            write_ldatum(wr, v);
        },
        LUnquote(v) => {
            wr.write_char(',');
            write_ldatum(wr, v);
        },
        LProc(_,_,_) => {
            wr.write_str(fmt!("<procedure 0x%08x>", addr));
        },
    }
}

priv fn write_list(wr: @io::Writer, &v: &LDatum) {
    match v {
        LCons(head, tail) => {
            wr.write_char(' ');
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
