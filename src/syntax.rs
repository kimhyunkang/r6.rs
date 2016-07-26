use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Debug;

use datum::{Datum, SimpleDatum};
use error::{MacroError, MacroErrorKind};

type Vars = HashSet<Cow<'static, str>>;

macro_rules! hashset {
    ($($e:expr),*) => ({
        let mut s = HashSet::new();
        $( s.insert($e); )*
        s
    })
}

#[derive(Debug)]
struct CompiledPattern {
    vars: Vars,
    pattern: Pattern
}

#[derive(Debug)]
enum Pattern {
    List(Vec<SubPattern>),
    DelimitedList(Vec<SubPattern>, Box<SubPattern>, Vec<SubPattern>)
}

#[derive(Debug)]
enum SubPattern {
    Underscore,
    Var(Cow<'static, str>),
    Const(SimpleDatum),
    Pattern(CompiledPattern)
}

type PatternMatch<T> = Vec<MatchData<T>>;

#[derive(Debug, PartialEq)]
enum MatchData<T> {
    Var(Cow<'static, str>, Datum<T>),
    Repeated(Vars, Vec<PatternMatch<T>>)
}

impl CompiledPattern {
    fn compile<T>(literals: &Vars, datum: &Datum<T>)
            -> Result<CompiledPattern, MacroError>
        where T: Clone + PartialEq + Debug
    {
        let (form, tail) = datum.improper_list();
        if tail.is_some() {
            println!("form: {:?}, tail: {:?}", form, tail);
            return Err(MacroError {
                kind: MacroErrorKind::NotImplemented,
                desc: "Dotted list pattern is not implemented yet".to_string()
            });
        }

        let patts: Vec<&[Datum<T>]> = form.split(|s| s == &Datum::Sym(Cow::Borrowed("...")))
                                          .collect();
        match patts.as_slice() {
            &[] => Ok(CompiledPattern {
                    vars: HashSet::new(),
                    pattern: Pattern::List(Vec::new())
                }),
            &[_] => {
                Pattern::compile_subpatterns(literals, &form)
                        .map(|(vars, pats)| CompiledPattern {
                            vars: vars,
                            pattern: Pattern::List(pats)
                        })
            },
            &[prefix, suffix] =>
                match prefix.split_last() {
                    None => Err(MacroError {
                        kind: MacroErrorKind::NotImplemented,
                        desc: "Macro form (`...` ...) is not implemented yet".to_string()
                    }),
                    Some((rep, rest)) => {
                        let (vars0, pats0) = try!(Pattern::compile_subpatterns(literals, rest));
                        let mut vars = vars0;
                        let (vars1, repeating) = try!(SubPattern::compile(literals, rep));
                        try!(union_vars(&mut vars, vars1));
                        let (vars2, pats2) = try!(Pattern::compile_subpatterns(literals, suffix));
                        try!(union_vars(&mut vars, vars2));
                        Ok(CompiledPattern {
                            vars: vars,
                            pattern: Pattern::DelimitedList(pats0, Box::new(repeating), pats2)
                        })
                    }
                },
            _ =>
                Err(MacroError {
                    kind: MacroErrorKind::MultipleEllipses,
                    desc: "Multiple ellipses in a single list".to_string()
                })
        }
    }

    fn compute_match<T>(&self, datum: &Datum<T>) -> Result<PatternMatch<T>, ()>
        where T: Clone
    {
        if let &Datum::Cons(_) = datum {
            let list: Vec<Datum<T>> = try!(datum.iter().collect());
            match self.pattern {
                Pattern::List(ref pat) => {
                    if list.len() != pat.len() {
                        return Err(());
                    }
                    let mut matches = Vec::new();
                    for (sp, elem) in pat.iter().zip(list) {
                        let mut m = try!(sp.compute_match(&elem));
                        matches.append(&mut m);
                    }
                    Ok(matches)
                },
                Pattern::DelimitedList(ref prefix, ref repeat, ref suffix) => {
                    if list.len() < prefix.len() + suffix.len() {
                        return Err(());
                    }
                    let mut matches = Vec::new();

                    for (sp, elem) in prefix.iter().zip(list.iter()) {
                        let mut m = try!(sp.compute_match(&elem));
                        matches.append(&mut m);
                    }

                    let rep_start = prefix.len();
                    let rep_end = list.len() - suffix.len();

                    let mut repeats = Vec::new();
                    for elem in list[rep_start .. rep_end].iter() {
                        let m = try!(repeat.compute_match(elem));
                        repeats.push(m);
                    }
                    matches.push(MatchData::Repeated(repeat.vars(), repeats));

                    for (sp, elem) in suffix.iter().zip(list[rep_end ..].iter()) {
                        let mut m = try!(sp.compute_match(&elem));
                        matches.append(&mut m);
                    }

                    Ok(matches)
                }
            }
        } else {
            Err(())
        }
    }
}

fn union_vars(lhs: &mut Vars, rhs: Vars)
        -> Result<(), MacroError>
{
    for var in rhs.into_iter() {
        if let Some(replaced) = lhs.replace(var) {
            return Err(MacroError {
                kind: MacroErrorKind::DuplicateVars,
                desc: format!("Duplicated variable `{}` in pattern", replaced)
            });
        }
    }

    Ok(())
}

impl Pattern {
    fn compile_subpatterns<T>(literals: &Vars, data: &[Datum<T>])
            -> Result<(Vars, Vec<SubPattern>), MacroError>
        where T: Clone + PartialEq + Debug
    {
        let subpats: Result<Vec<(Vars, SubPattern)>, MacroError> =
                data.iter().map(|sp| SubPattern::compile(literals, sp)).collect();
        let mut var_set = HashSet::new();
        let mut pattern = Vec::new();
        for (subset, sp) in try!(subpats).into_iter() {
            try!(union_vars(&mut var_set, subset));
            pattern.push(sp);
        }

        Ok((var_set, pattern))
    }
}

impl SubPattern {
    fn compile<T>(literals: &Vars, datum: &Datum<T>)
            -> Result<(Vars, SubPattern), MacroError>
        where T: Clone + PartialEq + Debug
    {
        match datum {
            &Datum::Cons(_) | &Datum::Vector(_) =>
                CompiledPattern::compile(literals, datum)
                        .map(|pat| (pat.vars.clone(), SubPattern::Pattern(pat))),
            &Datum::Sym(ref sym) =>
                if sym == "_" {
                    Ok((HashSet::new(), SubPattern::Underscore))
                } else if literals.contains(sym) {
                    Ok((HashSet::new(), SubPattern::Const(SimpleDatum::Sym(sym.clone()))))
                } else {
                    Ok((hashset!{sym.clone()}, SubPattern::Var(sym.clone())))
                },
            _ =>
                if let Some(c) = SimpleDatum::from_datum(datum.clone()) {
                    Ok((HashSet::new(), SubPattern::Const(c)))
                } else {
                    Err(MacroError {
                        kind: MacroErrorKind::InvalidDatum,
                        desc: format!("Invalid datum in macro pattern")
                    })
                }
        }
    }

    fn compute_match<T>(&self, datum: &Datum<T>) -> Result<Vec<MatchData<T>>, ()>
        where T: Clone
    {
        match self {
            &SubPattern::Underscore => Ok(Vec::new()),
            &SubPattern::Var(ref sym) => Ok(vec![MatchData::Var(sym.clone(), datum.clone())]),
            &SubPattern::Const(ref c) => if c.equals(datum) {
                    Ok(Vec::new())
                } else {
                    Err(())
                },
            &SubPattern::Pattern(ref pat) => pat.compute_match(datum)
        }
    }

    fn vars(&self) -> Vars {
        match self {
            &SubPattern::Var(ref sym) => hashset!{ sym.clone() },
            &SubPattern::Pattern(ref pat) => pat.vars.clone(),
            _ => HashSet::new()
        }
    }
}

enum Template {
    Var(Cow<'static, str>),
    Const(SimpleDatum),
    List(Vec<TemplateElement>, Option<Box<Template>>),
    Vector(Vec<TemplateElement>)
}

enum TemplateElement {
    Template(Template),
    Repeat(Vars, Template, usize)
}

struct TemplateCompiler<'a> {
    global_vars: &'a Vars
}

impl<'a> TemplateCompiler<'a> {
    fn compile_list<T>(&self, data: &[Datum<T>])
            -> Result<(Vars, Vec<TemplateElement>), MacroError>
        where T: Clone
    {
        if data.is_empty() {
            return Ok((HashSet::new(), Vec::new()));
        }

        let mut res = Vec::new();
        let (mut last_vars, mut last_elem) = try!(self.compile(&data[0]));
        let mut vars = last_vars.clone();
        let mut repeat_count = 0;
        for elem in data[1 .. ].iter() {
            if let &Datum::Sym(Cow::Borrowed("...")) = elem {
                repeat_count += 1;
            } else {
                if repeat_count == 0 {
                    res.push(TemplateElement::Template(last_elem));
                } else {
                    res.push(TemplateElement::Repeat(last_vars, last_elem, repeat_count));
                }

                let (cur_vars, cur_elem) = try!(self.compile(elem));
                for v in cur_vars.iter() {
                    vars.insert(v.clone());
                }
                last_vars = cur_vars;
                last_elem = cur_elem;
                repeat_count = 0;
            }
        }

        Ok((vars, res))
    }

    fn compile<T>(&self, datum: &Datum<T>)
            -> Result<(Vars, Template), MacroError>
        where T: Clone
    {
        match datum {
            &Datum::Sym(ref sym) =>
                if self.global_vars.contains(sym) {
                    Ok((hashset!{ sym.clone() }, Template::Var(sym.clone())))
                } else {
                    Ok((HashSet::new(), Template::Const(SimpleDatum::Sym(sym.clone()))))
                },
            &Datum::Cons(_) => {
                let (list, tail) = datum.improper_list();
                let (mut vars, res) = try!(self.compile_list(&list));

                let tail_template = match tail {
                    Some(t) => {
                        let (tail_vars, tail_res) = try!(self.compile(&t));
                        for v in tail_vars.into_iter() {
                            vars.insert(v);
                        }
                        Some(Box::new(tail_res))
                    },
                    None => None
                };
                Ok((vars, Template::List(res, tail_template)))
            },
            &Datum::Vector(ref ptr) =>
                self.compile_list(ptr).map(|(vars, list)| (vars, Template::Vector(list))),
            _ =>
                if let Some(c) = SimpleDatum::from_datum(datum.clone()) {
                    Ok((HashSet::new(), Template::Const(c)))
                } else {
                    Err(MacroError {
                        kind: MacroErrorKind::InvalidDatum,
                        desc: format!("Invalid datum in macro template")
                    })
                }
        }
    }
}

#[cfg(test)]
mod test_matches {
    use std::borrow::Cow;
    use std::collections::HashSet;

    use super::{CompiledPattern, MatchData};
    use datum::Datum;
    use number::Number;
    use parser::Parser;

    macro_rules! assert_matches {
        ([$($lits:expr),*] $pattern:expr, $datum:expr => $result:expr) => (
            let literals = vec![$($lits),*].into_iter().map(Cow::Borrowed).collect();

            let mut pat_parser = Parser::new($pattern.as_bytes());
            let pattern = pat_parser.parse_full::<()>().expect("Failed to parse pattern");

            let mut datum_parser = Parser::new($datum.as_bytes());
            let datum = datum_parser.parse_full::<()>().expect("Failed to parse datum");

            let compiled_pattern = CompiledPattern::compile(&literals, &pattern)
                    .expect("Failed to compile pattern");

            println!("compiled_pattern: {:?}", compiled_pattern);

            assert_eq!(Ok($result), compiled_pattern.compute_match(&datum));
        )
    }

    #[test]
    fn test_simple_pattern() {
        let expected = vec![
                MatchData::Var(Cow::Borrowed("a"), num!(1)),
                MatchData::Var(Cow::Borrowed("b"), num!(2))
        ];

        assert_matches!([] "(a b)", "(1 2)" => expected);
    }

    #[test]
    fn test_nested_pattern() {
        let expected = vec![
            MatchData::Var(Cow::Borrowed("a"), num!(1)),
            MatchData::Var(Cow::Borrowed("b"), num!(2)),
            MatchData::Var(Cow::Borrowed("c"), list!(num!(3))),
        ];

        assert_matches!([] "(a (b) c)", "(1 (2) (3))" => expected);
    }

    #[test]
    fn test_match_literals() {
        let expected = vec![
            MatchData::Var(Cow::Borrowed("a"), num!(1)),
            MatchData::Var(Cow::Borrowed("c"), list!(num!(3))),
        ];

        assert_matches!(["b"] "(a (b) c)", "(1 (b) (3))" => expected);
    }

    #[test]
    fn test_match_underscore() {
        let expected = vec![
            MatchData::Var(Cow::Borrowed("a"), num!(1)),
            MatchData::Var(Cow::Borrowed("c"), list!(num!(3))),
        ];

        assert_matches!([] "(a _ c)", "(1 (2) (3))" => expected);
    }

    #[test]
    fn test_repeated_variable() {
        let vars = hashset! { Cow::Borrowed("b") };

        let expected = vec![
            MatchData::Var(Cow::Borrowed("a"), num!(1)),
            MatchData::Repeated(vars, vec![
                                vec![MatchData::Var(Cow::Borrowed("b"), num!(2))],
                                vec![MatchData::Var(Cow::Borrowed("b"), num!(3))],
                                vec![MatchData::Var(Cow::Borrowed("b"), num!(4))]
                                ]),
            MatchData::Var(Cow::Borrowed("c"), num!(5)),
        ];

        assert_matches!([] "(a b ... c)", "(1 2 3 4 5)" => expected);
    }

    #[test]
    fn test_repeated_pattern() {
        let vars = hashset! { Cow::Borrowed("b"), Cow::Borrowed("c") };

        let expected = vec![
            MatchData::Var(Cow::Borrowed("a"), num!(1)),
            MatchData::Repeated(vars, vec![
                                    vec![
                                        MatchData::Var(Cow::Borrowed("b"), num!(2)),
                                        MatchData::Var(Cow::Borrowed("c"), num!(3))
                                    ],
                                    vec![
                                        MatchData::Var(Cow::Borrowed("b"), num!(4)),
                                        MatchData::Var(Cow::Borrowed("c"), num!(5))
                                    ]
                                ]),
            MatchData::Var(Cow::Borrowed("d"), list!(num!(6), num!(7))),
        ];

        assert_matches!([] "(a (b c) ... d)", "(1 (2 3) (4 5) (6 7))" => expected);
    }
}
