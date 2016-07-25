use std::borrow::Cow;
use std::collections::HashSet;

use datum::{Datum, SimpleDatum};
use error::{MacroError, MacroErrorKind};

struct CompiledPattern {
    vars: HashSet<Cow<'static, str>>,
    pattern: Pattern
}

enum Pattern {
    List(Vec<SubPattern>),
    DelimitedList(Vec<SubPattern>, Box<CompiledPattern>, Vec<SubPattern>)
}

enum SubPattern {
    Underscore,
    Var(Cow<'static, str>),
    Const(SimpleDatum),
    Pattern(CompiledPattern)
}

#[derive(Debug, PartialEq)]
struct PatternMatch<T> {
    vars: HashSet<Cow<'static, str>>,
    matches: Vec<MatchData<T>>
}

#[derive(Debug, PartialEq)]
enum MatchData<T> {
    Var(Cow<'static, str>, Datum<T>),
    Repeats(HashSet<Cow<'static, str>>, Vec<PatternMatch<T>>)
}

impl CompiledPattern {
    fn compile<T>(literals: &HashSet<Cow<'static, str>>, datum: &Datum<T>)
            -> Result<CompiledPattern, MacroError>
        where T: Clone + PartialEq
    {
        let (form, tail) = datum.improper_list();
        if tail.is_some() {
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
            &[chunk] => {
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
                        let repeating = try!(CompiledPattern::compile(literals, rep));
                        try!(union_vars(&mut vars, repeating.vars.clone()));
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
                    Ok(PatternMatch {
                        vars: self.vars.clone(),
                        matches: matches
                    })
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

                    let repeats = Vec::new();
                    for elem in list[rep_start .. rep_end].iter() {
                        let m = try!(repeat.compute_match(elem));
                        repeats.push(m);
                    }
                    matches.push(MatchData::Repeats(PatternMatch {
                        vars: repeat.vars.clone(),
                        matches: repeats
                    }));

                    for (sp, elem) in suffix.iter().zip(list[rep_end ..].iter()) {
                        let mut m = try!(sp.compute_match(&elem));
                        matches.append(&mut m);
                    }

                    Ok(PatternMatch {
                        vars: self.vars.clone(),
                        matches: matches
                    })
                }
            }
        } else {
            Err(())
        }
    }
}

fn union_vars(lhs: &mut HashSet<Cow<'static, str>>, rhs: HashSet<Cow<'static, str>>)
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
    fn compile_subpatterns<T>(literals: &HashSet<Cow<'static, str>>, data: &[Datum<T>])
            -> Result<(HashSet<Cow<'static, str>>, Vec<SubPattern>), MacroError>
        where T: Clone + PartialEq
    {
        let subpats: Result<Vec<(HashSet<Cow<'static, str>>, SubPattern)>, MacroError> =
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
    fn compile<T>(literals: &HashSet<Cow<'static, str>>, datum: &Datum<T>)
            -> Result<(HashSet<Cow<'static, str>>, SubPattern), MacroError>
        where T: Clone + PartialEq
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
                    Ok((HashSet::new(), SubPattern::Var(sym.clone())))
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

    fn vars(&self) -> Option<HashSet<Cow<'static, str>>> {
        match self {
            &SubPattern::Var(ref v) => {
                let mut set = HashSet::new();
                set.insert(v.clone());
                Some(set)
            },
            &SubPattern::Pattern(ref pat) => Some(pat.vars.clone()),
            _ => None
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
            &SubPattern::Pattern(ref pat) => pat.compute_match(datum).map(|v| v.matches)
        }
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;
    use std::collections::HashSet;
    use std::iter::FromIterator;

    use super::{CompiledPattern, PatternMatch, MatchData};
    use datum::Datum;
    use number::Number;
    use parser::Parser;

    macro_rules! assert_matches {
        ($pattern:expr, $datum:expr => $result:expr) => (
            let mut pat_parser = Parser::new($pattern.as_bytes());
            let pattern = pat_parser.parse_full::<()>().expect("Failed to parse pattern");

            let mut datum_parser = Parser::new($datum.as_bytes());
            let datum = datum_parser.parse_full::<()>().expect("Failed to parse datum");

            let compiled_pattern = CompiledPattern::compile(&HashSet::new(), &pattern)
                    .expect("Failed to compile pattern");

            assert_eq!(Ok($result), compiled_pattern.compute_match(&datum));
        )
    }

    #[test]
    fn test_simple_pattern() {
        let expected = PatternMatch {
            vars: HashSet::from_iter(vec![Cow::Borrowed("a"), Cow::Borrowed("b")]),
            matches: vec![MatchData::Var(Cow::Borrowed("a"), num!(1)), MatchData::Var(Cow::Borrowed("b"), num!(2))]
        };

        assert_matches!("(a b)", "(1 2)" => expected);
    }
}
