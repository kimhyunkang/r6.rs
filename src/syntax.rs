use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Debug;

use datum::{cons, Datum, SimpleDatum};
use error::{MacroError, MacroErrorKind};

pub type Vars = HashSet<Cow<'static, str>>;

macro_rules! hashset {
    ($($e:expr),*) => ({
        let mut s = HashSet::new();
        $( s.insert($e); )*
        s
    })
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompiledMacro {
    patterns: Vec<MacroPattern>
}

#[derive(Debug, Clone, PartialEq)]
struct MacroPattern {
    pattern: CompiledPattern,
    template: Template
}

impl CompiledMacro {
    pub fn compile<T>(literals: &Vars, rules: &[(Datum<T>, Datum<T>)])
            -> Result<CompiledMacro, MacroError>
        where T: Clone + Debug
    {
        let res: Result<Vec<MacroPattern>, MacroError> = rules.iter()
            .map(|&(ref pattern, ref template)|
                MacroPattern::compile(literals, &pattern, &template)
            )
            .collect();

        res.map(|pat| CompiledMacro { patterns: pat })
    }

    pub fn transform<T>(&self, datum: &Datum<T>) -> Result<Datum<T>, MacroError>
        where T: Clone
    {
        for &MacroPattern { ref pattern, ref template } in self.patterns.iter() {
            if let Some(m) = pattern.compute_match(datum) {
                return Ok(template.transform(&m));
            }
        }

        Err(MacroError {
            kind: MacroErrorKind::MatchNotFound,
            desc: "Matching pattern not found".to_string()
        })
    }
}

impl MacroPattern {
    fn compile<T>(literals: &Vars, pattern_src: &Datum<T>, template_src: &Datum<T>)
            -> Result<MacroPattern, MacroError>
        where T: Clone + Debug
    {
        let cp = CompiledPattern::compile(literals, pattern_src)?;

        let template = {
            let compiler = TemplateCompiler::new(&cp.vars);
            let (_, template) = compiler.compile(template_src)?;

            template.validate(&cp.pattern)?;

            template
        };

        Ok(MacroPattern {
            pattern: cp,
            template: template
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
struct CompiledPattern {
    vars: Vars,
    pattern: Pattern
}

#[derive(Debug, Clone, PartialEq)]
enum Pattern {
    List(Vec<SubPattern>),
    DelimitedList(Vec<SubPattern>, Box<SubPattern>, Vec<SubPattern>)
}

#[derive(Debug, Clone, PartialEq)]
enum SubPattern {
    Underscore,
    Var(Cow<'static, str>),
    Const(SimpleDatum),
    Pattern(CompiledPattern)
}

#[derive(Debug, PartialEq)]
struct PatternMatch<T> {
    matches: Vec<MatchData<T>>
}

#[derive(Debug, PartialEq)]
enum MatchData<T> {
    Var(Cow<'static, str>, Datum<T>),
    Repeated(Vars, Vec<PatternMatch<T>>)
}

impl CompiledPattern {
    fn compile<T>(literals: &Vars, datum: &Datum<T>)
            -> Result<CompiledPattern, MacroError>
        where T: Clone
    {
        let (form, tail) = datum.improper_list();
        if tail.is_some() {
            return Err(MacroError {
                kind: MacroErrorKind::NotImplemented,
                desc: "Dotted list pattern is not implemented yet".to_string()
            });
        }

        let patts: Vec<&[Datum<T>]> = form.split(|s|
            if let &Datum::Sym(ref sym) = s {
                sym == "..."
            } else {
                false
            }
        ).collect();

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
                        let (vars0, pats0) = Pattern::compile_subpatterns(literals, rest)?;
                        let mut vars = vars0;
                        let (vars1, repeating) = SubPattern::compile(literals, rep)?;
                        union_vars(&mut vars, vars1)?;
                        let (vars2, pats2) = Pattern::compile_subpatterns(literals, suffix)?;
                        union_vars(&mut vars, vars2)?;
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

    fn compute_match<T>(&self, datum: &Datum<T>) -> Option<PatternMatch<T>>
        where T: Clone
    {
        if let &Datum::Cons(_) = datum {
            let list: Vec<Datum<T>> = match datum.iter().collect() {
                Ok(l) => l,
                Err(_) => return None
            };
            match self.pattern {
                Pattern::List(ref pat) => {
                    if list.len() != pat.len() {
                        return None;
                    }
                    let mut matches = Vec::new();
                    for (sp, elem) in pat.iter().zip(list) {
                        let mut m = match sp.compute_match(&elem) {
                            Some(m) => m,
                            None => return None
                        };
                        matches.append(&mut m);
                    }
                    Some(PatternMatch::new(matches))
                },
                Pattern::DelimitedList(ref prefix, ref repeat, ref suffix) => {
                    if list.len() < prefix.len() + suffix.len() {
                        return None;
                    }
                    let mut matches = Vec::new();

                    for (sp, elem) in prefix.iter().zip(list.iter()) {
                        let mut m = match sp.compute_match(&elem) {
                            Some(m) => m,
                            None => return None
                        };
                        matches.append(&mut m);
                    }

                    let rep_start = prefix.len();
                    let rep_end = list.len() - suffix.len();

                    let mut repeats = Vec::new();
                    for elem in list[rep_start .. rep_end].iter() {
                        let m = match repeat.compute_match(elem) {
                            Some(m) => PatternMatch::new(m),
                            None => return None
                        };
                        repeats.push(m);
                    }
                    let rep = MatchData::Repeated(repeat.vars(), repeats);
                    matches.push(rep);

                    for (sp, elem) in suffix.iter().zip(list[rep_end ..].iter()) {
                        let mut m = match sp.compute_match(&elem) {
                            Some(m) => m,
                            None => return None
                        };
                        matches.append(&mut m);
                    }

                    Some(PatternMatch::new(matches))
                }
            }
        } else {
            None
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
        where T: Clone
    {
        let subpats: Result<Vec<(Vars, SubPattern)>, MacroError> =
                data.iter().map(|sp| SubPattern::compile(literals, sp)).collect();
        let mut var_set = HashSet::new();
        let mut pattern = Vec::new();
        for (subset, sp) in subpats?.into_iter() {
            union_vars(&mut var_set, subset)?;
            pattern.push(sp);
        }

        Ok((var_set, pattern))
    }

    fn contains_var(&self, var: &str) -> bool {
        match self {
            &Pattern::List(ref list) => {
                for sp in list.iter() {
                    if sp.contains_var(var) {
                        return true;
                    }
                }

                false
            },
            &Pattern::DelimitedList(ref prefix, _, ref suffix) => {
                for sp in prefix.iter() {
                    if sp.contains_var(var) {
                        return true;
                    }
                }

                for sp in suffix.iter() {
                    if sp.contains_var(var) {
                        return true;
                    }
                }

                false
            }
        }
    }
}

impl SubPattern {
    fn compile<T>(literals: &Vars, datum: &Datum<T>)
            -> Result<(Vars, SubPattern), MacroError>
        where T: Clone
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

    fn compute_match<T>(&self, datum: &Datum<T>) -> Option<Vec<MatchData<T>>>
        where T: Clone
    {
        match self {
            &SubPattern::Underscore => Some(Vec::new()),
            &SubPattern::Var(ref sym) => Some(vec![MatchData::Var(sym.clone(), datum.clone())]),
            &SubPattern::Const(ref c) => if c.equals(datum) {
                    Some(Vec::new())
                } else {
                    None
                },
            &SubPattern::Pattern(ref pat) => pat.compute_match(datum).map(|m| m.matches)
        }
    }

    fn vars(&self) -> Vars {
        match self {
            &SubPattern::Var(ref sym) => hashset!{ sym.clone() },
            &SubPattern::Pattern(ref pat) => pat.vars.clone(),
            _ => HashSet::new()
        }
    }

    fn contains_var(&self, var: &str) -> bool {
        match self {
            &SubPattern::Var(ref sym) => sym == var,
            &SubPattern::Pattern(ref cp) => cp.pattern.contains_var(var),
            _ => false
        }
    }
}

impl<T> PatternMatch<T> {
    fn new(matches: Vec<MatchData<T>>) -> PatternMatch<T> {
        PatternMatch { matches: matches }
    }

    fn find_var(&self, var: &str) -> Option<&Datum<T>>
    {
        for m in self.matches.iter() {
            if let &MatchData::Var(ref v, ref val) = m {
                if v == var {
                    return Some(val);
                }
            }
        }

        None
    }

    fn find_var_repeat(&self, vars: &Vars) -> Option<&[PatternMatch<T>]>
    {
        for m in self.matches.iter() {
            if let &MatchData::Repeated(ref vs, ref matches) = m {
                if vars.is_subset(vs) {
                    return Some(matches);
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Template {
    Var(Cow<'static, str>),
    Const(SimpleDatum),
    List(Vec<TemplateElement>, Option<Box<Template>>),
    Vector(Vec<TemplateElement>)
}

#[derive(Debug, Clone, PartialEq)]
enum TemplateElement {
    Template(Template),
    Repeat(Vars, Box<TemplateElement>)
}

struct TemplateCompiler<'a> {
    global_vars: &'a Vars
}

impl Template {
    fn validate(&self, pattern: &Pattern) -> Result<(), MacroError>
    {
        match self {
            &Template::Const(_) => Ok(()),
            &Template::Var(ref var) => if pattern.contains_var(var) {
                Ok(())
            } else {
                Err(MacroError {
                    kind: MacroErrorKind::BadRepeatingTemplate,
                    desc: format!("Template var `{}` not found in pattern", var)
                })
            },
            &Template::List(ref init, ref tail) => {
                for elem in init.iter() {
                    elem.validate(pattern)?;
                }
                if let &Some(ref elem) = tail {
                    elem.validate(pattern)?;
                }
                Ok(())
            },
            &Template::Vector(ref v) => {
                for elem in v.iter() {
                    elem.validate(pattern)?;
                }
                Ok(())
            }
        }
    }

    fn transform<T>(&self, data: &PatternMatch<T>) -> Datum<T>
        where T: Clone
    {
        match self {
            &Template::Const(ref c) =>
                c.clone().to_datum(),
            &Template::Var(ref sym) => {
                if let Some(val) = data.find_var(sym) {
                    val.clone()
                } else {
                    panic!("Unknown variable `{}`", sym)
                }
            },
            &Template::List(ref list, ref tail) => {
                let mut res = match tail {
                    &Some(ref t) => t.transform(data),
                    &None => Datum::Nil
                };

                for elem in list.iter().rev() {
                    match elem {
                        &TemplateElement::Template(ref t) => {
                            res = cons(t.transform(data), res);
                        },
                        &TemplateElement::Repeat(ref vars, ref t) => {
                            let matches = data.find_var_repeat(vars).expect("Unknown pattern");
                            for m in matches.iter().rev() {
                                res = cons(t.transform_repeat(m), res);
                            }
                        }
                    }
                }

                res
            },
            &Template::Vector(ref vec) => {
                let mut res = Datum::Nil;

                for elem in vec.iter().rev() {
                    match elem {
                        &TemplateElement::Template(ref t) => {
                            res = cons(t.transform(data), res);
                        },
                        &TemplateElement::Repeat(ref vars, ref t) => {
                            let matches = data.find_var_repeat(vars).expect("Unknown pattern");
                            for m in matches.iter().rev() {
                                res = cons(t.transform_repeat(m), res);
                            }
                        }
                    }
                }

                res
            }
        }
    }
}

impl TemplateElement {
    fn validate(&self, pattern: &Pattern) -> Result<(), MacroError>
    {
        match self {
            &TemplateElement::Template(ref template) =>
                template.validate(pattern),
            &TemplateElement::Repeat(ref vars, ref template) =>
                template.validate_repeat(vars, pattern)
        }
    }

    fn validate_repeat(&self, vars: &Vars, pattern: &Pattern) -> Result<(), MacroError>
    {
        match pattern {
            &Pattern::List(ref vec) => {
                for elem in vec.iter() {
                    if let &SubPattern::Pattern(ref cp) = elem {
                        if cp.vars.is_superset(vars) {
                            self.validate_repeat(vars, &cp.pattern)?;
                        }
                    }
                }

                Ok(())
            },
            &Pattern::DelimitedList(ref prefix, ref repeat, ref suffix) => {
                for elem in prefix.iter() {
                    if let &SubPattern::Pattern(ref cp) = elem {
                        if cp.vars.is_superset(vars) {
                            self.validate_repeat(vars, &cp.pattern)?;
                        }
                    }
                }

                match repeat.as_ref() {
                    &SubPattern::Var(ref sym) =>
                        if vars.len() == 1 && vars.contains(sym) {
                            if let &TemplateElement::Template(Template::Var(ref v)) = self {
                                if v == sym {
                                    return Ok(());
                                } else {
                                    panic!("Invalid template");
                                }
                            } else {
                                return Err(MacroError {
                                    kind: MacroErrorKind::BadRepeatingTemplate,
                                    desc: format!("Invalid template variable level")
                                })
                            }
                        },
                    &SubPattern::Pattern(ref cp) =>
                        if cp.vars.is_superset(vars) {
                            self.validate(&cp.pattern)?;
                        },
                    _ =>
                        ()
                }

                for elem in suffix.iter() {
                    if let &SubPattern::Pattern(ref cp) = elem {
                        if cp.vars.is_superset(vars) {
                            self.validate_repeat(vars, &cp.pattern)?;
                        }
                    }
                }

                Ok(())
            }
        }
    }

    fn transform_repeat<T>(&self, data: &PatternMatch<T>) -> Datum<T>
        where T: Clone
    {
        match self {
            &TemplateElement::Template(ref tmpl) =>
                tmpl.transform(data),
            &TemplateElement::Repeat(ref vars, ref tmpl) => {
                let matches = data.find_var_repeat(vars).expect("Template pattern not found");
                matches.iter().map(|m| tmpl.transform_repeat(m)).collect()
            }
        }
    }
}

impl<'a> TemplateCompiler<'a> {
    fn new(vars: &'a HashSet<Cow<'static, str>>) -> TemplateCompiler<'a> {
        TemplateCompiler {
            global_vars: vars
        }
    }

    fn compile_list<T>(&self, data: &[Datum<T>])
            -> Result<(Vars, Vec<TemplateElement>), MacroError>
        where T: Clone
    {
        if data.is_empty() {
            return Ok((HashSet::new(), Vec::new()));
        }

        let mut res = Vec::new();
        let (mut last_vars, elem) = self.compile(&data[0])?;
        let mut vars = last_vars.clone();
        let mut last_elem = TemplateElement::Template(elem);
        for elem in data[1 .. ].iter() {
            if let &Datum::Sym(Cow::Borrowed("...")) = elem {
                last_elem = TemplateElement::Repeat(last_vars.clone(), Box::new(last_elem));
            } else {
                res.push(last_elem);

                let (cur_vars, cur_elem) = self.compile(elem)?;
                for v in cur_vars.iter() {
                    vars.insert(v.clone());
                }
                last_vars = cur_vars;
                last_elem = TemplateElement::Template(cur_elem);
            }
        }

        res.push(last_elem);

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
                let (mut vars, res) = self.compile_list(&list)?;

                let tail_template = match tail {
                    Some(t) => {
                        let (tail_vars, tail_res) = self.compile(&t)?;
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

    use super::{CompiledPattern, MatchData, PatternMatch};
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

            if let Some(PatternMatch { matches: result }) = compiled_pattern.compute_match(&datum) {
                assert_eq!($result, result);
            } else {
                panic!("compute_match returned None");
            }
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
                PatternMatch::new(vec![MatchData::Var(Cow::Borrowed("b"), num!(2))]),
                PatternMatch::new(vec![MatchData::Var(Cow::Borrowed("b"), num!(3))]),
                PatternMatch::new(vec![MatchData::Var(Cow::Borrowed("b"), num!(4))])
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
                                    PatternMatch::new(vec![
                                        MatchData::Var(Cow::Borrowed("b"), num!(2)),
                                        MatchData::Var(Cow::Borrowed("c"), num!(3))
                                    ]),
                                    PatternMatch::new(vec![
                                        MatchData::Var(Cow::Borrowed("b"), num!(4)),
                                        MatchData::Var(Cow::Borrowed("c"), num!(5))
                                    ])
                                ]),
            MatchData::Var(Cow::Borrowed("d"), list!(num!(6), num!(7))),
        ];

        assert_matches!([] "(a (b c) ... d)", "(1 (2 3) (4 5) (6 7))" => expected);
    }
}

#[cfg(test)]
mod test_template {
    use std::borrow::Cow;
    use std::collections::HashSet;

    use datum::SimpleDatum;
    use parser::Parser;
    use super::{Template, TemplateCompiler, TemplateElement};

    macro_rules! assert_compiles {
        ([$($lits:expr),*] $template:expr => $result:expr) => (
            let literals = vec![$($lits),*].into_iter().map(Cow::Borrowed).collect();

            let mut parser = Parser::new($template.as_bytes());
            let template = parser.parse_full::<()>().expect("Failed to parse template");

            let compiler = TemplateCompiler::new(&literals);
            let (_, compiled_template) = compiler.compile(&template)
                    .expect("Failed to compile pattern");

            assert_eq!($result, compiled_template);
        )
    }

    #[test]
    fn test_template_compilation() {
        assert_compiles!(["a", "b", "c"] "(a b c)" =>
            Template::List(
                vec![
                    TemplateElement::Template(Template::Var(Cow::Borrowed("a"))),
                    TemplateElement::Template(Template::Var(Cow::Borrowed("b"))),
                    TemplateElement::Template(Template::Var(Cow::Borrowed("c")))
                ],
                None
            )
        );
    }

    #[test]
    fn test_free_variable() {
        assert_compiles!(["a", "b"] "(a b c)" =>
            Template::List(
                vec![
                    TemplateElement::Template(Template::Var(Cow::Borrowed("a"))),
                    TemplateElement::Template(Template::Var(Cow::Borrowed("b"))),
                    TemplateElement::Template(Template::Const(SimpleDatum::Sym(Cow::Borrowed("c"))))
                ],
                None
            )
        );
    }

    #[test]
    fn test_repeating_pattern() {
        assert_compiles!(["a", "b", "c"] "(a b ... c)" =>
            Template::List(
                vec![
                    TemplateElement::Template(Template::Var(Cow::Borrowed("a"))),
                    TemplateElement::Repeat(
                        hashset!{Cow::Borrowed("b")},
                        Box::new(TemplateElement::Template(Template::Var(Cow::Borrowed("b"))))
                    ),
                    TemplateElement::Template(Template::Var(Cow::Borrowed("c")))
                ],
                None
            )
        );
    }

    #[test]
    fn test_nested_repeating_pattern() {
        assert_compiles!(["a", "b", "c", "d"] "(a (b c ...) ... d)" =>
            Template::List(
                vec![
                    TemplateElement::Template(Template::Var(Cow::Borrowed("a"))),
                    TemplateElement::Repeat(
                        hashset!{Cow::Borrowed("b"), Cow::Borrowed("c")},
                        Box::new(TemplateElement::Template(Template::List(vec![
                            TemplateElement::Template(Template::Var(Cow::Borrowed("b"))),
                            TemplateElement::Repeat(
                                hashset!{Cow::Borrowed("c")},
                                Box::new(TemplateElement::Template(Template::Var(Cow::Borrowed("c"))))
                            )
                        ], None)))
                    ),
                    TemplateElement::Template(Template::Var(Cow::Borrowed("d")))
                ],
                None
            )
        );
    }
}

#[cfg(test)]
mod test_syntax_transform {
    use std::borrow::Cow;

    use parser::Parser;

    use super::CompiledMacro;

    macro_rules! assert_transforms {
        ([$($lits:expr),*] [$($pat:expr => $tmpl:expr),+] , $datum:expr => $result:expr) => (
            let literals = vec![$($lits),*].into_iter().map(Cow::Borrowed).collect();
            let mut rules = Vec::new();
            $(
                {
                    let mut parser = Parser::new($pat.as_bytes());
                    let pat = parser.parse_full::<()>().expect("Failed to parse pattern");

                    parser = Parser::new($tmpl.as_bytes());
                    let tmpl = parser.parse_full::<()>().expect("Failed to parse template");

                    rules.push((pat, tmpl));
                }
            )+

            let syntax = CompiledMacro::compile(&literals, &rules).expect("Failed to compile");

            let mut parser = Parser::new($datum.as_bytes());
            let datum = parser.parse_full::<()>().expect("Failed to parse datum");

            parser = Parser::new($result.as_bytes());
            let result = parser.parse_full::<()>().expect("Failed to parse result");

            assert_eq!(Ok(result), syntax.transform(&datum));
        )
    }

    #[test]
    fn test_transform() {
        assert_transforms!([] ["(id)" => "(define id 0)"], "(x)" => "(define x 0)");
    }

    #[test]
    fn test_repeat() {
        assert_transforms!(
            []
            ["(test stmt1 stmt2 ...)" => "(if test (begin stmt1 stmt2 ...))"],
            "(x (set! x 0) (set! x 1) (set! x 2))"
            =>
            "(if x (begin (set! x 0) (set! x 1) (set! x 2)))"
        );
    }

    #[test]
    fn test_nested_repeat() {
        assert_transforms!(
            []
            [
                "(() body1 body2 ...)" => "(let () body1 body2 ...)",
                "(((name1 val1) (name2 val2) ...) body1 body2 ...)" => "(let ((name1 val1)) (let* ((name2 val2) ...) body1 body2 ...))"
            ],
            "(((x 0) (y 1)) (+ x y) (* x y))"
            =>
            "(let ((x 0)) (let* ((y 1)) (+ x y) (* x y)))"
        );
    }
}
