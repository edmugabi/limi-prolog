use std::fmt;

use std::collections::{HashSet, BTreeMap, BTreeSet};

use crate::{sld::Env};
use crate::PrettyPrint;

#[derive(Debug, PartialEq)]
pub enum UnifError {
    Cyclic,
    NoMatch
}

pub enum EqnOp {
    Eq,
    Arrow
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Const(String),
    Var(String),
    List(Vec<Term>)
}

impl Term {
    pub fn cnst<S: Into<String>>(s: S) -> Term {
        Term::Const(s.into())
    }

    pub fn var<S: Into<String>>(s: S) -> Term {
        Term::Var(s.into())
    }

    pub fn free_variables(&self) -> HashSet<&str> {

        let mut vars = HashSet::new();
        match self {
            Term::Const(_)   => (),
            Term::Var(v)     => {vars.insert(v.as_str());},
            Term::List(list) => {
                for t in list {
                    vars.extend(t.free_variables());
                }
            }
        }
        vars
    }

    pub(super) fn all_vars(&self) -> HashSet<&str> {
        self.free_variables()
    }

    
    pub(super) fn subst_mut<'a>(&mut self, sub: &Env)
    {
        match self {
            Term::Var(v)  => match sub.0.get(v.as_str()) {
                Some(t) => *self = t.clone(),
                None    => ()
            }
            Term::Const(_) => (),
            Term::List(list) => {
                list.iter_mut()
                    .for_each(|t| t.subst_mut(sub));
            }
        }
    }
    

    pub(super) fn subst(&self, sub: &Env) -> Term {

        match self {
            Term::Var(v)  => match sub.0.get(v.as_str()) {
                //TODO Recursion not normally done here
                Some(t) => t.clone().subst(sub),
                None    => self.clone()
            }
            Term::Const(_) => self.clone(),
            Term::List(list) => {
                let list = list.iter()
                    .map(|t| t.subst(sub))
                    .collect();
                Term::List(list)
            }
        }
    }

    fn occurs(&self, var: &String) -> bool {
        match self {
            Term::Var(x) => {
                if x == var {
                    return true
                }
                else { return false }
            }
            Term::Const(_) => false,
            Term::List(list) => {
                list.into_iter()
                    .any(|t| t.occurs(var))
            }
        }
    }

    pub(super) fn unify_baader(&self, other: &Term, env: &mut Env)
    -> Result<(), UnifError>
    {
        let mut fst = self.clone();
        let mut snd = other.clone();
        let mut eqs = vec![(&mut fst, &mut snd)];
        
        solve(&mut eqs, env)
    }

    pub fn equation(&self) -> Option<(&Term, EqnOp, &Term)> {
        match self {
            Term::Const(_)
            | Term::Var(_)   => None,
            Term::List(list) => {
                if let [t0, Term::Const(c), t1] = list.as_slice() {
                    if c == "=" {
                        return Some((t0, EqnOp::Eq, t1))
                    }
                    else if c == "->"  {
                        return Some((t0, EqnOp::Arrow, t1))
                    }
                    else { None }
                }
                else { None }
            }
        }
    }
    
}

/*
Ok(true) - harmless cycle
Ok(false) - no cycle
Err(BCError::Cyclic) - is a cycle
*/
enum Cyclic {
    Trivial,
    True,
    False
}



fn solve<'a>(eqs:  &mut Vec<(&'a mut Term, &'a mut Term)>, env: &mut Env)
-> Result<(), UnifError>
{
    //println!("env: {:?}", env);
    use Term::*;
    match eqs.pop() {
        None => Ok(()),
        Some((Var(x), t)) => {
            if &Var(x.to_owned()) == t {
                solve(eqs, env)
            } else {
                if t.occurs(x) {
                    Err(UnifError::Cyclic)
                } else {
                    env.0.insert(x.to_owned(), t.clone());
                    eliminate(x.clone(), t.clone(), eqs, env)
                    //solve(eqs, env, type_env)
                }
            }
        }
        Some((t, v@Var(_))) => {
            let pair = (v,t);
            eqs.push(pair);
            solve(eqs, env)
        }
        Some((List(args1), List(args2))) => {
            let inner_eqs = args1.into_iter()
                .zip(args2);
            eqs.extend(inner_eqs);
            solve(eqs, env)
        }
        Some((Const(c1), Const(c2))) => {
            if c1 == c2 {
                solve(eqs, env)
            } else {
                Err(UnifError::NoMatch)
            }
        }
        Some((List(_), Const(_)))
        | Some((Const(_), List(_))) => {
            Err(UnifError::NoMatch)
        }
    }
}

fn eliminate<'a>(
    x: String,
    t: Term,
    eqs: &mut Vec<(&'a mut Term, &'a mut Term)>,
    env: &mut Env)
-> Result<(), UnifError>
{

    if t.occurs(&x) {
        return Err(UnifError::Cyclic)
    }
    let new_env = BTreeMap::from([(x, t)]);

    eqs.into_iter()
        .for_each(|(fst, snd)| {
            fst.subst_mut(&env);
            snd.subst_mut(&env);
        });

    env.0.extend(new_env);
    solve(eqs, env)
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Var(v) => write!(f, "{}", v),
            Term::Const(c) => write!(f, "{}", c),
            Term::List(list) => {

                let s = list.into_iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                    //.collect::<String>();
                    
                write!(f, "({})", s)
            }
        }
    }
}

impl PrettyPrint for Term {
    fn pretty_print(&self) -> String {
        match self {
            Term::Var(v) => format!("`{}", v),
            Term::Const(c) => format!("{}", c),
            Term::List(list) => {

                let s = list.into_iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(" , ");
                    //.collect::<String>();
                    
                format!("[{}]", s)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::pterm;

    #[test]
    fn unify_test1() {
        let t1: Term = Term::List(vec![
            Term::List(vec![
                Term::cnst("fact"),
                Term::List(vec![
                    Term::cnst("("),
                    Term::var("x"),
                    Term::cnst(")")
                ])
            ]),
            Term::cnst("->"),
            Term::List(vec![
                Term::var("x"),
                Term::cnst("*"),
                Term::var("y")
            ])
        ]);

        let t2: Term = Term::List(vec![
            Term::List(vec![
                Term::cnst("fact"),
                Term::List(vec![
                    Term::cnst("("),
                    Term::List(vec![
                        Term::var("`_0"),
                        Term::cnst("-"),
                        Term::cnst("1")
                    ]),
                    Term::cnst(")")
                ])
            ]),
            Term::cnst("->"),
            Term::var("z")
        ]);

        let mut env = Env(BTreeMap::new());
        let result = t1.unify_baader(&t2, &mut env);

        println!("{}", env.pretty_print());
    }

    #[test]
    fn unify1() {
        let (_,t0) = pterm("f(X,g(X,X))").unwrap();
        let (_,t1) = pterm("f(h(Y),g(Z,h(a)))").unwrap();
        println!("{:?}", t0);

        let (_,ha) = pterm("h(a)").unwrap();
        let expected_env = BTreeMap::from([
            ("X".into(), ha.clone()),
            ("Y".into(), Term::cnst("a")),
            ("Z".into(), ha)
        ]);

        let mut res_env = Env(BTreeMap::new());
        let res = t0.unify_baader(&t1, &mut res_env);
        assert_eq!(res, Ok(()));
        assert_eq!(res_env, Env(expected_env));

    }
}