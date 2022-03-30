use std::fmt;
use std::collections::{BTreeSet, BTreeMap};

use super::term::{Term};
use crate::sld::Env;

use crate::PrettyPrint;

#[derive(Debug, PartialEq)]
pub struct Clause {
    pub conds  : Vec<Term>,
    pub conseqt: Term,
}

impl Clause {
    pub(super) fn new(conds: Vec<Term>, conseqt: Term) -> Clause {
        Clause { conds, conseqt }
    }

    pub(super) fn all_vars(&self) -> BTreeSet<&str> {
        self.conds
            .iter()
            .chain(std::iter::once(&self.conseqt))
            .flat_map(|term| term.all_vars())
            .collect::<BTreeSet<_>>()
    }
    
    pub(super) fn rename_rule(&self, k: usize) -> (Self, usize) {
        
        let fvs = self.all_vars();
        let n = fvs.len();

        let sub: BTreeMap<String, Term> = fvs.into_iter()
            .enumerate()
            .map(|(i, fv)| (fv, format!("_{}", i + k)))
            .map(|(k, v)| (k.to_owned(), Term::var(v))  )
            .collect();
        let sub = Env(sub);

        let conds = self.conds.iter()
            .map(|term| term.subst(&sub))
            .collect::<Vec<_>>();

        let clause = Clause {
            conds: conds,
            conseqt: self.conseqt.subst(&sub),
        };
        (clause, k + n)
    }
    
}


impl fmt::Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for cond in &self.conds {
            f.write_str(&format!("    {}\n", cond))?;
        }
        f.write_str(&format!("=> {}", self.conseqt))
    }
}

impl PrettyPrint for [Clause]

{
    fn pretty_print(&self) -> String {
        self.iter()
            .map(|clause| clause.to_string())
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

impl PrettyPrint for Vec<Term> {
    fn pretty_print(&self) -> String {
        self.iter()
            .map(|t| format!("{}",t))
            .collect::<Vec<_>>()
            .join("  && ") + " =>"
            
    }
}

impl PrettyPrint for Vec<Vec<Term>> {
    fn pretty_print(&self) -> String {
        self.iter()
            .map(|t| format!("{}",t.pretty_print()))
            .collect::<Vec<_>>()
            .join("\n\n")
            
    }
} 