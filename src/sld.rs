use std::collections::BTreeMap;

use crate::clause::Clause;
use crate::term::Term;
use crate::PrettyPrint;

pub enum Soln<'b> {
    CP(CPoint<'b>, CPoint<'b>),
    Res(Env),
    NoClauses
}

#[derive(Clone)]
pub enum Strategy {
    BFS,
    DFS,
    DLS(usize),
}

#[derive(Clone)]
pub struct CPoints<'b> {
    cpoints: Vec<CPoint<'b>>,
    strategy: Strategy
}

impl<'b> Iterator for CPoints<'b> {
    type Item=Env;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.cpoints.pop().map( |cp: CPoint<'_>| cp.solve_goal()) {
                Some(Soln::Res(env)) => {
                    return Some(env)
                },
                Some(Soln::CP(cp0, cp1)) => match self.strategy {
                    Strategy::DFS => self.cpoints.extend([cp1,cp0]),
                    Strategy::BFS => self.cpoints.extend([cp0,cp1]),
                    Strategy::DLS(n) => {
                        if cp0.depth <= n {
                            self.cpoints.extend([cp1,cp0])
                        } else {
                            self.cpoints.extend([cp1])
                        }
                    },
                },
                Some(Soln::NoClauses) => continue,
                None => return None
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct CPoint<'b> {
    pub goals: Vec<Term>,
    pub clauses: &'b [Clause],
    pub env : Env,
    pub depth: usize,
    pub k: usize
}

impl<'b> CPoint<'b> {

    pub fn new(clauses: &[Clause], goals: Vec<Term>) -> CPoint<'_> {
        CPoint {
            clauses,
            goals,
            env: Env(BTreeMap::new()),
            depth: 0,
            k: 0
        }
    }

    pub fn backchain(self, strategy: Strategy) -> CPoints<'b> where Self: 'b {
        let root = CPoints {
            cpoints: vec![self],
            strategy
        };
        root
    }

    //TODO CHECK WHETHER IT TERMINATES, MAY ALSO REPEAT SOLUTIONS
    fn backchain_iddfs(self) -> impl Iterator<Item=Env> + 'b where Self: 'b {
        (0..).into_iter()
            .flat_map(move |n| {
                let cpoints = CPoints {
                    cpoints: vec![self.clone()],
                    strategy: Strategy::DLS(n)
                };
                cpoints
            })
    }

    
    fn solve_goal<'c>(self) -> Soln<'c> where Self: 'c {
        match self.goals.split_first() {

            None => Soln::Res(self.env.clone()),
            Some((goal, rest_goals)) => {
                let clauses = self.clauses;
                for i in 0..clauses.len() {
                    let rule = &clauses[i];
                    let renamed = rule.rename_rule(self.k);
                    let mut new_env = Env(BTreeMap::new());

                    let goal = goal.subst(&self.env);

                    //println!("{}", goal);
                    //println!("{}", renamed.0);
                    //println!("{}", self.env.pretty_print());
                    //println!("\n\n");

                    let (Clause {conds, conseqt }, k1) = renamed;

                    match conseqt.unify_baader(&goal, &mut new_env) {
                        Ok(()) => {
                            let mut ret_goals = conds;
                            ret_goals.extend(rest_goals.into_iter().cloned());
            
                            let rest_rules = &clauses[i+1..clauses.len()];

                            let ret_env = new_env.compose(&self.env.clone());

                            let cp0 = CPoint {
                                goals: ret_goals,
                                clauses: clauses,
                                env: ret_env,
                                depth: self.depth+1,
                                k: k1
                            };
                            let cp1 = CPoint {
                                goals: self.goals.clone(),
                                clauses: rest_rules,
                                env: self.env,
                                depth: self.depth,
                                k : self.k
                            };

                            //println!("cp0: {:?}", cp0);
                            //println!("\n\n");
                            //println!("cp1: {:?}", cp1);


                            return Soln::CP(cp0, cp1)
                            
                        },
                        Err(_) => continue
                    }
                }
                Soln::NoClauses
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Env(pub BTreeMap<String, Term>);

impl Env {

    pub fn empty() -> Env {
        Env(BTreeMap::new())
    }
    // self after other
    pub fn compose(self, other: &Env) -> Env {
        let mut ret_env: BTreeMap<_,_> = other.0.iter()
            //Substitute in rhs
            .map(|(x,t)| (x.clone(), t.subst(&self)))
            // remove identity substitutions created
            .filter(|(x, t)| &Term::Var(x.to_string()) != t)
            .collect();

        // remove substitions whose lhs clashes with ret_env
        for (k, v) in self.0.into_iter() {
            ret_env.entry(k).or_insert(v);
        }

        Env(ret_env)
    }
}

impl PrettyPrint for Env {
    fn pretty_print(&self) -> String {
        self.0.iter()
            .map(|(k,v)| format!("`{} -> {}", k, v))
            .collect::<Vec<_>>()
            .join("\n")
    }
}