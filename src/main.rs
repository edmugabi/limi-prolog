mod parser;
mod term;
mod clause;
mod sld;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io;

use crate::parser::{pprogram, pgoals};
use crate::parser::UserInput;
use parser::puser_input;

use crate::sld::{CPoint, Strategy};

use nom::error::VerboseError;
type IResult<I,O> = nom::IResult<I,O,VerboseError<I>>;

pub trait PrettyPrint {
    fn pretty_print(&self) -> String;
}

fn main() -> std::io::Result<()>{
    let stdin  = io::stdin();
    let stdout = io::stdout();

    loop {
        {
            let mut handle = stdout.lock();
            handle.write_all(b":- ")?;
            handle.flush()?;
        }

        let mut user_input = String::new();
        stdin.read_line(&mut user_input)?;


        let mut clauses = vec![];

        assert_eq!(&user_input.split_off(user_input.len()-1), "\n"); // remove the \n

        match puser_input(&user_input) {
            UserInput::FileName(name) => {
                let mut file = File::open(name)?;

                let mut contents = String::new();
                file.read_to_string(&mut contents)?;

                match pprogram(&contents) {
                    Ok((_, cls)) => {
                        clauses = cls;
                        println!("Read new clauses into program");
                    },
                    Err(_)       => {
                        println!("Error parsing program")
                    }
                }
            }
            UserInput::Goal(goal) => {
                // treat it as a query
                let cp = CPoint::new(&clauses, goal);
                let mut env_iter = cp.backchain(Strategy::DFS);
                match env_iter.next() {
                    Some(env) => println!("{}", env.pretty_print()),
                    None => {
                        println!("No Solution");
                    }
                };
            }
            UserInput::Disj => { continue },
            UserInput::NoParse => {
                println!("Please enter correct input");
                
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_sld() {
        let clauses = "
    mother(X,Y)
    mother(Y,Z)
=> grandmother(X,Z)

mother(alice, stella)
mother(stella, gift)
        ";

        let (_,clauses) = pprogram(clauses).unwrap();

        let (_, goal) = pgoals("grandmother(alice, gift)").unwrap();
        let cp = CPoint::new(&clauses, goal);
        let mut env_iter = cp.backchain(Strategy::DFS);

        let first_soln = env_iter.next();
        //println!("{:?}", first_soln);
        assert!(first_soln.is_some());

        assert_eq!(env_iter.next(), None);
    }

    #[test]
    fn sld_test2() {
        let (_,program) = pprogram("
plus(0, Y, Y)

    plus(X,Y,Z)
=> plus(s(X), Y, s(Z))
        ").unwrap();

        println!("{}", program.pretty_print());

        let (_,goals) = pgoals("plus(s(0),Y,s(s(s(0))))").unwrap();
        println!("{}", goals[0].pretty_print());

        let cp = CPoint::new(&program, goals);
        let mut env_iter = cp.backchain(Strategy::BFS);
        println!("{}", env_iter.next().unwrap().pretty_print());
    }
}