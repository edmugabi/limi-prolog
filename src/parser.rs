use crate::IResult;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, multispace0};
use nom::character::complete::{alphanumeric0, alphanumeric1, satisfy, space0};
use nom::combinator::{all_consuming, map, recognize, value};
use nom::multi::{many0, many_m_n, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair};

use crate::clause::Clause;
use crate::term::Term;

fn pvariable(input: &str) -> IResult<&str, &str> {
    recognize(pair(satisfy(char::is_uppercase), alphanumeric0))(input)
}

fn pconst(input: &str) -> IResult<&str, &str> {
    let pred = |ch| char::is_lowercase(ch) || char::is_digit(ch, 10) || ch == '=';
    recognize(pair(satisfy(pred), alphanumeric0))(input)
}

fn pargs(input: &str) -> IResult<&str, Vec<Term>> {
    let mut pargs = separated_list1(delimited(space0, tag(","), space0), pterm);
    pargs(input)
}

pub fn pterm(input: &str) -> IResult<&str, Term> {
    let plbrace = delimited(space0, tag("("), space0);
    let prbrace = preceded(space0, tag(")"));
    let pargs = delimited(plbrace, pargs, prbrace);

    let pexpr = map(pair(pconst, pargs), |(head, args)| {
        Term::List(vec![Term::cnst(head), Term::List(args)])
    });

    let pconst = map(pconst, |s| Term::cnst(s));

    let pvariable = map(pvariable, |s| Term::var(s));

    alt((pexpr, pvariable, pconst))(input)
}

fn pdefinite_clause(input: &str) -> IResult<&str, Clause> {
    let pcond = preceded(tag("    "), pterm);
    let pline_sep = |i| pair(space0, tag("\n"))(i);
    let pconds = separated_list0(pline_sep, pcond);
    let pconseqt = preceded(pair(tag("=>"), space0), pterm);

    let pclause = map(
        separated_pair(pconds, pline_sep, pconseqt),
        |(conds, conseqt)| Clause { conds, conseqt },
    );

    let punit_clause = map(pterm, |conseqt| Clause {
        conds: vec![],
        conseqt,
    });
    alt((pclause, punit_clause))(input)
}

pub fn pprogram(input: &str) -> IResult<&str, Vec<Clause>> {
    let prog_sep = |i| many_m_n(1, 2, pair(space0, tag("\n")))(i);
    let pinner = delimited(
        prog_sep,
        separated_list0(prog_sep, pdefinite_clause),
        multispace0,
    );
    all_consuming(pinner)(input)
}

pub fn pgoals(input: &str) -> IResult<&str, Vec<Term>> {
    let pterm_list = separated_list0(tag("&&"), delimited(space0, pterm, space0));
    all_consuming(pterm_list)(input)
}

#[derive(Debug, PartialEq)]
pub enum UserInput {
    FileName(String),
    Goal(Vec<Term>),
    Disj,
    NoParse,
}

pub fn puser_input(input: &str) -> UserInput {
    let pfile_name = map(delimited(tag("["), alpha1, tag("]")), |s: &str| {
        UserInput::FileName(s.into())
    });

    let pdisj = map(delimited(space0, tag("||"), space0), |_| UserInput::Disj);

    let p = alt((pfile_name, map(pgoals, UserInput::Goal), pdisj));

    match all_consuming(p)(input) {
        Ok((_, user_input)) => user_input,
        Err(_) => UserInput::NoParse,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pterm_test1() {
        let (rest, t) = pterm("f(g(X), Y)").unwrap();
        assert_eq!(rest, "");
        use Term::*;
        let expected = List(vec![
            Const("f".into()),
            List(vec![
                List(vec![Const("g".into()), List(vec![Var("X".into())])]),
                Var("Y".into()),
            ]),
        ]);
        assert_eq!(t, expected);
    }

    #[test]
    fn pconds_test() {
        let pcond = preceded(tag("    "), pterm);
        let pline_sep = |i| pair(space0, tag("\n"))(i);
        let mut pconds = separated_list0(pline_sep, pcond);
        let input = 
"    f(X)
    g(Y,a)
    h(f(X,Y),Y)";
        let (rest, list) = pconds(input).unwrap();
        assert_eq!(rest, "");
        println!("{:?}", list);
    }

    #[test]
    fn pdefinite_clause_test() {
        let input =
"    mother(X,Y)
    mother(Y,Z)
=> grandmother(X,Z)";

        let (rest, clause) = pdefinite_clause(input).unwrap();
        assert_eq!(rest, "");
        println!("clause: {:?}", clause);
    }

    #[test]
    fn pdefinite_clause_test1() {
        let (rest, fact) = pdefinite_clause("mother(alice, sarah)").unwrap();
        assert_eq!(rest, "");
        use Term::*;
        let expected = Clause {
            conds: vec![],
            conseqt: List(vec![
                Const("mother".into()),
                List(vec![Const("alice".into()),Const("sarah".into())])
            ]) };
        assert_eq!(expected, fact);
    }

    #[test]
    fn puser_input_test() {
        let input = "[filename]";
        let mut pfile_name = map(delimited(tag("["), alpha1, tag("]")), |s: &str| {
            UserInput::FileName(s.into())
        });
        let expected: IResult<_, _> = Ok(("", UserInput::FileName("filename".into())));
        assert_eq!(pfile_name(input), expected);
    }
}
