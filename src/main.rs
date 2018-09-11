#![allow(unused)]

#[macro_use]
extern crate lazy_static;

mod solver;
mod symbol;
mod terms;

use solver::Solver;
use symbol::Symbol;
use terms::{Atom, Term, Predicate, Rule, Var};

fn main() {
    let pred = Symbol::new("pred");
    let a = Symbol::new("a");
    let b = Symbol::new("b");
    let c = Symbol::new("c");
    let rules: Vec<Rule<Var>> = vec![
        Rule {
            head: Predicate {
                name: pred,
                args: vec![
                    Term::Atom(Atom { symbol: a }),
                    Term::Atom(Atom { symbol: b }),
                ],
            },
            tail: vec![],
        },
    ];
    let solver = solver::Solver::new(&rules);
    let query = Predicate {
        name: pred,
        args: vec![
            Term::Atom(Atom { symbol: a }),
            Term::Atom(Atom { symbol: c }),
        ],
    };
    for solution in solver.solve(&query) {
        println!("Got solution");
    }
    println!("Done");
}
