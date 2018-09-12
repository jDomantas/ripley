#![allow(unused)]

#[macro_use]
extern crate lazy_static;

mod error;
mod solver;
mod symbol;
mod terms;
mod syntax {
    pub mod lexer;
    pub mod parser;
}

use std::io::{self, BufRead};
use solver::Solver;
use symbol::Symbol;
use terms::Input;
use error::CompileError;

fn main() -> io::Result<()> {
    let mut rules = Vec::new();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line?;
        let tokens = match syntax::lexer::lex(&line) {
            Ok(tokens) => tokens,
            Err(e) => {
                e.report(&line);
                continue;
            }
        };
        let input = match syntax::parser::parse(&tokens) {
            Ok(input) => input,
            Err(e) => {
                e.report(&line);
                continue;
            }
        };
        match input {
            Input::Rule(rule) => rules.push(rule),
            Input::Query(predicate) => {
                let solver = Solver::new(&rules);
                let solutions = solver.solve(&predicate);
                if solutions.len() == 0 {
                    println!("no");
                } else {
                    for solution in solutions {
                        println!("Solution:");
                        if solution.len() == 0 {
                            println!("  yes");
                            continue;
                        }
                        for (var, term) in solution {
                            println!("  {} = {}", var, term);
                        }
                    }
                }
            }
        }
    }
    Ok(())
}
