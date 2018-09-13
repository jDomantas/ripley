extern crate ripley;

use std::io::{self, BufRead};
use ripley::terms::Input;
use ripley::Solver;

fn main() -> io::Result<()> {
    let mut rules = Vec::new();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line?;
        let input = match ripley::syntax::parse(&line) {
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
