extern crate ripley;

use std::collections::HashMap;
use ripley::Solver;
use ripley::terms::Input;

fn main() {
    with_rules(r#"
        edge(a, b).
        edge(A, B) :- edge(B, A).
    "#, |solver| {
        check(&solver, "? edge(a, X)", "X = b");
        check(&solver, "? edge(b, X)", "X = a");
    });
    with_rules(r#"
        edge(a, b).
        edge(A, B) :- edge(B, A).
        path(A, B) :- edge(A, B).
        path(A, B) :- edge(A, C), path(C, B).
    "#, |solver| {
        check(&solver, "? path(a, X)", "X = a; X = b");
        check(&solver, "? path(b, X)", "X = a; X = b");
    })
}

fn with_rules(source: &str, f: impl FnOnce(Solver)) {
    let mut rules = Vec::new();
    for line in source.lines() {
        let line = line.trim();
        if line.len() == 0 {
            continue;
        }
        match ripley::syntax::parse(line) {
            Ok(Input::Rule(rule)) => rules.push(rule),
            Ok(Input::Query(_)) => panic!("unexpected query"),
            Err(e) => {
                e.report(line);
                panic!("parse error");
            }
        }
    }
    let solver = Solver::new(&rules);
    f(solver);
}

fn check(solver: &Solver, query: &str, solution: &str) {
    let query = match ripley::syntax::parse(query) {
        Ok(Input::Rule(_)) => panic!("unexpected rule"),
        Ok(Input::Query(query)) => query,
        Err(e) => {
            e.report(query);
            panic!("parse error");
        },
    };
    let solutions = solver.solve(&query);
    let answer = format_solutions(&solutions);
    let is_ok = if solutions.len() == 0 {
        solution == "false"
    } else {
        solution == answer
    };
    if is_ok {
        println!("Test passed: {}", query);
    } else {
        println!("Test failed: {}", query);
        println!("   Expected: {}", solution);
        println!("   Got:      {}", answer);
    }
    println!();
}

fn format_solution(solution: &HashMap<ripley::terms::Var, ripley::terms::Term<ripley::terms::Var>>) -> String {
    let mut parts = solution
        .iter()
        .map(|(v, t)| format!("{} = {}", v, t))
        .collect::<Vec<_>>();
    parts.sort();
    let mut ans = String::new();
    for part in &parts {
        if ans.len() > 0 {
            ans.push_str(", ");
        }
        ans.push_str(part);
    }
    ans
}

fn format_solutions(solutions: &[HashMap<ripley::terms::Var, ripley::terms::Term<ripley::terms::Var>>]) -> String {
    let solutions = solutions
        .iter()
        .map(format_solution)
        .collect::<Vec<_>>();
    let mut ans = String::new();
    for sol in &solutions {
        if ans.len() > 0 {
            ans.push_str(", ");
        }
        ans.push_str(sol);
    }
    ans
}
