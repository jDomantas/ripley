use std::collections::HashMap;
use terms::{Predicate, Rule, Term, Var};

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
struct InferVar(u32);
type InferTerm = Term<InferVar>;

use std::fmt;
impl fmt::Display for InferVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

struct UnifyError;
type UnifyResult = Result<(), UnifyError>;

#[derive(Default)]
struct VarSource {
    next_var: u32,
}

impl VarSource {
    fn fresh_var(&mut self) -> InferVar {
        let var = InferVar(self.next_var);
        self.next_var += 1;
        var
    }

    fn generator<'a>(&'a mut self) -> impl FnOnce() -> InferVar + 'a {
        move || self.fresh_var()
    }
}

#[derive(Default, Clone)]
struct Unifier {
    unifier: HashMap<InferVar, InferTerm>,
}

impl Unifier {
    fn unify(&mut self, a: InferTerm, b: InferTerm) -> UnifyResult {
        let a = self.normalize(a);
        let b = self.normalize(b);
        match (a, b) {
            (Term::Var(a), Term::Var(b)) => {
                if a != b {
                    self.unifier.insert(a, Term::Var(b));
                }
                Ok(())
            }
            (Term::Var(a), b @ Term::Atom(_)) | (b @ Term::Atom(_), Term::Var(a)) => {
                self.unifier.insert(a, b);
                Ok(())
            }
            (Term::Atom(a), Term::Atom(b)) if a == b => Ok(()),
            (Term::Atom(_), Term::Atom(_)) => Err(UnifyError),
        }
    }

    fn unify_pred(&mut self, a: &Predicate<InferVar>, b: &Predicate<InferVar>) -> UnifyResult {
        if a.name != b.name || a.args.len() != b.args.len() {
            return Err(UnifyError);
        }
        a.args
            .iter()
            .zip(b.args.iter())
            .map(|(a, b)| self.unify(*a, *b))
            .collect()
    }

    fn normalize(&mut self, term: InferTerm) -> InferTerm {
        let var = match term {
            Term::Var(var) => var,
            atom @ Term::Atom(_) => return atom,
        };
        match self.unifier.get(&var) {
            Some(&Term::Var(next)) => {
                let result = self.normalize(Term::Var(next));
                self.unifier.insert(var, result);
                result
            }
            Some(&term) => term,
            None => Term::Var(var),
        }
    }

    fn normalize_pred(&mut self, pred: &Predicate<InferVar>) -> Predicate<InferVar> {
        Predicate {
            name: pred.name,
            args: pred.args.iter().map(|t| self.normalize(*t)).collect(),
        }
    }
}

type Solutions<V> = Vec<HashMap<V, Term<V>>>;

struct SolveCtx<'a> {
    rules: &'a [Rule<Var>],
    var_source: VarSource,
    query_stack: Vec<Predicate<InferVar>>,
    trace: bool,
}

impl<'a> SolveCtx<'a> {
    fn new(rules: &'a [Rule<Var>], trace: bool) -> SolveCtx<'a> {
        SolveCtx {
            rules,
            var_source: Default::default(),
            query_stack: Default::default(),
            trace,
        }
    }

    fn trace(&self, f: impl FnOnce()) {
        if self.trace {
            for _ in 0..self.query_stack.len() {
                print!("  ");
            }
            f();
        }
    }

    fn solve_goal(&mut self, goal: &Predicate<InferVar>) -> Solutions<InferVar> {
        self.trace(|| {
            println!("{}", goal);
        });
        if let Some(pred) = self.query_stack.iter().find(|p| p.alpha_equivalent(goal)) {
            self.trace(|| {
                println!("  pruning, same as {}", pred);
            });
            return Vec::new();
        }
        self.query_stack.push(goal.clone());
        let mut answers = Vec::new();
        for rule in self.rules {
            let rule = self.instantiate_rule(rule);
            answers.extend(self.goal_with_rule(&rule, &goal));
        }
        self.query_stack.pop();
        answers
    }

    fn goal_with_rule(
        &mut self,
        rule: &Rule<InferVar>,
        goal: &Predicate<InferVar>,
    ) -> Solutions<InferVar> {
        if rule.head.name != goal.name || rule.head.args.len() != goal.args.len() {
            return Vec::new();
        }
        self.trace(|| {
            println!("- {}", rule);
        });
        let mut unifier = Unifier::default();
        if unifier.unify_pred(&rule.head, &goal).is_err() {
            return Vec::new();
        }
        self.solve_tail(&rule.tail, unifier)
    }

    fn solve_tail(
        &mut self,
        tail: &[Predicate<InferVar>],
        mut unifier: Unifier,
    ) -> Solutions<InferVar> {
        if tail.len() == 0 {
            return vec![unifier.unifier];
        }
        let mut answers = Vec::new();
        let (first, rest) = (&tail[0], &tail[1..]);
        let head_solutions = self.solve_goal(&unifier.normalize_pred(first));
        for solution in head_solutions {
            let mut unifier = unifier.clone();
            for (&var, &term) in &solution {
                if unifier.unify(Term::Var(var), term).is_err() {
                    continue;
                }
            }
            for solution in self.solve_tail(rest, unifier.clone()) {
                let mut unifier = unifier.clone();
                for (&var, &term) in &solution {
                    if unifier.unify(Term::Var(var), term).is_err() {
                        continue;
                    }
                }
                answers.push(unifier.unifier);
            }
        }
        answers
    }

    fn instantiate_rule(&mut self, rule: &Rule<Var>) -> Rule<InferVar> {
        let existing = &mut HashMap::new();
        let head = instantiate_predicate(self, &rule.head, existing);
        let tail = rule
            .tail
            .iter()
            .map(|p| instantiate_predicate(self, p, existing))
            .collect();
        Rule { head, tail }
    }
}

fn instantiate_term(
    c: &mut SolveCtx,
    term: &Term<Var>,
    existing: &mut HashMap<Var, InferVar>,
) -> InferTerm {
    match *term {
        Term::Var(var) => Term::Var(*existing.entry(var).or_insert_with(c.var_source.generator())),
        Term::Atom(atom) => Term::Atom(atom),
    }
}

fn instantiate_predicate(
    c: &mut SolveCtx,
    pred: &Predicate<Var>,
    existing: &mut HashMap<Var, InferVar>,
) -> Predicate<InferVar> {
    let args = pred
        .args
        .iter()
        .map(|t| instantiate_term(c, t, existing))
        .collect();
    Predicate {
        name: pred.name,
        args,
    }
}

pub struct Solver<'a> {
    rules: &'a [Rule<Var>],
}

impl<'a> Solver<'a> {
    pub fn new(rules: &'a [Rule<Var>]) -> Solver<'a> {
        Solver { rules }
    }

    pub fn solve(&self, goal: &Predicate<Var>, trace: bool) -> Solutions<Var> {
        let mut ctx = SolveCtx::new(self.rules, trace);
        let inst_goal = instantiate_predicate(&mut ctx, goal, &mut Default::default());
        let mut solutions = Vec::new();
        for solution in ctx.solve_goal(&inst_goal) {
            let mut back_ref = HashMap::new();
            let mut unifier = Unifier { unifier: solution };
            let mut sol = HashMap::new();
            for (&original, &instantiated) in goal.args.iter().zip(inst_goal.args.iter()) {
                let original = match original {
                    Term::Atom(_) => continue,
                    Term::Var(var) => var,
                };
                let term = unifier.normalize(instantiated);
                match term {
                    Term::Atom(atom) => {
                        sol.insert(original, Term::Atom(atom));
                    }
                    Term::Var(var) => {
                        match back_ref.get(&var) {
                            Some(&v) => { sol.insert(original, Term::Var(v)); }
                            None => { back_ref.insert(var, original); }
                        }
                    }
                }
            }
            solutions.push(sol);
        }
        solutions
    }
}
