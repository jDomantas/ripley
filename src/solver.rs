use std::collections::HashMap;
use terms::{Predicate, Rule, Term, Var, NamedPredicate, EqualityPredicate, ComparisonPredicate, Comparison};

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
struct InferVar(u32);
type InferTerm = Term<InferVar>;

use std::fmt;
impl fmt::Display for InferVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "?{}", self.0)
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
            (Term::Var(a), b) | (b, Term::Var(a)) => {
                self.unifier.insert(a, b);
                Ok(())
            }
            (Term::Number(a), Term::Number(b)) if a == b => Ok(()),
            (Term::Atom(a), Term::Atom(b)) if a == b => Ok(()),
            (_, _) => Err(UnifyError),
        }
    }

    fn unify_pred(&mut self, a: &Predicate<InferVar>, b: &Predicate<InferVar>) -> UnifyResult {
        if a.kind() != b.kind() {
            return Err(UnifyError);
        }
        a.args()
            .iter()
            .zip(b.args().iter())
            .map(|(a, b)| self.unify(*a, *b))
            .collect()
    }

    fn normalize(&mut self, term: InferTerm) -> InferTerm {
        let var = match term {
            Term::Var(var) => var,
            other => return other,
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
        match pred {
            Predicate::Named(pred) => {
                Predicate::Named(NamedPredicate {
                    name: pred.name,
                    args: pred.args.iter().map(|t| self.normalize(*t)).collect(),
                })
            }
            Predicate::Equality(pred) => {
                Predicate::Equality(EqualityPredicate {
                    args: [
                        self.normalize(pred.args[0]),
                        self.normalize(pred.args[1]),
                    ],
                    equal: pred.equal,
                })
            }
            Predicate::Comparison(pred) => {
                Predicate::Comparison(ComparisonPredicate {
                    args: [
                        self.normalize(pred.args[0]),
                        self.normalize(pred.args[1]),
                    ],
                    comparison: pred.comparison,
                })
            }
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
        if let Some(pred) = self.query_stack.iter().find(|p| p.equivalent(goal)) {
            self.trace(|| {
                println!("  pruning, same as {}", pred);
            });
            return Vec::new();
        }
        self.query_stack.push(goal.clone());
        let answers = match goal {
            goal @ Predicate::Named(_) => {
                let mut answers = Vec::new();
                for rule in self.rules {
                    let rule = self.instantiate_rule(rule);
                    answers.extend(self.goal_with_rule(&rule, &goal));
                }
                answers
            }
            Predicate::Equality(goal) => {
                self.solve_equality_goal(goal)
            }
            Predicate::Comparison(goal) => {
                self.solve_comparison_goal(goal)
            }
        };
        self.query_stack.pop();
        answers
    }

    fn solve_equality_goal(&mut self, goal: &EqualityPredicate<InferVar>) -> Solutions<InferVar> {
        let mut unifier = Unifier::default();
        if goal.equal {
            if unifier.unify(goal.args[0], goal.args[1]).is_ok() {
                vec![unifier.unifier]
            } else {
                Vec::new()
            }
        } else {
            match (goal.args[0], goal.args[1]) {
                (Term::Var(a), Term::Var(b)) if a == b => return Vec::new(),
                (Term::Var(_), _) | (_, Term::Var(_)) => panic!("infinite answers for {}", goal),
                (a, b) => {
                    if unifier.unify(a, b).is_ok() {
                        Vec::new()
                    } else {
                        vec![HashMap::new()]
                    }
                }
            }
        }
    }

    fn solve_comparison_goal(&mut self, goal: &ComparisonPredicate<InferVar>) -> Solutions<InferVar> {
        let (lhs, rhs, include_equal) = match goal.comparison {
            Comparison::Less => (goal.args[0], goal.args[1], false),
            Comparison::LessEqual => (goal.args[0], goal.args[1], true),
            Comparison::Greater => (goal.args[1], goal.args[0], false),
            Comparison::GreaterEqual => (goal.args[1], goal.args[0], true),
        };
        let cap = match rhs {
            Term::Atom(_) => return Vec::new(),
            Term::Number(num) => num,
            Term::Var(_) => panic!("infinite answers for {}", goal),
        };
        match lhs {
            Term::Atom(_) => Vec::new(),
            Term::Number(num) => {
                if num < cap || (num == cap && include_equal) {
                    vec![HashMap::new()]
                } else {
                    Vec::new()
                }
            }
            Term::Var(var) => {
                let solutions = if include_equal {
                    0..(cap + 1)
                } else {
                    0..cap
                };
                solutions
                    .map(|num| {
                        let mut sol = HashMap::new();
                        sol.insert(var, Term::Number(num));
                        sol
                    })
                    .collect()
            }
        }
    }

    fn goal_with_rule(
        &mut self,
        rule: &Rule<InferVar>,
        goal: &Predicate<InferVar>,
    ) -> Solutions<InferVar> {
        if rule.head.kind() != goal.kind() {
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
        Term::Number(num) => Term::Number(num),
    }
}

fn instantiate_predicate(
    c: &mut SolveCtx,
    pred: &Predicate<Var>,
    existing: &mut HashMap<Var, InferVar>,
) -> Predicate<InferVar> {
    match pred {
        Predicate::Named(pred) => {
            let args = pred
                .args
                .iter()
                .map(|t| instantiate_term(c, t, existing))
                .collect();
            Predicate::Named(NamedPredicate {
                name: pred.name,
                args,
            })
        }
        Predicate::Equality(pred) => {
            let args = [
                instantiate_term(c, &pred.args[0], existing),
                instantiate_term(c, &pred.args[1], existing),
            ];
            Predicate::Equality(EqualityPredicate { args, equal: pred.equal })
        }
        Predicate::Comparison(pred) => {
            let args = [
                instantiate_term(c, &pred.args[0], existing),
                instantiate_term(c, &pred.args[1], existing),
            ];
            Predicate::Comparison(ComparisonPredicate {
                args,
                comparison: pred.comparison,
            })
        }
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
            for (&original, &instantiated) in goal.args().iter().zip(inst_goal.args().iter()) {
                let original = match original {
                    Term::Var(var) => var,
                    Term::Number(_) |
                    Term::Atom(_) => continue,
                };
                let var_name = original.to_string();
                if var_name == "_" {
                    continue;
                }
                let term = unifier.normalize(instantiated);
                match term {
                    Term::Number(num) => {
                        sol.insert(original, Term::Number(num));
                    }
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
