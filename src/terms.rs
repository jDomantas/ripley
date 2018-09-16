use std::fmt;
use symbol::Symbol;

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub struct Atom {
    pub symbol: Symbol,
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.symbol)
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Term<V> {
    Var(V),
    Atom(Atom),
    Number(u64),
}

impl<V: fmt::Display> fmt::Display for Term<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::Var(var) => write!(f, "{}", var),
            Term::Atom(atom) => write!(f, "{}", atom),
            Term::Number(num) => write!(f, "{}", num),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub struct Var {
    pub symbol: Symbol,
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.symbol)
    }
}

#[derive(Debug, Clone)]
pub enum Predicate<V> {
    Named(NamedPredicate<V>),
    Equality(EqualityPredicate<V>),
    Comparison(ComparisonPredicate<V>),
}

impl<V> Predicate<V> {
    pub fn args(&self) -> &[Term<V>] {
        match self {
            Predicate::Named(pred) => &pred.args,
            Predicate::Equality(pred) => &pred.args,
            Predicate::Comparison(pred) => &pred.args,
        }
    }

    pub fn kind(&self) -> PredicateKind {
        match self {
            Predicate::Named(pred) => PredicateKind::Named(pred.name, pred.args.len()),
            Predicate::Equality(pred) => PredicateKind::Equality(pred.equal),
            Predicate::Comparison(pred) => PredicateKind::Comparison(pred.comparison),
        }
    }
}

impl<V: Eq> Predicate<V> {
    pub fn equivalent(&self, other: &Predicate<V>) -> bool {
        match (self, other) {
            (Predicate::Named(a), Predicate::Named(b)) => a.equivalent(b),
            (Predicate::Equality(a), Predicate::Equality(b)) => a.args == b.args,
            (Predicate::Comparison(a), Predicate::Comparison(b)) => a.args == b.args && a.comparison == b.comparison,
            (_, _) => false,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum PredicateKind {
    Named(Symbol, usize),
    Equality(bool),
    Comparison(Comparison),
}

impl fmt::Display for PredicateKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PredicateKind::Named(name, args) => write!(f, "{}/{}", name, args),
            PredicateKind::Equality(true) => write!(f, "=/2"),
            PredicateKind::Equality(false) => write!(f, "\\=/2"),
            PredicateKind::Comparison(Comparison::Less) => write!(f, "</2"),
            PredicateKind::Comparison(Comparison::LessEqual) => write!(f, "<=/2"),
            PredicateKind::Comparison(Comparison::Greater) => write!(f, ">/2"),
            PredicateKind::Comparison(Comparison::GreaterEqual) => write!(f, "=</2"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EqualityPredicate<V> {
    pub args: [Term<V>; 2],
    pub equal: bool,
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Comparison {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone)]
pub struct ComparisonPredicate<V> {
    pub args: [Term<V>; 2],
    pub comparison: Comparison,
}

#[derive(Debug, Clone)]
pub struct NamedPredicate<V> {
    pub name: Symbol,
    pub args: Vec<Term<V>>,
}

impl<V: Eq> NamedPredicate<V> {
    pub fn equivalent(&self, other: &NamedPredicate<V>) -> bool {
        if self.name != other.name || self.args.len() != other.args.len() {
            return false;
        }
        self.args.iter().zip(other.args.iter()).all(|(a, b)| a == b)
    }
}

impl<V: fmt::Display> fmt::Display for EqualityPredicate<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.args[0], if self.equal { "=" } else { "\\=" }, self.args[1])
    }
}

impl<V: fmt::Display> fmt::Display for ComparisonPredicate<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let operator = match self.comparison {
            Comparison::Less => "<",
            Comparison::LessEqual => "=<",
            Comparison::Greater => ">",
            Comparison::GreaterEqual => ">=",
        };
        write!(f, "{} {} {}", self.args[0], operator, self.args[1])
    }
}

impl<V: fmt::Display> fmt::Display for NamedPredicate<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if self.args.len() > 0 {
            write!(f, "(")?;
            let mut comma = false;
            for arg in &self.args {
                if comma {
                    write!(f, ", ")?;
                }
                comma = true;
                write!(f, "{}", arg)?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl<V: fmt::Display> fmt::Display for Predicate<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Predicate::Named(p) => write!(f, "{}", p),
            Predicate::Equality(p) => write!(f, "{}", p),
            Predicate::Comparison(p) => write!(f, "{}", p),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Rule<V> {
    pub head: Predicate<V>,
    pub tail: Vec<Predicate<V>>,
}

impl<V: fmt::Display> fmt::Display for Rule<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.head)?;
        if self.tail.len() > 0 {
            write!(f, " :- ")?;
            let mut comma = false;
            for arg in &self.tail {
                if comma {
                    write!(f, ", ")?;
                }
                comma = true;
                write!(f, "{}", arg)?;
            }
        }
        write!(f, ".")
    }
}

#[derive(Debug, Clone)]
pub enum Input {
    Rule(Rule<Var>),
    Query(Predicate<Var>),
}

impl fmt::Display for Input {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Input::Rule(rule) => write!(f, "{}", rule),
            Input::Query(pred) => write!(f, "? {}", pred),
        }
    }
}
