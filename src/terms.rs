use std::fmt;
use std::hash::Hash;
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
}

impl<V: fmt::Display> fmt::Display for Term<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::Var(var) => write!(f, "?{}", var),
            Term::Atom(atom) => write!(f, "{}", atom),
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
pub struct Predicate<V> {
    pub name: Symbol,
    pub args: Vec<Term<V>>,
}

impl<V: Eq + Hash + Copy> Predicate<V> {
    pub fn alpha_equivalent(&self, other: &Predicate<V>) -> bool {
        if self.name != other.name || self.args.len() != other.args.len() {
            return false;
        }
        self.args.iter().zip(other.args.iter()).all(|(a, b)| a == b)
    }
}

impl<V: fmt::Display> fmt::Display for Predicate<V> {
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
