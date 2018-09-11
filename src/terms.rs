use symbol::Symbol;

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub struct Atom {
    pub symbol: Symbol,
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Term<V> {
    Var(V),
    Atom(Atom),
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub struct Var {
    pub symbol: Symbol,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Predicate<V> {
    pub name: Symbol,
    pub args: Vec<Term<V>>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Rule<V> {
    pub head: Predicate<V>,
    pub tail: Vec<Predicate<V>>,
}
