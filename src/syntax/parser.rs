use std::collections::HashMap;
use std::io::{self, Write};
use error::CompileError;
use syntax::lexer::{Token, TokenKind};
use terms::{Var, Atom, Term, Predicate, NamedPredicate, EqualityPredicate, ComparisonPredicate, Comparison, Rule, Input};
use symbol::Symbol;

#[derive(Debug, Copy, Clone)]
pub enum ParseErrorKind {
    ExpectedSymbol,
    ExpectedLeftParenOrOp,
    ExpectedTermOrParen,
    ExpectedCommaOrParen,
    ExpectedDotOrImplies,
    ExpectedCommaOrDot,
    ExpectedEnd,
    ExpectedOp,
}

impl ParseErrorKind {
    fn description(&self) -> &'static str {
        use self::ParseErrorKind::*;
        match self {
            ExpectedSymbol => "expected name",
            ExpectedLeftParenOrOp => "expected '(' or operator",
            ExpectedTermOrParen => "expected term or ')'",
            ExpectedCommaOrParen => "expected ',' or ')'",
            ExpectedDotOrImplies => "expected '.' or ':-'",
            ExpectedCommaOrDot => "expected ',' or '.'",
            ExpectedEnd => "unexpected token",
            ExpectedOp => "expected operator"
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub position: usize,
}

impl CompileError for ParseError {
    fn report(&self, source: &str) {
        let stdout = io::stdout();
        let mut stdout = stdout.lock();
        let _ = writeln!(stdout, "error: {}", self.kind.description());
        let _ = writeln!(stdout, "at column {}", self.position + 1);
        let _ = writeln!(stdout, " {}", source);
        for _ in 0..=self.position {
            let _ = write!(stdout, " ");
        }
        let _ = writeln!(stdout, "^");
    }
}

type PResult<T> = Result<T, ParseError>;

struct Parser<'a> {
    tokens: &'a [Token<'a>],
    end_pos: usize,
    vars: HashMap<&'a str, Var>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token<'a>]) -> Parser<'a> {
        let end_pos = match tokens.last() {
            Some(tok) => tok.end + 1,
            None => 0,
        };
        Parser {
            tokens,
            end_pos,
            vars: HashMap::new(),
        }
    }

    fn peek(&self) -> Option<TokenKind<'a>> {
        self.tokens.get(0).map(|t| t.kind)
    }

    fn advance(&mut self) {
        self.tokens = self.tokens.get(1..).unwrap_or(&[]);
    }

    fn check(&mut self, tok: TokenKind) -> bool {
        match self.peek() {
            Some(t) if t == tok => {
                self.advance();
                true
            }
            _ => {
                false
            }
        }
    }

    fn error(&self, kind: ParseErrorKind) -> ParseError {
        let position = match self.tokens.get(0) {
            Some(tok) => tok.start,
            None => self.end_pos,
        };
        ParseError { position, kind }
    }

    fn expect(&mut self, kind: TokenKind, err: ParseErrorKind) -> PResult<()> {
        if self.check(kind) {
            Ok(())
        } else {
            Err(self.error(err))
        }
    }

    fn expect_eof(&self) -> PResult<()> {
        if self.peek().is_none() {
            Ok(())
        } else {
            Err(self.error(ParseErrorKind::ExpectedEnd))
        }
    }

    fn check_symbol(&mut self) -> Option<&'a str> {
        if let Some(TokenKind::Symbol(name)) = self.peek() {
            self.advance();
            Some(name)
        } else {
            None
        }
    }

    fn check_var(&mut self) -> Option<&'a str> {
        if let Some(TokenKind::Var(name)) = self.peek() {
            self.advance();
            Some(name)
        } else {
            None
        }
    }

    fn check_number(&mut self) -> Option<u64> {
        if let Some(TokenKind::Number(num)) = self.peek() {
            self.advance();
            Some(num)
        } else {
            None
        }
    }

    fn parse_term(&mut self) -> PResult<Term<Var>> {
        if let Some(name) = self.check_symbol() {
            let atom = Atom { symbol: Symbol::new_atom(name) };
            Ok(Term::Atom(atom))
        } else if let Some(name) = self.check_var() {
            let var = self.make_var(name);
            Ok(Term::Var(var))
        } else if let Some(num) = self.check_number() {
            Ok(Term::Number(num))
        } else {
            Err(self.error(ParseErrorKind::ExpectedTermOrParen))
        }
    }

    fn make_var(&mut self, name: &'a str) -> Var {
        *self
            .vars
            .entry(name)
            .or_insert_with(|| Var { symbol: Symbol::new_var(name) })
    }

    fn parse_terms(&mut self) -> PResult<Vec<Term<Var>>> {
        self.expect(TokenKind::LeftParen, ParseErrorKind::ExpectedLeftParenOrOp)?;
        let mut terms = Vec::new();
        if self.check(TokenKind::RightParen) {
            return Ok(terms);
        }
        loop {
            terms.push(self.parse_term()?);
            if self.check(TokenKind::RightParen) {
                return Ok(terms);
            }
            self.expect(TokenKind::Comma, ParseErrorKind::ExpectedCommaOrParen)?;
        }
    }

    fn parse_predicate(&mut self) -> PResult<Predicate<Var>> {
        let lhs = if let Some(name) = self.check_symbol() {
            let name = Symbol::new_atom(name);
            if self.peek() == Some(TokenKind::LeftParen) {
                let args = self.parse_terms()?;
                return Ok(Predicate::Named(NamedPredicate {
                    name,
                    args,
                }));
            } else {
                Term::Atom(Atom { symbol: name })
            }
        } else {
            self.parse_term()?
        };
        if self.check(TokenKind::Equal) {
            let rhs = self.parse_term()?;
            Ok(Predicate::Equality(EqualityPredicate { args: [lhs, rhs], }))
        } else {
            let cmp = match self.peek() {
                Some(TokenKind::Less) => Comparison::Less,
                Some(TokenKind::LessEqual) => Comparison::LessEqual,
                Some(TokenKind::Greater) => Comparison::Greater,
                Some(TokenKind::GreaterEqual) => Comparison::GreaterEqual,
                _ => return Err(self.error(ParseErrorKind::ExpectedOp)),
            };
            self.advance();
            let rhs = self.parse_term()?;
            Ok(Predicate::Comparison(ComparisonPredicate {
                args: [lhs, rhs],
                comparison: cmp,
            }))
        }
    }

    fn parse_rule(&mut self) -> PResult<Rule<Var>> {
        let head = self.parse_predicate()?;
        if self.check(TokenKind::Dot) {
            self.expect_eof()?;
            return Ok(Rule {
                head,
                tail: Vec::new(),
            });
        }
        self.expect(TokenKind::ImpliedBy, ParseErrorKind::ExpectedDotOrImplies)?;
        let mut tail = Vec::new();
        loop {
            tail.push(self.parse_predicate()?);
            if self.check(TokenKind::Dot) {
                self.expect_eof()?;
                return Ok(Rule { head, tail });
            }
            self.expect(TokenKind::Comma, ParseErrorKind::ExpectedCommaOrDot)?;
        }
    }

    fn parse_input(&mut self) -> PResult<Input> {
        if self.check(TokenKind::Question) {
            let query = self.parse_predicate()?;
            self.expect_eof()?;
            Ok(Input::Query(query))
        } else {
            Ok(Input::Rule(self.parse_rule()?))
        }
    }
}

pub fn parse_rule(tokens: &[Token]) -> PResult<Rule<Var>> {
    let mut parser = Parser::new(tokens);
    parser.parse_rule()
}

pub fn parse(tokens: &[Token]) -> PResult<Input> {
    let mut parser = Parser::new(tokens);
    parser.parse_input()
}
