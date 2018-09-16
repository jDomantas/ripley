use std::io::{self, Write};
use error::CompileError;

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum TokenKind<'a> {
    Symbol(&'a str),
    Var(&'a str),
    Number(u64),
    LeftParen,
    RightParen,
    Comma,
    Dot,
    ImpliedBy,
    Question,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Copy, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct LexError {
    position: usize,
}

impl CompileError for LexError {
    fn report(&self, source: &str) {
        let stdout = io::stdout();
        let mut stdout = stdout.lock();
        let _ = writeln!(stdout, "error: invalid token");
        let _ = writeln!(stdout, "at column {}", self.position + 1);
        let _ = writeln!(stdout, " {}", source);
        for _ in 0..=self.position {
            let _ = write!(stdout, " ");
        }
        let _ = writeln!(stdout, "^");
    }
}

struct Lexer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            source,
            pos: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().next()
    }

    fn advance(&mut self) {
        if let Some(ch) = self.peek() {
            self.pos += 1;
            self.source = &self.source[ch.len_utf8()..];
        }
    }

    fn check(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn single_char(&mut self, kind: TokenKind<'static>) -> Token<'a> {
        let start = self.pos;
        self.advance();
        let end = self.pos;
        Token { kind, start, end }
    }

    fn one_or_two(&mut self, ch: char, one: TokenKind<'static>, two: TokenKind<'static>) -> Token<'a> {
        let start = self.pos;
        self.advance();
        let kind = match self.peek() {
            Some(c) if c == ch => {
                self.advance();
                two
            }
            _ => one,
        };
        let end = self.pos;
        Token { kind, start, end }
    }

    fn eat_name<F>(&mut self, constructor: F) -> Token<'a>
    where
        F: FnOnce(&'a str) -> TokenKind<'a>
    {
        let start = self.pos;
        let slice = self.source;
        loop {
            match self.peek() {
                Some(ch) if is_name_char(ch) => self.advance(),
                _ => break,
            }
        }
        let end = self.pos;
        let suffix = self.source;
        let name = &slice[..(slice.len() - suffix.len())];
        let kind = constructor(name);
        Token {
            kind,
            start,
            end
        }
    }

    fn eat_number(&mut self) -> Result<Token<'static>, LexError> {
        let mut value = Some(0u64);
        let start = self.pos;
        loop {
            match self.peek() {
                Some(c @ '0' ... '9') => {
                    self.advance();
                    let digit = c as u64 - '0' as u64;
                    value = value
                        .and_then(|x| x.checked_mul(10))
                        .and_then(|x| x.checked_add(digit));
                }
                Some(c) if is_name_char(c) => {
                    return Err(LexError {
                        position: self.pos,
                    });
                }
                _ => break,
            }
        }
        if let Some(value) = value {
            Ok(Token {
                kind: TokenKind::Number(value),
                start,
                end: self.pos,
            })
        } else {
            Err(LexError {
                position: start,
            })
        }
    }

    fn next_token(&mut self) -> Result<Option<Token<'a>>, LexError> {
        loop {
            let ch = if let Some(ch) = self.peek() { ch } else { return Ok(None); };
            match ch {
                ch if ch.is_whitespace() => { self.advance(); }
                '(' => return Ok(Some(self.single_char(TokenKind::LeftParen))),
                ')' => return Ok(Some(self.single_char(TokenKind::RightParen))),
                ',' => return Ok(Some(self.single_char(TokenKind::Comma))),
                '.' => return Ok(Some(self.single_char(TokenKind::Dot))),
                '?' => return Ok(Some(self.single_char(TokenKind::Question))),
                ':' => {
                    let start = self.pos;
                    self.advance();
                    return if self.check('-') {
                        Ok(Some(Token {
                            kind: TokenKind::ImpliedBy,
                            start,
                            end: self.pos,
                        }))
                    } else {
                        Err(LexError {
                            position: self.pos,
                        })
                    };
                }
                '\\' => {
                    let start = self.pos;
                    self.advance();
                    return if self.check('=') {
                        Ok(Some(Token {
                            kind: TokenKind::NotEqual,
                            start,
                            end: self.pos,
                        }))
                    } else {
                        Err(LexError {
                            position: self.pos,
                        })
                    };
                }
                '<' => return Ok(Some(self.single_char(TokenKind::Less))),
                '>' => return Ok(Some(self.one_or_two('=', TokenKind::Greater, TokenKind::GreaterEqual))),
                '=' => return Ok(Some(self.one_or_two('<', TokenKind::Equal, TokenKind::LessEqual))),
                '0' ... '9' => return Ok(Some(self.eat_number()?)),
                'a' ... 'z' => return Ok(Some(self.eat_name(TokenKind::Symbol))),
                'A' ... 'Z' => return Ok(Some(self.eat_name(TokenKind::Var))),
                _ => return Err(LexError { position: self.pos }),
            }
        }
    }
}

fn is_name_char(ch: char) -> bool {
    match ch {
        'a' ... 'z' |
        'A' ... 'Z' |
        '0' ... '9' |
        '_' => true,
        _ => false,
    }
}

pub fn lex<'a>(source: &'a str) -> Result<Vec<Token<'a>>, LexError> {
    let mut tokens = Vec::new();
    let mut lexer = Lexer::new(source);
    while let Some(token) = lexer.next_token()? {
        tokens.push(token);
    }
    Ok(tokens)
}
