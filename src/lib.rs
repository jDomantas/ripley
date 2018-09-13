#[macro_use]
extern crate lazy_static;

pub mod error;
pub mod solver;
pub mod symbol;
pub mod terms;
pub mod syntax {
    pub mod lexer;
    pub mod parser;

    pub fn parse(source: &str) -> Result<::terms::Input, Box<::CompileError>> {
        let tokens = match lexer::lex(source) {
            Ok(tokens) => tokens,
            Err(e) => return Err(Box::new(e)),
        };
        parser::parse(&tokens).map_err(|e| Box::new(e) as Box<::CompileError>)
    }
}

pub use error::CompileError;
pub use solver::Solver;
pub use symbol::Symbol;
