use std::collections::HashMap;
use std::fmt;
use std::sync::Mutex;

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub struct Symbol {
    id: u32,
}

impl Symbol {
    pub fn new_var(from: &str) -> Symbol {
        let mut table = SYMBOL_TABLE.lock().unwrap();
        let sym = Symbol {
            id: table.symbol_names.len() as u32,
        };
        table.symbol_names.push(from.to_string());
        sym
    }

    pub fn new_atom(from: &str) -> Symbol {
        let mut table = SYMBOL_TABLE.lock().unwrap();
        if let Some(&symbol) = table.atoms.get(from) {
            return symbol;
        }
        let sym = Symbol {
            id: table.symbol_names.len() as u32,
        };
        table.symbol_names.push(from.to_string());
        table.atoms.insert(from.to_string(), sym);
        sym
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let table = SYMBOL_TABLE.lock().unwrap();
        let name = &table.symbol_names[self.id as usize];
        write!(f, "{}", name)
    }
}

struct SymbolTable {
    symbol_names: Vec<String>,
    atoms: HashMap<String, Symbol>,
}

lazy_static! {
    static ref SYMBOL_TABLE: Mutex<SymbolTable> = Mutex::new(SymbolTable {
        symbol_names: Vec::new(),
        atoms: HashMap::new(),
    });
}
