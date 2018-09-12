pub trait CompileError {
    fn report(&self, source: &str);
}
