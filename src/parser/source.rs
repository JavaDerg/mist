use crate::parser::span::TokenSource;

pub type Source = usize;

#[derive(Default)]
pub struct SourceTracker {
    cur: usize,
    sources: Vec<Box<dyn TokenSource>>,
}

impl SourceTracker {
    pub fn push(&mut self, source: impl TokenSource + Sized + 'static) -> Source {
        self.sources.push(Box::new(source));
        self.cur += 1;
        self.cur - 1
    }

    pub fn lookup(&self, src: Source) -> Option<&dyn TokenSource> {
        self.sources.get(src).map(|src| &**src)
    }
}
