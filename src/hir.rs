mod bind;

use std::collections::HashMap;

pub use bind::Binder;

#[derive(Clone)]
pub struct Scope<'a, T> {
    map: HashMap<String, T>,
    parent: Option<&'a Scope<'a, T>>,
}

impl<'a, T> Scope<'a, T> {
    pub fn new() -> Self {
        Self {
            map: HashMap::from([]),
            parent: None,
        }
    }

    pub fn with_parent(parent: &'a Self) -> Self {
        Self {
            map: HashMap::from([]),
            parent: Some(parent),
        }
    }

    pub fn lookup(&self, key: &String) -> Option<&T> {
        match self.map.get(key) {
            Some(t) => Some(t),
            None => match self.parent {
                Some(p) => p.lookup(key),
                None => None,
            },
        }
    }

    pub fn put(&mut self, key: String, value: T) -> Option<T> {
        self.map.insert(key, value)
    }
}
