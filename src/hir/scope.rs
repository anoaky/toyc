use std::{collections::HashMap, hash::Hash};

#[derive(Clone)]
pub struct Scope<'a, K, V> {
    map: HashMap<K, V>,
    parent: Option<&'a Scope<'a, K, V>>,
}

impl<'a, K, V> Scope<'a, K, V>
where
    V: Eq,
    K: Eq + Hash,
{
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

    pub fn lookup(&self, key: &K) -> Option<&V> {
        match self.map.get(key) {
            Some(t) => Some(t),
            None => match self.parent {
                Some(p) => p.lookup(key),
                None => None,
            },
        }
    }

    pub fn lookup_local(&self, key: &K) -> Option<&V> {
        self.map.get(key)
    }

    pub fn put(&mut self, key: K, value: V) -> Option<V> {
        self.map.insert(key, value)
    }
}
