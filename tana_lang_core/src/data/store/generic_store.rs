use std::{
    collections::HashMap,
    hash::Hash,
};
use anyhow::{
    bail,
    Result,
};
use crate::data::*;

#[derive(Debug)]
pub struct GenericStore<Key, Val>
where
    Key: Clone + Eq + Hash + Construct,
{
    map: HashMap<Key, Val>,
    vec: Vec<(Key, Val)>,
}

impl<Key, Val> GenericStore<Key, Val>
where
    Key: Clone + Eq + Hash + Construct,
    Val: Clone,
{
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            vec: Vec::new(),
        }
    }

    pub fn insert(&mut self, key: Key, val: Val) -> Result<Val> {
        if self.map.contains_key(&key) {
            bail!("Registration duplicated: `{}`", key.description());
        }
        self.map.insert(key.clone(), val.clone());
        self.vec.push((key, val.clone()));
        Ok(val)
    }

    pub fn insert_or_get(&mut self, key: Key, val: Val) -> Val {
        if let Some(val) = self.map.get(&key) {
            val.clone()
        }
        else {
            self.map.insert(key.clone(), val.clone());
            self.vec.push((key.clone(), val.clone()));
            val
        }
    }

    pub fn get(&self, key: &Key) -> Result<Val> {
        if let Some(val) = self.map.get(key) {
            Ok(val.clone())
        }
        else {
            bail!("Key not found: `{}`", key.description());
        }
    }

    pub fn keys_and_vals<'a>(&'a self) -> impl Iterator<Item = &'a (Key, Val)> {
        self.vec.iter()
    }
}
