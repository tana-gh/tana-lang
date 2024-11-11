use std::{
    collections::HashMap,
    hash::Hash,
    rc::Rc,
};
use anyhow::{
    bail,
    Result,
};
use crate::data::*;

#[derive(Debug)]
pub struct IdStore<Key, Val>
where
    Key: Clone + Eq + Hash + Construct,
{
    id_map: HashMap<Key, usize>,
    vals: Vec<Rc<Val>>,
}

impl<Key, Val> IdProvider for IdStore<Key, Val>
where
    Key: Clone + Eq + Hash + Construct,
{
    fn next_id(&self) -> usize {
        self.vals.len()
    }
}

impl<Key, Val> IdStore<Key, Val>
where
    Key: Clone + Eq + Hash + Construct,
{
    pub fn new() -> Self {
        Self {
            id_map: HashMap::new(),
            vals: Vec::new(),
        }
    }

    pub fn insert(&mut self, key: Key, val: Rc<Val>) -> Result<Rc<Val>> {
        if self.id_map.contains_key(&key) {
            bail!("Registration duplicated: `{}`", key.description())
        }
        let id = self.next_id();
        self.id_map.insert(key, id);
        self.vals.push(val.clone());
        Ok(val)
    }

    pub fn insert_or_get(&mut self, key: Key, val: Rc<Val>) -> Rc<Val> {
        if let Some(id) = self.id_map.get(&key) {
            self.vals[*id].clone()
        }
        else {
            let id = self.next_id();
            self.id_map.insert(key, id);
            self.vals.push(val.clone());
            val
        }
    }

    pub fn force_insert(&mut self, key: Key, val: Rc<Val>) -> Rc<Val> {
        if let Some(id) = self.id_map.get(&key) {
            self.vals[*id] = val.clone();
        }
        else {
            let id = self.next_id();
            self.id_map.insert(key, id);
            self.vals.push(val.clone());
        }
        val
    }

    pub fn get(&self, key: &Key) -> Result<Rc<Val>> {
        if let Some(id) = self.id_map.get(key) {
            Ok(self.vals[*id].clone())
        }
        else {
            bail!("Key not found: `{}`", key.description());
        }
    }

    pub fn vals<'a>(&'a self) -> impl Iterator<Item = Rc<Val>> + 'a {
        self.vals.iter().cloned()
    }
}
