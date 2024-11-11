
pub trait IdProvider {
    fn next_id(&self) -> usize;
}

#[derive(Debug)]
pub struct IdVal {
    id: usize,
}

impl IdProvider for IdVal {
    fn next_id(&self) -> usize {
        self.id
    }
}

impl IdVal {
    pub fn new() -> Self {
        Self { id: 0 }
    }

    pub fn increment(&mut self) {
        self.id += 1;
    }
}
