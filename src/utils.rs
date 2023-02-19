use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

#[derive(Debug)]
pub struct PureRef<'a, T>(pub &'a T);

impl<'a, T> PartialEq for PureRef<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<'a, T> Eq for PureRef<'a, T> {}

impl<'a, T> Clone for PureRef<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'a, T> Copy for PureRef<'a, T> {}

impl<'a, T> Deref for PureRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, T> Hash for PureRef<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as *const T as usize)
    }
}

pub fn hash_dedup<T: Eq + Hash + Clone>(items: &mut Vec<T>) {
    let mut seen = HashSet::new();
    items.retain(|item| seen.insert(item.clone()));
}