use std::collections::HashSet;
use std::hash::Hash;

pub fn union<T: Hash + Eq>(mut set1: HashSet<T>, set2: HashSet<T>) -> HashSet<T> {
    set1.extend(set2);
    set1
}

pub fn difference<T: Hash + Eq>(mut set1: HashSet<T>, set2: HashSet<T>) -> HashSet<T> {
    set1.retain(|v| !set2.contains(v));
    set1
}
