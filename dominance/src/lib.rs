//! This module contains dominance utilities for Bril functions.
//!
//! Dominance relationships have the following 2 variants and can be defined
//! as follows.
//! 1. A dominates B iff all paths from the entry to B include A.
//! 2. A post-dominates B iff all paths from B to the exit include A.
//!
//! Within the above mentioned variants there are further nuanced definitions
//! of dominance as well.
//! 1. Strict - A strictly dominates B iff A dominates B and A ≠ B.
//!     Dominance is reflexive, so “strict” dominance just takes
//!     that part away.
//! 2. Immediate - A immediately dominates B iff A dominates B but A does not strictly
//!    dominate any other node that strictly dominates B, in which case A
//!     is B’s direct parent in the dominator tree.
//! 3. PostStrict - A strictly post-dominates B iff A post-dominates B and A ≠ B.
//! 4. PostImmediate - A immediately post-dominates B iff A post-dominates B but A does not
//!    strictly post-dominate any other node that strictly dominates B, in which
//!    case A is B’s direct child in the dominator tree.

extern crate bril_control_flow;
extern crate bril_rs;

use bril_control_flow::{BasicBlock, ControlFlowGraph};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

/// The dominator tree is a convenient data structure for storing the dominance
/// relationships in an entire function. The recursive children of a given node
/// in a tree are the nodes that that node dominates.
#[derive(Debug)]
pub struct DominanceTree {
    pub nodes: Vec<BasicBlock>,
    pub bb_label_to_index_map: HashMap<String, usize>,
    pub children: Vec<HashSet<usize>>,
}

impl Display for DominanceTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "=== Basic Blocks ===")?;
        for bb in self.nodes.iter() {
            writeln!(f, "{bb}")?;
        }

        writeln!(f, "=== Dominance Relations ===")?;
        for (i, doms) in self.children.iter().enumerate() {
            write!(f, "{} => ", self.nodes[i].name())?;
            for dom in doms.iter() {
                write!(f, "{}, ", self.nodes[*dom].name())?;
            }
            writeln!(f, "")?;
        }

        Ok(())
    }
}

impl From<ControlFlowGraph> for DominanceTree {
    fn from(cfg: ControlFlowGraph) -> Self {
        let dominators = find_dominators(&cfg);
        let mut children = vec![HashSet::new(); cfg.nodes.len()];

        for i in 0..cfg.nodes.len() {
            let parent_dominator = cfg.predecessors[i]
                .iter()
                .filter(|&pred_idx| dominators[i].contains(pred_idx))
                .cloned()
                .collect::<Vec<usize>>();

            if parent_dominator.len() > 1 {
                panic!("A node cannot have more than 1 immediate dominator.");
            } else if let Some(parent_dominator) = parent_dominator.get(0) {
                children[*parent_dominator].insert(i);
            }
        }

        DominanceTree {
            nodes: cfg.nodes,
            bb_label_to_index_map: cfg.bb_label_to_index_map,
            children,
        }
    }
}

/// Returns the dominator sets associated to a bril function.
pub fn find_dominators(cfg: &ControlFlowGraph) -> Vec<HashSet<usize>> {
    let mut dominators = vec![(0..cfg.nodes.len()).collect::<HashSet<usize>>(); cfg.nodes.len()];

    // The only possible dominator of the entry block is the
    // entry block itself.
    dominators[0] = {
        let mut entry_set = HashSet::new();
        entry_set.insert(0);
        entry_set
    };

    let mut dom_changed = true;

    while dom_changed {
        dom_changed = false;
        // For every node in the cfg except for the entry block.
        for i in 1..cfg.nodes.len() {
            let mut dominators_of_preds = cfg.predecessors[i].iter().map(|i| &dominators[*i]).fold(
                HashSet::new(),
                |acc, curr_pred_dominators| {
                    if acc.is_empty() {
                        curr_pred_dominators.clone()
                    } else {
                        acc.intersection(curr_pred_dominators).cloned().collect()
                    }
                },
            );

            dominators_of_preds.insert(i);

            if dominators[i] != dominators_of_preds {
                dom_changed = true;
                dominators[i] = dominators_of_preds;
            }
        }
    }

    dominators
}

pub fn print_dominators(cfg: &ControlFlowGraph) {
    let dominators = find_dominators(cfg);
    println!("=== Dominators ===");
    for (i, doms) in dominators.iter().enumerate() {
        print!("{} => ", cfg.nodes[i].name());
        for dom in doms.iter() {
            print!("{}, ", cfg.nodes[*dom].name());
        }
        println!("");
    }
}

/// Given the dominator sets for a function, which represent the set of nodes that
/// ***dominate*** each node, inverts the relations to create information representing the
/// set of nodes ***dominated by*** each node.
pub fn invert_dominators(dominators: Vec<HashSet<usize>>) -> Vec<HashSet<usize>> {
    let mut result = vec![HashSet::new(); dominators.len()];
    for i in 0..dominators.len() {
        for &dominator in dominators[i].iter() {
            result[dominator].insert(i);
        }
    }
    result
}

/// Returns the dominance frontier sets associated to a bril function.
pub fn find_dominance_frontiers(cfg: &ControlFlowGraph) -> Vec<HashSet<usize>> {
    let dominators = find_dominators(cfg);
    let dominated_by = invert_dominators(dominators);
    let mut frontiers = vec![HashSet::new(); dominated_by.len()];

    for block in 0..dominated_by.len() {
        // The (recursive) successors of a given basic block, i.e.,
        // all basic blocks that a given basic block can reach.
        let mut successors = HashSet::new();
        for &dominated in dominated_by[block].iter() {
            successors.extend(cfg.successors[dominated].iter());
        }

        // If a basic block B is a successor of another basic block A,
        // but is not dominated by A, then it will constitute A's frontiers.
        successors
            .into_iter()
            .filter(|succ| !dominated_by[block].contains(&succ) || *succ == block)
            .for_each(|succ| {
                frontiers[block].insert(succ);
            })
    }

    frontiers
}

pub fn print_dominance_frontiers(cfg: &ControlFlowGraph) {
    let dominance_frontiers = find_dominance_frontiers(cfg);
    println!("=== Dominance Frontiers ===");
    for (block, frontiers) in dominance_frontiers.iter().enumerate() {
        print!("{} => ", cfg.nodes[block].name());
        for frontier in frontiers.iter() {
            print!("{}, ", cfg.nodes[*frontier].name());
        }
        println!("");
    }
}

#[cfg(test)]
mod test {
    extern crate bril_control_flow;

    use crate::find_dominators;
    use bril_control_flow::ControlFlowGraph;
    use bril_rs::load_program_from_read;
    use std::collections::HashSet;

    /// A dominates B iff all paths from the entry to B include A.
    fn validate_dominance(cfg: &ControlFlowGraph, dominator: usize, dominated: usize) {
        fn dfs(
            cfg: &ControlFlowGraph,
            visited: &mut HashSet<usize>,
            curr: usize,
            dominator: usize,
            dominated: usize,
            mut found_dominator: bool,
        ) -> bool {
            if curr == dominator {
                found_dominator = true;
            }

            if curr == dominated {
                if found_dominator {
                    println!("Validated that dominator node: [{}], was visited on the path from entry to dominated node: [{}]", cfg.nodes[dominator].name(), cfg.nodes[dominated].name());
                    return true;
                } else {
                    println!("[ERROR] Dominator node: [{}] is not visited on all paths from entry to dominated: [{}]", cfg.nodes[dominator].name(), cfg.nodes[dominated].name());
                    return false;
                }
            }

            visited.insert(curr);

            for child in cfg.successors[curr].iter() {
                if !visited.contains(child) {
                    let result = dfs(cfg, visited, *child, dominator, dominated, found_dominator);
                    if !result {
                        panic!(
                            "Dominator set for node: [{}] is invalid.",
                            cfg.nodes[dominated].name()
                        );
                    }
                }
            }

            true
        }

        // Base case for dominance - every node dominates itself.
        if dominator == dominated {
            return;
        }

        let mut visited = HashSet::new();
        dfs(cfg, &mut visited, 0, dominator, dominated, false);
    }

    #[test]
    fn find_dominators_correctness() {
        let program = "{\"functions\":[{\"instrs\":[{\"label\":\"entry\"},{\"dest\":\"x\",\"op\":\"const\",\"type\":\"int\",\"value\":0},{\"dest\":\"i\",\"op\":\"const\",\"type\":\"int\",\"value\":0},{\"dest\":\"one\",\"op\":\"const\",\"type\":\"int\",\"value\":1},{\"label\":\"loop\"},{\"dest\":\"max\",\"op\":\"const\",\"type\":\"int\",\"value\":10},{\"args\":[\"i\",\"max\"],\"dest\":\"cond\",\"op\":\"lt\",\"type\":\"bool\"},{\"args\":[\"cond\"],\"labels\":[\"body\",\"exit\"],\"op\":\"br\"},{\"label\":\"body\"},{\"dest\":\"mid\",\"op\":\"const\",\"type\":\"int\",\"value\":5},{\"args\":[\"i\",\"mid\"],\"dest\":\"cond\",\"op\":\"lt\",\"type\":\"bool\"},{\"args\":[\"cond\"],\"labels\":[\"then\",\"endif\"],\"op\":\"br\"},{\"label\":\"then\"},{\"args\":[\"x\",\"one\"],\"dest\":\"x\",\"op\":\"add\",\"type\":\"int\"},{\"labels\":[\"endif\"],\"op\":\"jmp\"},{\"label\":\"endif\"},{\"dest\":\"factor\",\"op\":\"const\",\"type\":\"int\",\"value\":2},{\"args\":[\"x\",\"factor\"],\"dest\":\"x\",\"op\":\"mul\",\"type\":\"int\"},{\"args\":[\"i\",\"one\"],\"dest\":\"i\",\"op\":\"add\",\"type\":\"int\"},{\"labels\":[\"loop\"],\"op\":\"jmp\"},{\"label\":\"exit\"},{\"args\":[\"x\"],\"op\":\"print\"}],\"name\":\"main\"}]}";
        let program = load_program_from_read(program.as_bytes());

        for func in program.functions.iter() {
            let cfg = ControlFlowGraph::from(&*func);
            let dominators = find_dominators(&cfg);

            for (dominated, dominator_set) in dominators.iter().enumerate() {
                for &dominator in dominator_set {
                    validate_dominance(&cfg, dominator, dominated);
                }
            }
        }
    }

    #[test]
    #[should_panic]
    fn find_dominators_panics_when_invalid_dominator_set() {
        let program = "{\"functions\":[{\"instrs\":[{\"label\":\"entry\"},{\"dest\":\"x\",\"op\":\"const\",\"type\":\"int\",\"value\":0},{\"dest\":\"i\",\"op\":\"const\",\"type\":\"int\",\"value\":0},{\"dest\":\"one\",\"op\":\"const\",\"type\":\"int\",\"value\":1},{\"label\":\"loop\"},{\"dest\":\"max\",\"op\":\"const\",\"type\":\"int\",\"value\":10},{\"args\":[\"i\",\"max\"],\"dest\":\"cond\",\"op\":\"lt\",\"type\":\"bool\"},{\"args\":[\"cond\"],\"labels\":[\"body\",\"exit\"],\"op\":\"br\"},{\"label\":\"body\"},{\"dest\":\"mid\",\"op\":\"const\",\"type\":\"int\",\"value\":5},{\"args\":[\"i\",\"mid\"],\"dest\":\"cond\",\"op\":\"lt\",\"type\":\"bool\"},{\"args\":[\"cond\"],\"labels\":[\"then\",\"endif\"],\"op\":\"br\"},{\"label\":\"then\"},{\"args\":[\"x\",\"one\"],\"dest\":\"x\",\"op\":\"add\",\"type\":\"int\"},{\"labels\":[\"endif\"],\"op\":\"jmp\"},{\"label\":\"endif\"},{\"dest\":\"factor\",\"op\":\"const\",\"type\":\"int\",\"value\":2},{\"args\":[\"x\",\"factor\"],\"dest\":\"x\",\"op\":\"mul\",\"type\":\"int\"},{\"args\":[\"i\",\"one\"],\"dest\":\"i\",\"op\":\"add\",\"type\":\"int\"},{\"labels\":[\"loop\"],\"op\":\"jmp\"},{\"label\":\"exit\"},{\"args\":[\"x\"],\"op\":\"print\"}],\"name\":\"main\"}]}";
        let program = load_program_from_read(program.as_bytes());

        for func in program.functions.iter() {
            let cfg = ControlFlowGraph::from(&*func);
            let mut dominators = find_dominators(&cfg);

            // Inject error => Make `then` a dominator of `endif`.
            dominators[4].insert(3);

            for (dominated, dominator_set) in dominators.iter().enumerate() {
                for &dominator in dominator_set {
                    validate_dominance(&cfg, dominator, dominated);
                }
            }
        }
    }
}
