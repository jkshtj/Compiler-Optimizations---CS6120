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
    pub strictly_dominated_by_me: Vec<HashSet<usize>>,
    pub immediately_dominated_by_me: Vec<HashSet<usize>>,
}

impl Display for DominanceTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "=== Strictly dominated ===")?;
        for (i, doms) in self.strictly_dominated_by_me.iter().enumerate() {
            write!(f, "{} => ", self.nodes[i].name())?;
            for dom in doms.iter() {
                write!(f, "{}, ", self.nodes[*dom].name())?;
            }
            writeln!(f, "")?;
        }

        writeln!(f, "=== Immediately dominated ===")?;
        for (i, doms) in self.immediately_dominated_by_me.iter().enumerate() {
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
        let dominated_by_me = invert_dominators(find_dominators(&cfg));
        let strictly_dominated_by_me: Vec<HashSet<usize>> = dominated_by_me
            .into_iter()
            .enumerate()
            .map(|(i, mut dbm)| {
                dbm.remove(&i);
                dbm
            })
            .collect();

        let mut immediately_dominated_by_me = strictly_dominated_by_me.clone();

        // A node immediately dominates only those nodes that it strictly
        // dominates, but those strictly dominated by it don't strictly
        // dominate.
        //
        // For instance, let's say we have the following nodes in our dominance tree -
        // ```
        // 0: {1, 2, 3}
        // 1: {2, 3}
        // 2: {}
        // 3: {}
        // ```
        // The node `0` immediately dominated only those nodes that it
        // strictly dominates, minus the ones that its strictly dominated
        // children don't strictly dominate:
        // ```
        //  {1, 2, 3} - {{2, 3} U {} U {}}
        // ```
        for i in 0..strictly_dominated_by_me.len() {
            // Get a set of all nodes strictly dominated by all nodes
            // I strictly dominate.
            let strictly_dominated_2x: HashSet<usize> = strictly_dominated_by_me[i]
                .iter()
                .flat_map(|sdbm| strictly_dominated_by_me[*sdbm].clone())
                .collect();
            immediately_dominated_by_me[i] = immediately_dominated_by_me[i]
                .difference(&strictly_dominated_2x)
                .copied()
                .collect();
        }

        DominanceTree {
            nodes: cfg.nodes,
            bb_label_to_index_map: cfg.bb_label_to_index_map,
            strictly_dominated_by_me,
            immediately_dominated_by_me,
        }
    }
}

/// Returns the dominator sets associated to a bril function.
pub fn find_dominators(cfg: &ControlFlowGraph) -> Vec<HashSet<usize>> {
    let mut dominate_me = vec![(0..cfg.nodes.len()).collect::<HashSet<usize>>(); cfg.nodes.len()];

    // The only possible dominator of the entry block is the
    // entry block itself.
    dominate_me[0] = {
        let mut entry_set = HashSet::new();
        entry_set.insert(0);
        entry_set
    };

    let mut dom_changed = true;

    while dom_changed {
        dom_changed = false;
        // For every node in the cfg except for the entry block.
        for i in 1..cfg.nodes.len() {
            let mut dominators_of_preds = cfg.predecessors[i].iter().map(|i| &dominate_me[*i]).fold(
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

            if dominate_me[i] != dominators_of_preds {
                dom_changed = true;
                dominate_me[i] = dominators_of_preds;
            }
        }
    }

    dominate_me
}

/// Given the dominator sets for a function, which represent the set of nodes that
/// ***dominate*** each node, inverts the relations to create information representing the
/// set of nodes ***dominated by*** each node.
pub fn invert_dominators(dominate_me: Vec<HashSet<usize>>) -> Vec<HashSet<usize>> {
    let mut dominated_by_me = vec![HashSet::new(); dominate_me.len()];
    for i in 0..dominate_me.len() {
        for &dominator in dominate_me[i].iter() {
            dominated_by_me[dominator].insert(i);
        }
    }
    dominated_by_me
}

pub fn print_dominators(cfg: &ControlFlowGraph, dominate_me: bool) {
    if dominate_me {
        let dominate_me = find_dominators(cfg);
        println!("=== Dominate me ===");
        for (i, dominate_me) in dominate_me.iter().enumerate() {
            print!("{} => ", cfg.nodes[i].name());
            for dom in dominate_me.iter() {
                print!("{}, ", cfg.nodes[*dom].name());
            }
            println!();
        }
    } else {
        let dominated_by_me = invert_dominators(find_dominators(cfg));
        println!("=== Dominated by me ===");
        for (i, dominated_by_me) in dominated_by_me.iter().enumerate() {
            print!("{} => ", cfg.nodes[i].name());
            for dbm in dominated_by_me {
                print!("{}, ", cfg.nodes[*dbm].name());
            }
            println!();
        }
    }
}

/// Returns the dominance frontier sets associated to a bril function.
///
/// General idea
/// ============
/// Every node on the dominance frontier of a node `n` is -
///
/// 1. Either a direct successor of `n` in the CFG, that is not
/// dominated by `n`.
///
/// 2. Or a direct successor `a`, in the CFG, of one of `n`'s children `c`
/// in the dominator tree, not dominated by `c`.
pub fn find_dominance_frontiers(cfg: &ControlFlowGraph) -> Vec<HashSet<usize>> {
    let dominate_me = find_dominators(cfg);
    let dominated_by_me = invert_dominators(dominate_me);
    let mut frontiers = vec![HashSet::new(); dominated_by_me.len()];

    for block in 0..dominated_by_me.len() {
        // The (recursive) successors of a given basic block, i.e.,
        // all basic blocks that a given basic block can reach.
        let mut successors = HashSet::new();
        for &dominated in dominated_by_me[block].iter() {
            successors.extend(cfg.successors[dominated].iter());
        }

        // If a basic block B is a successor of another basic block A,
        // but is not dominated by A, then it will constitute A's frontiers.
        successors
            .into_iter()
            .filter(|succ| !dominated_by_me[block].contains(&succ) || *succ == block)
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
