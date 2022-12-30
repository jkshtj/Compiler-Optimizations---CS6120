//! Generic dataflow framework.
pub mod live_variables;
pub mod reaching_definitions;
mod sets_util;

extern crate bril_control_flow;
extern crate bril_rs;

use bril_control_flow::{BasicBlock, ControlFlowGraph};
use std::fmt::Debug;
use Direction::Forward;

/// Direction of the dataflow analysis.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Direction {
    /// Forward means OUT set of a node is a function of its IN set.
    Forward,
    /// Backward means IN set of a node is a function of its OUT set.
    Backward,
}

/// Trait that must be implemented by types that
/// represent a control flow graph. The trait provides
/// a uniform interface to expose general control flow graph
/// information, such as, nubmer of basic blocks in the graph,
/// a list of basic blocks, successors/predecessors of a basic block,
/// index of a basic block, its successors or predecessors etc.
pub trait ControlFlow {
    type Node: Debug + Clone;

    fn num_blocks(&self) -> usize;
    fn nodes(&self) -> Vec<&Self::Node>;
    fn successors_of(&self, node: &Self::Node) -> Vec<&Self::Node>;
    fn predecessors_of(&self, node: &Self::Node) -> Vec<&Self::Node>;
    fn index_of(&self, node: &Self::Node) -> usize;
    fn predecessor_indices_of(&self, node: &Self::Node) -> Vec<usize>;
    fn successor_indices_of(&self, node: &Self::Node) -> Vec<usize>;
}

impl ControlFlow for ControlFlowGraph {
    type Node = BasicBlock;

    fn num_blocks(&self) -> usize {
        self.nodes.len()
    }

    fn nodes(&self) -> Vec<&Self::Node> {
        self.nodes.iter().collect()
    }

    fn successors_of(&self, node: &Self::Node) -> Vec<&Self::Node> {
        let bb_index = self.bb_label_to_index_map[node.name()];
        self.successors[bb_index]
            .iter()
            .map(|&i| &self.nodes[i])
            .collect()
    }

    fn predecessors_of(&self, node: &Self::Node) -> Vec<&Self::Node> {
        let bb_index = self.bb_label_to_index_map[node.name()];
        self.predecessors[bb_index]
            .iter()
            .map(|&i| &self.nodes[i])
            .collect()
    }

    fn index_of(&self, node: &Self::Node) -> usize {
        self.bb_label_to_index_map[node.name()]
    }

    fn predecessor_indices_of(&self, node: &Self::Node) -> Vec<usize> {
        let bb_index = self.bb_label_to_index_map[node.name()];
        self.predecessors[bb_index].clone()
    }

    fn successor_indices_of(&self, node: &Self::Node) -> Vec<usize> {
        let bb_index = self.bb_label_to_index_map[node.name()];
        self.successors[bb_index].clone()
    }
}

/// The generic dataflow analysis framework. This framework is generic over
/// 4 parameters.
/// 1. Value -
/// The set of "values" that the dataflow analysis is
/// computing on. Eg, for live-variable analysis
/// the values are just sets of variables.
///
/// 2. Meet -
/// Defines the operator used for combining values from
/// multiple incoming edges. Eg, for live-variable analysis
/// the meet operator is the Union operator.
///
/// In FORWARD analysis, the incoming edges are the predecessors
/// of the current basic block. And in BACKWARD analysis, incoming
/// edges are the successors of the basic block.
///
/// 3. Transfer -
/// Defines how dataflow values are transformed by a node.
/// Depends on the direction of the dataflow. For FORWARD analysis,
/// the transfer function defines how the OUT set of the node depends
/// on its IN set.
///
/// 4. InitEntry -
/// Defines how the entry block for this dataflow
/// analysis is initialized. The entry block may be the
/// first or the last block, depending on the direction
/// of the analysis.
///
/// Depending on the specifics of the analysis, the logic to
/// initialize the entry block may differ as well.
///
/// **Note** - This framework assumes and depends on the fact that the
/// control flow graph it is being used upon is internally built using
/// indexes.
pub trait DataflowAnalysis<Value, Meet, Transfer, InitEntry>: ControlFlow
where
    Value: Debug + Default + Clone + PartialEq,
    Meet: Fn(Vec<Value>) -> Value,
    Transfer: Fn(&Self::Node, Value) -> Value,
    InitEntry: FnOnce(&Self) -> Value,
{
    /// Returns the direction of this dataflow analysis.
    fn direction() -> Direction;

    /// Generic dataflow solver, that implements the following algorithm -
    /// ```ignore
    /// // The logic to initialize the entry depends on the specific dataflow solver implementation.
    /// in[entry] = specialized_init_entry()
    /// out[*] = init
    ///
    /// worklist = all blocks
    /// while worklist is not empty:
    ///     b = pick any block from worklist
    ///     in[b] = merge(out[p] for every predecessor p of b)
    ///     out[b] = transfer(b, in[b])
    ///     if out[b] changed:
    ///         worklist += successors of b
    /// ```
    /// The algorithm has 4 components that its going to be generic over -
    /// 1. Direction of dataflow - FORWARD or BACKWARD
    /// 2. Dataflow values - Specified in the solver impl. In the above algorithm, represented by `in[b]` and `out[b]`.
    /// 3. Meet operator - Specified in the solver impl.
    /// 4. Transfer function - Specified in the solver impl.
    fn run_dataflow(
        &self,
        meet: Meet,
        transfer: Transfer,
        init_entry: InitEntry,
    ) -> (Vec<Value>, Vec<Value>) {
        let (mut in_set, mut out_set) = (
            vec![Value::default(); self.num_blocks()],
            vec![Value::default(); self.num_blocks()],
        );
        if Self::direction() == Forward {
            in_set[0] = init_entry(self);
        } else {
            out_set[self.num_blocks() - 1] = init_entry(self);
        };

        let mut worklist = self.nodes();

        loop {
            let mut new_worklist = vec![];
            for bb in worklist.into_iter() {
                let i = self.index_of(bb);

                if Self::direction() == Forward {
                    // Derive this node's IN set from the OUT sets of
                    // all its predecessors.
                    in_set[i] = meet(
                        self.predecessor_indices_of(bb)
                            .into_iter()
                            .map(|i| out_set[i].clone())
                            .collect(),
                    );

                    let out_set_new = transfer(bb, in_set[i].clone());

                    // OUT set for this node has changed. Put all its
                    // successors on the worklist.
                    if out_set_new != out_set[i] {
                        out_set[i] = out_set_new;
                        new_worklist.extend(self.successors_of(&bb))
                    }
                } else {
                    // Derive this node's OUT set from the IN sets of
                    // all its successors.
                    out_set[i] = meet(
                        self.successor_indices_of(bb)
                            .into_iter()
                            .map(|i| in_set[i].clone())
                            .collect(),
                    );

                    let in_set_new = transfer(bb, out_set[i].clone());

                    // IN set for this node has changed. Put all its
                    // predecessors on the worklist.
                    if in_set_new != in_set[i] {
                        in_set[i] = in_set_new;
                        new_worklist.extend(self.predecessors_of(&bb))
                    }
                }
            }

            // The fix point algorithm has converged.
            if new_worklist.is_empty() {
                break;
            }

            worklist = new_worklist;
        }

        (in_set, out_set)
    }
}
