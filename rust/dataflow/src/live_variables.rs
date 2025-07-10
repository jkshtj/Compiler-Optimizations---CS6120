//! Dataflow analysis for reaching definitions.
use crate::sets_util::{difference, union};
use crate::{DataflowAnalysis, Direction};
use bril_control_flow::{BasicBlock, ControlFlowGraph};
use bril_rs::{Code, Instruction};
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;

/// Represents the type of values that the live
/// variable dataflow analysis is carried on.
pub type LiveVariable = String;

pub type DataflowValue = HashSet<LiveVariable>;

pub fn meet(successors: Vec<DataflowValue>) -> DataflowValue {
    successors.into_iter().flatten().collect()
}

/// Transfer function for live variable analysis -
/// in(out) = use(b) ∪ (out - def(b))
/// in => Set of incoming live variables.
/// out => Set of outgoing live variables.
/// use => Set of variables used in this block.
/// def => Set of variable defined in this basic block.
///         were redefined in this basic block.
pub fn transfer(bb: &BasicBlock, out_set: DataflowValue) -> DataflowValue {
    fn used(code: &Code) -> HashSet<String> {
        match code {
            Code::Instruction(instr) => match instr {
                Instruction::Value { args, .. } | Instruction::Effect { args, .. } => {
                    HashSet::from_iter(args.iter().cloned())
                }
                _ => HashSet::new(),
            },
            _ => HashSet::new(),
        }
    }

    fn defined(code: &Code) -> HashSet<String> {
        match code {
            Code::Instruction(instr) => {
                let defined = match instr {
                    Instruction::Constant { dest, .. } | Instruction::Value { dest, .. } => {
                        Some(dest.to_owned())
                    }
                    _ => None,
                };

                let mut result = HashSet::new();
                if let Some(defined) = defined {
                    result.insert(defined);
                }
                result
            }
            _ => HashSet::new(),
        }
    }

    let mut out_set = out_set;

    for code in bb.instrs.iter().rev() {
        let used = used(code);
        let defined = defined(code);
        let in_set = union(used, difference(out_set, defined));
        out_set = in_set;
    }

    // We are iterating through instructions in the
    // basic block in reverse - last to first. In the last
    // iteration of the above for loop, when we are looking
    // at the first instruction of the basic block, the determined
    // `in_set` of the instruction will be assigned to `out_set`.
    // Just using correct naming due to this case here.
    let in_set = out_set;
    in_set
}

pub fn init_entry(_cfg: &ControlFlowGraph) -> DataflowValue {
    HashSet::new()
}

impl<Meet, Transfer, InitEntry> DataflowAnalysis<DataflowValue, Meet, Transfer, InitEntry>
    for ControlFlowGraph
where
    Meet: Fn(Vec<DataflowValue>) -> DataflowValue,
    Transfer: Fn(&BasicBlock, DataflowValue) -> DataflowValue,
    InitEntry: FnOnce(&Self) -> DataflowValue,
{
    fn direction() -> Direction {
        Direction::Backward
    }
}

pub fn find_live_variables(cfg: ControlFlowGraph) {
    let (in_sets, out_sets) = cfg.run_dataflow(meet, transfer, init_entry);

    for (i, bb) in cfg.nodes.into_iter().enumerate() {
        println!("{}", bb.name());

        print!("IN: ");
        in_sets[i].iter().for_each(|v| print!("{}, ", v));
        println!();

        print!("OUT: ");
        out_sets[i].iter().for_each(|v| print!("{}, ", v));
        println!("\n");
    }
}
