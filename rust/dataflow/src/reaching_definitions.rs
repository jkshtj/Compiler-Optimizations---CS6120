//! Dataflow analysis for reaching definitions.
use crate::sets_util::union;
use crate::{DataflowAnalysis, Direction};
use bril_control_flow::{BasicBlock, ControlFlowGraph};
use bril_rs::{Code, Instruction};
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

/// Represents the type of values that the reaching
/// definition dataflow analysis is carried on.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ReachingDefinition {
    variable: String,
    definition: String,
}

pub type DataflowValue = HashSet<ReachingDefinition>;

pub fn meet(predecessors: Vec<DataflowValue>) -> DataflowValue {
    predecessors.into_iter().flatten().collect()
}

/// Transfer function for reaching definitions -
/// out(in) = def(b) ∪ (in - kills(b))
/// in => Set of incoming reaching definitions.
/// out => Set of outgoing reaching definitions.
/// def => Set of definitions created in this basic block.
/// kill => Set of definitions "killed" in this block, i.e., set of variables in `in` that
///         were redefined in this basic block.
pub fn transfer(bb: &BasicBlock, in_set: DataflowValue) -> DataflowValue {
    let mut in_map = in_set
        .into_iter()
        .map(|def| (def.variable, def.definition))
        .collect::<HashMap<String, String>>();

    let mut defined = HashSet::new();

    bb.instrs.iter().for_each(|code| {
        if let Code::Instruction(instr) = code {
            match instr {
                Instruction::Constant { dest, .. } | Instruction::Value { dest, .. } => {
                    // Remove "killed" definitions from the IN set.
                    if in_map.contains_key(dest) {
                        in_map.remove(dest);
                    }
                    defined.insert(ReachingDefinition {
                        variable: dest.to_owned(),
                        definition: instr.to_string(),
                    });
                }
                _ => (),
            }
        }
    });

    let in_minus_killed = in_map
        .into_iter()
        .map(|(var, assignment)| ReachingDefinition {
            variable: var.to_owned(),
            definition: assignment.to_owned(),
        })
        .collect::<HashSet<ReachingDefinition>>();

    let out = union(defined, in_minus_killed);
    out
}

pub fn init_entry(cfg: &ControlFlowGraph) -> DataflowValue {
    cfg.input_args
        .iter()
        .map(|arg| ReachingDefinition {
            variable: arg.name.to_string(),
            definition: "Φ".to_string(),
        })
        .collect()
}

impl<Meet, Transfer, InitEntry> DataflowAnalysis<DataflowValue, Meet, Transfer, InitEntry>
    for ControlFlowGraph
where
    Meet: Fn(Vec<DataflowValue>) -> DataflowValue,
    Transfer: Fn(&BasicBlock, DataflowValue) -> DataflowValue,
    InitEntry: FnOnce(&Self) -> DataflowValue,
{
    fn direction() -> Direction {
        Direction::Forward
    }
}

pub fn find_reaching_definitions(cfg: ControlFlowGraph) {
    let (in_sets, out_sets) = cfg.run_dataflow(meet, transfer, init_entry);

    for (i, bb) in cfg.nodes.into_iter().enumerate() {
        println!("{}", bb.name());

        print!("IN: ");
        in_sets[i].iter().for_each(|v| {
            print!(
                "{}[{}], ",
                v.variable,
                v.definition
                    .split('=')
                    .last()
                    .unwrap()
                    .trim()
                    .replace(';', "")
            );
        });
        println!();

        print!("OUT: ");
        out_sets[i].iter().for_each(|v| {
            print!(
                "{}[{}], ",
                v.variable,
                v.definition
                    .split('=')
                    .last()
                    .unwrap()
                    .trim()
                    .replace(';', "")
            );
        });
        println!("\n");
    }
}
