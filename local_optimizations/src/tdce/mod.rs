//! This module contains utilities to implement trivial dead code and dead store
//! elimination optimization on bril programs.

extern crate bril_control_flow;
extern crate bril_rs;
extern crate tracing;

use self::bril_control_flow::BasicBlock;
use bril_rs::{Function, Instruction};
use std::collections::{HashMap, HashSet};
use tracing::debug;

/// A global optimization that deletes unused
/// instructions across basic blocks.
///
/// The algorithm used in this optimization is
/// fairly simple. In a single "pass" of this optimization
/// we iterate over all instructions and store the variables
/// that are GEN-ed (as in, used in a given instruction) in a
/// set called "Used". We then iterate over all the instructions
/// again, this time reacting only when we are looking at
/// instructions that are KILLing (as in, defining) a variable -- we
/// check if the variable being KILLed does not exist in the "Used" set,
/// and if it doesn't, we delete the instruction.
pub fn dead_code_elimination_pass(func: &mut Function) -> bool {
    let mut changed = false;

    let mut used = HashSet::new();

    // Collect used variables
    for instr in func.instrs.iter() {
        if let bril_rs::Code::Instruction(instr) = instr {
            match instr {
                Instruction::Value { args, .. } | Instruction::Effect { args, .. } => {
                    args.iter().for_each(|arg| {
                        used.insert(arg.clone());
                    })
                }
                _ => debug!(
                    "Skipping instr: [{}], as it does not have any GEN-ed variables.",
                    instr
                ),
            }
        }
    }

    // Delete unused instructions
    let mut to_delete = HashSet::new();
    for (i, instr) in func.instrs.iter().enumerate() {
        if let bril_rs::Code::Instruction(instr) = instr {
            match instr {
                Instruction::Value { dest, .. } | Instruction::Constant { dest, .. } => {
                    if !used.contains(dest) {
                        to_delete.insert(i);
                        changed = true;
                    }
                }
                _ => debug!(
                    "Skipping instr: [{}], as it does not have any KILL-ed variables.",
                    instr
                ),
            }
        }
    }

    func.instrs = func
        .instrs
        .iter()
        .enumerate()
        .filter(|(i, _)| !to_delete.contains(i))
        .map(|(_, instr)| instr.clone())
        .collect();

    changed
}

/// Since the deletion of an instruction can then open up the
/// possibility of other unused variables, we run
/// `dead_code_elimination_pass` until convergence.
pub fn dead_code_elimination(func: &mut Function) {
    while dead_code_elimination_pass(func) {
        continue;
    }
}

/// A local optimization that deletes dead stores
/// to variables in basic blocks.
///
/// In a single pass through a given basic block, we keep track
/// of the last seen definitions of variables using a map of
/// variable name to the index of the instruction in the basic
/// block. Let's assume this map is called `last_def`.
///
/// For each instruction in the basic block we do 2 things -
/// 1. We check if any of the GEN-ed variables in the current instruction
/// have their last definition already present in the `last_def`. If yes,
/// then these variables did not get a dead store and we can delete them
/// from the `last_def` map.
/// 2. We check if the variable KILL-ed in the current instruction
/// already has a last seen definition in the `last_def` map. If yes,
/// we mark the instruction associated to the current last definition of
/// this variable for deletion. Finally, we update the variable's last
/// seen definition.
///
/// When we reach the end of the basic block, we let the instructions
/// corresponding to the remaining entries in the `last_def` map be,
/// because these variable might be used in some other part/basic block
/// associated to the function.
pub fn dead_store_elimination_pass(bb: &mut BasicBlock) -> bool {
    let mut last_def = HashMap::new();
    let mut to_delete = HashSet::new();
    let mut changed = false;

    for (i, instr) in bb.instrs.iter().enumerate() {
        // Check for use of a variable
        if let bril_rs::Code::Instruction(instr) = instr {
            match instr {
                Instruction::Value { args, .. } | Instruction::Effect { args, .. } => {
                    args.iter().for_each(|arg| {
                        last_def.remove(arg);
                    })
                }
                _ => debug!(
                    "Skipping instr: [{}], as it does not have any GEN-ed variables.",
                    instr
                ),
            }
        }

        // Then check for definition of a variable
        if let bril_rs::Code::Instruction(instr) = instr {
            match instr {
                Instruction::Value { dest, .. } | Instruction::Constant { dest, .. } => {
                    if last_def.contains_key(dest) {
                        to_delete.insert(last_def[dest]);
                        changed = true;
                    }

                    last_def.entry(dest).and_modify(|v| *v = i).or_insert(i);
                }
                _ => debug!(
                    "Skipping instr: [{}], as it does not have any KILL-ed variables.",
                    instr
                ),
            }
        }
    }

    bb.instrs = bb
        .instrs
        .iter()
        .enumerate()
        .filter(|(i, _)| !to_delete.contains(i))
        .map(|(_, instr)| instr.clone())
        .collect();

    changed
}

/// Since the deletion of an instruction can then open up the
/// possibility of new dead stores to variables, we run
/// `dead_store_elimination_pass` until convergence.
pub fn dead_store_elimination(func: &mut Function) -> bool {
    let mut changed = false;
    let mut bbs = BasicBlock::to_basic_blocks(func);

    for bb in bbs.iter_mut() {
        changed |= dead_store_elimination_pass(bb);
    }

    if changed {
        func.instrs = BasicBlock::flatten(bbs);
    }

    changed
}

pub fn run_tdce_pass(func: &mut Function) {
    loop {
        dead_code_elimination(func);
        if !dead_store_elimination(func) {
            break;
        }
    }
}
