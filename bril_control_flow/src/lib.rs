extern crate bril_rs;

use bril_rs::{Code, EffectOps, Function, Instruction};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub instrs: Vec<Code>,
}

impl BasicBlock {
    pub fn new(instrs: Vec<Code>) -> Self {
        Self { instrs }
    }

    pub fn to_basic_blocks(func: &Function) -> Vec<BasicBlock> {
        let mut result = vec![];
        let mut sub = vec![];

        for instr in func.instrs.iter() {
            match instr {
                bril_rs::Code::Instruction(Instruction::Effect { op, .. }) => match op {
                    EffectOps::Jump | EffectOps::Branch | EffectOps::Return => {
                        sub.push(instr.clone());
                        result.push(BasicBlock::new(sub));
                        sub = vec![];
                    }
                    _ => sub.push(instr.clone()),
                },
                bril_rs::Code::Label { .. } => {
                    // If this is a label, we need to start a new basic block
                    if !sub.is_empty() {
                        result.push(BasicBlock::new(sub));
                    }
                    sub = vec![];
                }
                _ => sub.push(instr.clone()),
            }
        }

        if !sub.is_empty() {
            result.push(BasicBlock::new(sub));
        }

        result
    }

    pub fn flatten(bbs: Vec<BasicBlock>) -> Vec<Code> {
        bbs.into_iter().flat_map(|bb| bb.instrs).collect()
    }

    /// For each instruction in the basic block, returns
    /// information regarding whether or not the destination
    /// variable in the instruction, if there is one, is updated
    /// at a later point in the basic block. If the instruction
    /// does not have a destination variable, simply returns false.
    pub fn last_writes(&self) -> Vec<bool> {
        let mut result = vec![true; self.instrs.len()];
        let mut seen = HashSet::new();

        for (i, instr) in self.instrs.iter().rev().enumerate() {
            let updated_later = match instr {
                Code::Instruction(instr) => match instr {
                    Instruction::Constant { dest, .. } | Instruction::Value { dest, .. } => {
                        if seen.contains(dest) {
                            false
                        } else {
                            seen.insert(dest);
                            true
                        }
                    }
                    _ => true,
                },
                _ => true,
            };
            result[self.instrs.len() - i - 1] = updated_later;
        }

        result
    }
}
