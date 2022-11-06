extern crate bril_rs;

use bril_rs::{Function, Code, Instruction, EffectOps};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub instrs: Vec<Code>,
}

impl BasicBlock {
    pub fn new(instrs: Vec<Code>) -> Self {
        Self {
            instrs,
        }
    }

    pub fn to_basic_blocks(func: &Function) -> Vec<BasicBlock> {
        let mut result = vec![];
        let mut sub = vec![];

        for instr in func.instrs.iter() {
            match instr {
                bril_rs::Code::Instruction(Instruction::Effect { op, .. }) => match op {
                    EffectOps::Jump |
                    EffectOps::Branch |
                    EffectOps::Return => {
                        sub.push(instr.clone());
                        result.push(BasicBlock::new(sub));
                        sub= vec![];
                    }
                    _ => sub.push(instr.clone()),
                },
                bril_rs::Code::Label{ .. } => {
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

    pub fn coalesce_basic_blocks(bbs: Vec<BasicBlock>) -> Vec<Code> {
        bbs.into_iter().flat_map(|bb| bb.instrs).collect()
    }
}