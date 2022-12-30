extern crate bril_rs;

use bril_rs::{Code, EffectOps, Function, Instruction, Argument};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

/// Controlflow graph representation of a bril function.
///
/// `nodes` - list of basic blocks that make up the control flow graph.
/// `bb_label_to_index_map` - bb label to the bb index in `nodes`.
/// `successors` - the list of successors of the basic block at `nodes[i]`, for i ∈ [0, nodes.len()-1]
/// `predecessors` - the list of predecessors of the basic block at `nodes[i]`, for i ∈ [0, nodes.len()-1]
#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub nodes: Vec<BasicBlock>,
    pub bb_label_to_index_map: HashMap<String, usize>,
    pub successors: Vec<Vec<usize>>,
    pub predecessors: Vec<Vec<usize>>,
    pub input_args: Vec<Argument>,
}

impl Display for ControlFlowGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "=== Basic Blocks ===")?;
        for bb in self.nodes.iter() {
            writeln!(f, "{bb}")?;
        }

        writeln!(f, "=== Predecessors ===")?;
        for (i, preds) in self.predecessors.iter().enumerate() {
            writeln!(f, "{} => {:?}", i, preds)?;
        }

        writeln!(f, "=== Successors ===")?;
        for (i, succs) in self.successors.iter().enumerate() {
            writeln!(f, "{} => {:?}", i, succs)?;
        }

        Ok(())
    }
}

impl From<&Function> for ControlFlowGraph {
    fn from(func: &Function) -> Self {
        let bbs = BasicBlock::form_blocks(func);
        let bb_label_to_index_map: HashMap<String, usize> = bbs
            .iter()
            .enumerate()
            .map(|(i, bb)| {
                (bb.name().to_owned(), i)
            })
            .collect();

        let mut predecessors = vec![vec![]; bbs.len()];
        let mut successors = vec![vec![]; bbs.len()];

        for (i, bb) in bbs.iter().enumerate() {
            if let Code::Label {
                label: curr_bb_label,
                ..
            } = &bb.instrs[0]
            {
                let last = bb
                    .instrs
                    .last()
                    .expect("Empty basic block should not have been possible!");
                match last {
                    // If last instruction is a jump update the
                    // successors of the current basic block and
                    // the predecessors of the target basic block.
                    Code::Instruction(Instruction::Effect { op, labels, .. })
                        if op == &EffectOps::Jump || op == &EffectOps::Branch =>
                    {
                        for label in labels {
                            let target_bb_label_index = bb_label_to_index_map[label];
                            let curr_bb_label_index = bb_label_to_index_map[curr_bb_label];
                            predecessors[target_bb_label_index].push(curr_bb_label_index);
                            successors[curr_bb_label_index].push(target_bb_label_index);
                        }
                    }
                    // Else the next basic block is going to be
                    // a successor of the current basic block
                    // as the "control" is going to fall through.
                    _ => {
                        if i < (bbs.len() - 1) {
                            predecessors[i + 1].push(i);
                            successors[i].push(i + 1);
                        }
                    }
                }
            } else {
                panic!("First instruction of the basic block was expected to be a label, at this point.");
            }
        }

        Self {
            nodes: bbs,
            bb_label_to_index_map,
            successors,
            predecessors,
            input_args: func.args.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub instrs: Vec<Code>,
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for instr in self.instrs.iter() {
            writeln!(f, "{instr}")?;
        }
        Ok(())
    }
}

impl BasicBlock {
    pub fn new(instrs: Vec<Code>) -> Self {
        Self { instrs }
    }

    pub fn name(&self) -> &str {
        if let Code::Label { label, .. } = &self.instrs[0] {
            &label
        } else {
            panic!("the first instruction of the basic block must be a `Label`.");
        }
    }

    pub fn last(&self) -> &Code {
        // Safe to unwrap here as we never construct empty
        // basic blocks.
        self.instrs.last().unwrap()
    }

    /// Derives a list of basic blocks from a Bril `Function`.
    /// Each basic block is assigned a label that is either
    /// generated or the existing label of the block in the
    /// concerned Bril function.
    pub fn form_blocks(func: &Function) -> Vec<BasicBlock> {
        let mut result = vec![];
        let mut sub = vec![];
        let mut label_number = 0;

        for instr in func.instrs.iter() {
            match instr {
                bril_rs::Code::Instruction(Instruction::Effect { op, .. })
                    if op == &EffectOps::Jump || op == &EffectOps::Branch =>
                {
                    sub.push(instr.clone());
                    result.push(BasicBlock::new(sub));
                    sub = vec![];
                }
                bril_rs::Code::Label { .. } => {
                    // If this is a label, we need to start a new basic block
                    if !sub.is_empty() {
                        result.push(BasicBlock::new(sub));
                    }
                    sub = vec![instr.clone()];
                }
                _ => {
                    if sub.is_empty() {
                        sub.push(bril_rs::Code::Label {
                            label: format!("bb{}", label_number),
                            pos: None,
                        });
                        label_number += 1;
                    }
                    sub.push(instr.clone())
                }
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

    /// Returns a set of variables in the basic block that are
    /// read before they are ever written to.
    ///
    /// This function exists based on the __ASSUMPTION__ that we're
    /// only dealing with valid bril programs and that if we're
    /// seeing a variable in the current basic block, that has not
    /// already been defined in the basic block, then the variable
    /// must have been defined in a previous basic block.
    ///
    /// An example of such a bril program can be seen below.
    /// ```bril
    /// (1) @main {
    /// (2)  x: int = const 4;
    /// (3)  jmp .label;
    /// (4).label:
    /// (5)  copy1: int = id x;
    /// (6)  print copy1;
    /// (7) }
    /// ```
    /// The above program consists of 2 basic blocks -the first basic
    /// block from line 2 to 3 and the second basic block from line 5 to 6.
    /// `x` is defined in basic block 1 and then read/used without being
    /// written to first in basic block 2.
    pub fn read_first(&self) -> HashSet<String> {
        let mut result = HashSet::new();
        let mut written_to = HashSet::new();

        for instr in &self.instrs {
            if let Code::Instruction(instr) = instr {
                match instr {
                    Instruction::Constant { dest, .. } => {
                        written_to.insert(dest.clone());
                    }
                    Instruction::Value { args, dest, .. } => {
                        let read_first: HashSet<String> = args
                            .iter()
                            .filter(|&arg| !written_to.contains(arg))
                            .cloned()
                            .collect();
                        result.extend(read_first);

                        written_to.insert(dest.clone());
                    }
                    Instruction::Effect { args, .. } => {
                        let read_first: HashSet<String> = args
                            .iter()
                            .filter(|&arg| !written_to.contains(arg))
                            .cloned()
                            .collect();
                        result.extend(read_first);
                    }
                }
            }
        }

        result
    }
}
