//! Contains SSA utilities for Bril functions that allow us
//! to convert Bril functions to and from SSA form.

extern crate bril_control_flow;
extern crate bril_util;
extern crate bril_rs;
extern crate dominance;

use bril_control_flow::{ControlFlowGraph, BasicBlock};
use std::collections::{HashMap, HashSet};
use bril_rs::{Code, Instruction, ValueOps, Type};
use dominance::{find_dominance_frontiers, DominanceTree};

pub mod to_ssa {
    use super::*;

    pub fn to_ssa(mut cfg: ControlFlowGraph) -> ControlFlowGraph {
        let dominance_tree = cfg.clone().into();
        let mut stack = SSAVariableNameStack::new(&cfg);
        let mut bb_2_phi_nodes = HashMap::new();

        let bb_2_phi_node_vars = super::to_ssa::get_phis(&mut cfg);
        super::to_ssa::rename_variables(&mut cfg, 0, &dominance_tree, &mut stack, &bb_2_phi_node_vars, &mut bb_2_phi_nodes);
        super::to_ssa::insert_phis(&mut cfg, bb_2_phi_nodes);

        cfg
    }

    /// Returns the phi-nodes associated to each basic block in
    /// the control flow graph.
    ///
    /// Return type is a map of a basic block to the phi-nodes it contains.
    pub(crate) fn get_phis(cfg: &mut ControlFlowGraph) -> HashMap<usize, HashSet<String>> {
        let mut result = HashMap::new();
        let dominance_frontiers = find_dominance_frontiers(&cfg);
        let mut var_2_defining_blocks = get_var_2_defining_blocks_map(&cfg);

        for (var, defining_blocks) in var_2_defining_blocks.iter_mut() {
            let mut defining_blocks: Vec<usize> = defining_blocks
                .iter()
                .copied()
                .collect();

            let mut visited_defining_blocks = HashSet::new();

            while let Some(block) = defining_blocks.pop() {
                visited_defining_blocks.insert(block);

                for &df in dominance_frontiers[block].iter() {
                    // Add a ϕ-node to current block, unless we have done so already.
                    result
                        .entry(df)
                        .or_insert(HashSet::new())
                        .insert(var.to_owned());

                    // Add current block to defining blocks of `var` (because it now writes
                    // to `var`!), unless it's already in there.
                    if !visited_defining_blocks.contains(&df) {
                        defining_blocks.push(df);
                    }
                }
            }
        }

        result
    }

    /// Returns the following mapping -
    ///
    /// ```ignore
    /// var `v` in func represented by the CFG --> basic blocks defining `v`
    /// ```
    pub(crate) fn get_var_2_defining_blocks_map(cfg: &ControlFlowGraph) -> HashMap<String, HashSet<usize>> {
        let mut result = HashMap::new();

        for (i, block) in cfg.nodes.iter().enumerate() {
            for instr in block.instrs.iter() {
                if let Code::Instruction(instr) = instr {
                    match instr {
                        Instruction::Constant { dest, .. } |
                        Instruction::Value { dest, .. } => {
                            result
                                .entry(dest.clone())
                                .or_insert(HashSet::new())
                                .insert(i);
                        }
                        _ => ()
                    }
                }
            }
        }

        result
    }

    /// A type that maintains a per variable stack to provide
    /// access to the latest variable version at any point during
    /// the renaming portion of the to-SSA conversion.
    #[derive(Debug, Clone)]
    pub(crate) struct SSAVariableNameStack {
        per_variable_stack: HashMap<String, Vec<String>>,
        per_variable_version: HashMap<String, usize>,
    }

    impl SSAVariableNameStack {
        pub fn new(cfg: &ControlFlowGraph) -> Self {
            let per_variable_stack = Self::get_per_variable_name_stack(cfg);
            let per_variable_version = per_variable_stack
                .keys()
                .map(|k| (k.to_owned(), 0))
                .collect();

            Self {
                per_variable_stack,
                per_variable_version,
            }
        }

        /// Returns a set of stacks, where each stack corresponds to a
        /// single variable in the control flow graph.
        fn get_per_variable_name_stack(cfg: &ControlFlowGraph) -> HashMap<String, Vec<String>> {
            let mut result = HashMap::new();

            for input_arg in cfg.input_args.iter() {
                result
                    .entry(input_arg.name.clone())
                    .or_insert(vec![input_arg.name.clone()]);
            }

            for bb in cfg.nodes.iter() {
                for instr in bb.instrs.iter() {
                    if let Code::Instruction(instr) = instr {
                        match instr {
                            Instruction::Constant { dest, .. } |
                            Instruction::Value { dest, .. } => {
                                if !result.contains_key(dest) {
                                    result.insert(dest.clone(), vec![]);
                                }
                            }
                            _ => ()
                        }
                    }
                }
            }

            result
        }

        pub fn generate_and_push_fresh_name(&mut self, var: &str) -> String {
            let fresh_name = self.generate_fresh_name_for_var(var);
            self.push_fresh_var_name_on_stack(var, fresh_name.clone());
            fresh_name
        }

        pub fn generate_fresh_name_for_var(&mut self, var: &str) -> String {
            if let Some(version) = self.per_variable_version.get_mut(var) {
                *version += 1;
                format!("{}.{}", var, *version-1)
            } else {
                panic!("Variable with name {} is undefined!", var);
            }
        }

        pub fn push_fresh_var_name_on_stack(&mut self, var: &str, fresh_name: String) {
            if let Some(stack) = self.per_variable_stack.get_mut(var) {
                stack.push(fresh_name);
            } else {
                panic!("Variable with name {} is undefined!", var);
            }
        }

        pub fn top<'a>(&'a mut self, var: &str) -> &'a str {
            if let Some(stack) = self.per_variable_stack.get(var) {
                if let Some(top) = stack.last() {
                    top
                } else {
                    "__undefined"
                }
            } else {
                panic!("Variable with name {} is undefined!", var);
            }
        }
    }

    /// A custom utility type to represent a bril phi
    /// node. Easier to use than using the Phi instruction
    /// for our purposes.
    #[derive(Debug)]
    pub(crate) struct PhiNode {
        dest: Option<String>,
        args: Vec<String>,
        labels: Vec<String>,
    }

    impl PhiNode {
        pub fn new() -> Self {
            PhiNode {
                dest: None,
                labels: vec![],
                args: vec![],
            }
        }

        pub fn set_dest(&mut self, dest: String) {
            self.dest.replace(dest);
        }

        pub fn add_arg(&mut self, arg: String) {
            self.args.push(arg);
        }

        pub fn add_label(&mut self, label: String) {
            self.labels.push(label);
        }
    }

    /// Goes over each instruction in the control flow graph
    /// and renames variable declarations and definitions to
    /// convert the program into SSA form.
    pub(crate) fn rename_variables(
        cfg: &mut ControlFlowGraph,
        bb: usize,
        dominance_tree: &DominanceTree,
        stack: &mut SSAVariableNameStack,
        bb_2_phi_node_vars: &HashMap<usize, HashSet<String>>,
        bb_2_phi_nodes: &mut HashMap<usize, HashMap<String, PhiNode>>,
    ) {
        // Save stacks
        let old_stack = stack.per_variable_stack.clone();

        // Rename phi node destinations
        //
        // The destinations for phi nodes of the current basic block must be set
        // in the `rename` call for this basic block. Setting the destination
        // of a phi node earlier - for instance setting it when we reach into the
        // basic block after renaming one of its predecessors in the CFG, will lead to
        // the phi node's destination becoming incorrectly "live", i.e., any other
        // siblings(other children of this bb's parent) of this basic block will
        // use the phi node's destination as the latest name of the underlying
        // variable.
        if let Some(var_2_phi_node_map) = bb_2_phi_nodes.get_mut(&bb) {
            for (var, phi_node) in var_2_phi_node_map {
                let fresh_name = stack.generate_and_push_fresh_name(var);
                phi_node.set_dest(fresh_name);
            }
        }

        // Rename arguments and destinations in normal instructions
        for instr in cfg.nodes[bb].instrs.iter_mut() {
            if let Code::Instruction(instr) = instr {
                match instr {
                    Instruction::Constant { dest, .. } => {
                        let fresh_name = stack.generate_fresh_name_for_var(dest);
                        stack.push_fresh_var_name_on_stack(dest, fresh_name.clone());
                        *dest = fresh_name;
                    }
                    Instruction::Value { args, dest, .. } => {
                        for arg in args.iter_mut() {
                            *arg = stack.top(arg).to_owned();
                        }

                        let fresh_name = stack.generate_fresh_name_for_var(dest);
                        stack.push_fresh_var_name_on_stack(dest, fresh_name.clone());
                        *dest = fresh_name;
                    }
                    Instruction::Effect { args, .. } => {
                        for arg in args.iter_mut() {
                            *arg = stack.top(arg).to_owned();
                        }
                    }
                }
            }
        }

        // Rename phi-node arguments (in successors)
        for succ in cfg.successors[bb].iter() {
            if let Some(phi_node_vars) = bb_2_phi_node_vars.get(succ) {
                for var in phi_node_vars {
                    // Check whether there we have already started constructing
                    // phi nodes for this block, if not, let's start now.
                    // Then check whether we have already started a phi node for
                    // variable `var` in this block, if not, let's start now.
                    let phi_node_for_var_in_succ_bb = bb_2_phi_nodes
                        .entry(*succ)
                        .or_insert(HashMap::new())
                        .entry(var.to_owned())
                        .or_insert(PhiNode::new());

                    // Make the phi node read the version of `var` at `var`'s
                    // stack top.
                    phi_node_for_var_in_succ_bb.add_arg(stack.top(var).to_owned()
                    );
                    // To the phi node, add the basic block label from where the
                    // latest version of `var` is coming.
                    phi_node_for_var_in_succ_bb.add_label(cfg.nodes[bb].name().to_owned());
                }
            }
        }

        // Recursive rename calls for immediately dominated blocks
        for &child in dominance_tree.immediately_dominated_by_me[bb].iter() {
            rename_variables(cfg, child, dominance_tree, stack, bb_2_phi_node_vars, bb_2_phi_nodes);
        }

        // Restore stacks
        stack.per_variable_stack = old_stack;
    }

    pub(crate) fn insert_phis(
        cfg: &mut ControlFlowGraph,
        mut bb_2_phi_nodes: HashMap<usize, HashMap<String, PhiNode>>,
    ) {
        for (i, bb) in cfg.nodes.iter_mut().enumerate() {
            if let Some(phi_nodes_in_block) = bb_2_phi_nodes.remove(&i) {
                for (_, mut phi_node) in phi_nodes_in_block {
                    if phi_node.labels.len() > 1 {
                        bb.instrs.insert(1, Code::Instruction(Instruction::Value {
                            args: phi_node.args,
                            dest: phi_node.dest.take().expect("Phi node destination must have been set by now!"),
                            funcs: vec![],
                            labels: phi_node.labels,
                            op: ValueOps::Phi,
                            pos: None,
                            op_type: Type::Int
                        }))
                    }
                }
            }
        }
    }
}

mod from_ssa {
    use super::*;
    use bril_rs::EffectOps;

    pub fn from_ssa(mut cfg: ControlFlowGraph) -> ControlFlowGraph {
        let phi_predecessor_blocks = get_phi_predecessor_blocks(&mut cfg);
        insert_copies_into_phi_predecessor_blocks(phi_predecessor_blocks, &mut cfg);
        cfg
    }

    fn insert_copies_into_phi_predecessor_blocks(phi_predecessor_blocks: HashMap<usize, Vec<PhiNodeCopy>>, cfg: &mut ControlFlowGraph) {
        // Insert copies at the end of the blocks that
        // comprised the values of the phi-nodes in this
        // block.
        for (bb_index, phi_node_copies) in phi_predecessor_blocks {
            for (dest, src) in phi_node_copies {
                if let Some(Code::Instruction(Instruction::Effect {op, ..})) = cfg.nodes[bb_index].instrs.last() {
                    if *op == EffectOps::Branch || *op == EffectOps::Jump {
                        let index = cfg.nodes[bb_index].instrs.len() - 1;
                        cfg.nodes[bb_index].instrs.insert(index, Code::Instruction(Instruction::Value {
                            args: vec![src],
                            dest,
                            funcs: vec![],
                            labels: vec![],
                            op: ValueOps::Id,
                            pos: None,
                            op_type: Type::Int
                        }));
                    }
                } else {
                    cfg.nodes[bb_index].instrs.push(Code::Instruction(Instruction::Value {
                        args: vec![src],
                        dest,
                        funcs: vec![],
                        labels: vec![],
                        op: ValueOps::Id,
                        pos: None,
                        op_type: Type::Int
                    }));
                }
            }
        }
    }

    type PhiNodeCopy = (/* Destination */ String, /* Source */ String);
    fn get_phi_predecessor_blocks(cfg: &mut ControlFlowGraph) -> HashMap<usize, Vec<PhiNodeCopy>> {
        let mut phi_predecessor_blocks: HashMap<usize, Vec<(String, String)>> = HashMap::new();

        for bb in cfg.nodes.iter_mut() {
            // Collect all blocks at the end of which a copy
            // needs to be inserted.
            for instr in bb.instrs.iter() {
                if let Code::Instruction(Instruction::Value { dest, op, args, labels, .. }) = instr {
                    if *op == ValueOps::Phi {
                        args.iter()
                            .zip(labels)
                            .for_each(|(src, bb_label)| {
                                // Ignore copies that will contribute an
                                // undefined value to corresponding phi node.
                                if src != "__undefined" {
                                    phi_predecessor_blocks
                                        .entry(cfg.bb_label_to_index_map[bb_label])
                                        .or_default()
                                        .push((dest.clone(), src.clone()));
                                }
                            });
                    }
                }
            }

            // Remove all phi instructions from this block.
            bb.instrs.retain(|instr| if let Code::Instruction(Instruction::Value { op, ..}) = instr {
                *op != ValueOps::Phi
            } else {
                true
            });
        }

        phi_predecessor_blocks
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use bril_util::{construct_control_flow_graph_from_ir_file, construct_control_flow_graph_from_ir_string};
    use dominance::{print_dominators, find_dominators, invert_dominators, print_dominance_frontiers, DominanceTree};
    use to_ssa::{SSAVariableNameStack, PhiNode};

    #[test]
    fn var_2_defining_blocks_map() {
        let ir = "@main(cond: bool) {
.entry:
    a: int = const 47;
    br cond .left .right;
.left:
    a: int = add a a;
    jmp .exit;
.right:
    a: int = mul a a;
    jmp .exit;
.exit:
    print a;
}";
        let cfg = construct_control_flow_graph_from_ir_string(ir).pop().unwrap();
        let expected = {
            let mut expected = HashMap::new();
            expected.insert("a".to_owned(), {
               let mut blocks = HashSet::new();
                blocks.insert(0 as usize);
                blocks.insert(1);
                blocks.insert(2);
                blocks
            });
            expected
        };
        let actual = super::to_ssa::get_var_2_defining_blocks_map(&cfg);
        println!("{:?}", actual);
        assert_eq!(expected, actual)
    }

    #[test]
    fn phi_nodes() {
        let path = "../../examples/test/ssa/if-orig.bril";
        for mut cfg in construct_control_flow_graph_from_ir_file(path) {
            let phis = super::to_ssa::get_phis(&mut cfg);
            println!("{:?}", phis);
        }
    }

    #[test]
    fn rename_variables() {
        let path = "../../examples/test/to_ssa/if.bril";
        for mut cfg in construct_control_flow_graph_from_ir_file(path) {
            let dominance_tree: DominanceTree = cfg.clone().into();
            let mut stack = SSAVariableNameStack::new(&cfg);
            let bb_2_phi_node_vars = super::to_ssa::get_phis(&mut cfg);
            let mut bb_2_phi_nodes: HashMap<usize, HashMap<String, PhiNode>> = HashMap::new();

            super::to_ssa::rename_variables(&mut cfg, 0, &dominance_tree, &mut stack, &bb_2_phi_node_vars, &mut bb_2_phi_nodes);
            super::to_ssa::insert_phis(&mut cfg, bb_2_phi_nodes);
            println!("{}", cfg);
        }
    }

    #[test]
    fn to_ssa() {
        let non_ssa_ir_file_path = "../../examples/test/to_ssa/while.bril";
        let non_ssa_cfgs = construct_control_flow_graph_from_ir_file(non_ssa_ir_file_path);

        for  non_ssa_cfg in non_ssa_cfgs {
            let to_ssa_cfg = super::to_ssa::to_ssa(non_ssa_cfg);
            println!("To SSA CFG: {}.", to_ssa_cfg);
        }
    }

    #[test]
    fn from_ssa() {
        let non_ssa_ir_file_path = "../../examples/test/ssa_roundtrip/while.bril";
        let non_ssa_cfgs = construct_control_flow_graph_from_ir_file(non_ssa_ir_file_path);

        for non_ssa_cfg in non_ssa_cfgs {
            let to_ssa_cfg = super::to_ssa::to_ssa(non_ssa_cfg);
            let from_ssa_cfg = super::from_ssa::from_ssa(to_ssa_cfg);
            println!("From SSA CFG: {}.", from_ssa_cfg);
        }
    }
}