//! Contains bril utilities commonly used across different
//! bril libraries for control flow, data flow, optimizations etc.

extern crate bril_control_flow;
extern crate tracing;
extern crate bril_rs;

use std::path::Path;
use std::ops::ControlFlow;
use bril_control_flow::ControlFlowGraph;
use tracing::debug;
use std::fs::{File, OpenOptions};
use std::process::Command;
use bril_rs::load_program_from_read;
use std::io::{Write, Read};

pub fn construct_control_flow_graph_from_ir_file(path: impl AsRef<str>) -> Vec<ControlFlowGraph> {
    debug!("Opening file: {}", path.as_ref());

    let file = File::open(path.as_ref()).unwrap();

    let output = Command::new("bril2json")
        .stdin(file)
        .output()
        .expect("failed to execute bril2json. conversion from bril IR to bril JSON failed.");

    let mut program = load_program_from_read(&output.stdout[..]);

    let mut result = vec![];

    for func in program.functions.iter_mut() {
        debug!("Generating control flow graph for bril function: {}", func.name);
        result.push(ControlFlowGraph::from(&*func));
        debug!("Finished generating control flow graph for bril function: {}", func.name);
    }

    result
}

pub fn construct_control_flow_graph_from_ir_string(ir: impl AsRef<str>) -> Vec<ControlFlowGraph> {
    debug!("Received IR: {}", ir.as_ref());

    let path = "/tmp/bril_ssa";
    let mut output = File::create(path).unwrap();
    write!(output, "{}", ir.as_ref()).unwrap();
    let input = File::open(path).unwrap();

    let output = Command::new("bril2json")
        .stdin(input)
        .output()
        .expect("failed to execute bril2json. conversion from bril IR to bril JSON failed.");

    let mut program = load_program_from_read(&output.stdout[..]);

    let mut result = vec![];

    for func in program.functions.iter_mut() {
        debug!("Generating control flow graph for bril function: {}", func.name);
        result.push(ControlFlowGraph::from(&*func));
        debug!("Finished generating control flow graph for bril function: {}", func.name);
    }

    result
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Read;

    #[test]
    fn test_ir_str_to_cfg() {
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
        let result = construct_control_flow_graph_from_ir_string(ir);
        println!("{:?}", result);
    }
}