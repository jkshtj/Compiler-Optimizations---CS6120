extern crate bril_control_flow;
extern crate bril_rs;
extern crate dominance;
extern crate tracing;
extern crate tracing_subscriber;

use bril_control_flow::ControlFlowGraph;
use bril_rs::load_program_from_read;
use dominance::{
    find_dominance_frontiers, find_dominators, invert_dominators, print_dominance_frontiers,
    print_dominators, DominanceTree,
};
use std::fs::File;
use std::process::Command;
use tracing::{debug, Level};
use tracing_subscriber::FmtSubscriber;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 3, "Must provide the input filename, log level!");

    let level = match &args[1][..] {
        "-info" => Level::INFO,
        "-debug" => Level::DEBUG,
        "-trace" => Level::TRACE,
        _ => panic!("Invalid log level provided: {}", args[2]),
    };

    FmtSubscriber::builder().with_max_level(level).init();

    let path = &args[2];
    debug!("Opening file: {}", path);

    let file = File::open(path).unwrap();

    let output = Command::new("bril2json")
        .stdin(file)
        .output()
        .expect("failed to execute process");

    let mut program = load_program_from_read(&output.stdout[..]);

    for func in program.functions.iter_mut() {
        let cfg = ControlFlowGraph::from(&*func);
        print_dominators(&cfg);
        print_dominance_frontiers(&cfg);
        let dominance_tree: DominanceTree = cfg.into();
        println!("{dominance_tree}");
    }
}
