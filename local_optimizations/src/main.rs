mod lvn;
mod tdce;

extern crate bril_control_flow;
extern crate bril_rs;
extern crate tracing;
extern crate tracing_subscriber;

use bril_rs::load_program_from_read;
use lvn::run_lvn;
use std::fs::File;
use std::process::Command;
use tdce::run_tdce_pass;
use tracing::{debug, info, Level};
use tracing_subscriber::FmtSubscriber;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    assert_eq!(
        args.len(),
        3,
        "Must provide the input filename and log level!"
    );

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

    info!("Original({}) : {}", args[2], program);

    for func in program.functions.iter_mut() {
        run_lvn(func);
        run_tdce_pass(func);
    }

    info!(
        "Optimized({}): {}\n==========================\n",
        args[2], program
    );
}
