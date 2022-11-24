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
use tracing::{debug, error, info, Level};
use tracing_subscriber::FmtSubscriber;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    assert_eq!(
        args.len(),
        4,
        "Must provide the input filename, log level and whether or not to run the trivial dead code elimination pass!"
    );

    let level = match &args[1][..] {
        "-info" => Level::INFO,
        "-debug" => Level::DEBUG,
        "-trace" => Level::TRACE,
        _ => panic!("Invalid log level provided: {}", args[2]),
    };

    let run_dce = match &args[2][..] {
        "-dce" => true,
        _ => {
            error!("Invalid argument provided: {}. If you intend to run dead code elimination, you need to pass the `-dce` argument", args[2]);
            false
        }
    };

    FmtSubscriber::builder().with_max_level(level).init();

    let path = &args[3];
    debug!("Opening file: {}", path);

    let file = File::open(path).unwrap();

    let output = Command::new("bril2json")
        .stdin(file)
        .output()
        .expect("failed to execute process");

    let mut program = load_program_from_read(&output.stdout[..]);

    info!("Original({}) : {}", args[3], program);

    for func in program.functions.iter_mut() {
        run_lvn(func);
        if run_dce {
            run_tdce_pass(func);
        }
    }

    info!(
        "Optimized({}): {}\n==========================\n",
        args[3], program
    );
}
