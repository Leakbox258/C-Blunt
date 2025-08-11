mod parser;
mod pass;
mod scf;
mod utils;
mod visitor;
use parser::sysy;

use std::env;
use std::fs;
use std::process::exit;

use crate::pass::manager::OptInfo;
use crate::pass::manager::Pass;
use crate::pass::manager::PassManager;
use crate::scf::Print;
use crate::visitor::visitor::Visitor;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_path = None;
    let mut output_path = Some(String::from("a.out"));
    let mut _s: bool = false;

    let mut opts = OptInfo::new();

    for i in 0..args.len() {
        let arg_str = args[i].as_str();

        match arg_str {
            "-S" => _s = true,
            "-o" => {
                output_path = None;
            }
            _ => {
                let path = args[i].clone();

                if path.as_bytes()[0] == b'-' {
                    eprintln!("unknown param: {}", path);
                    exit(1);
                }

                if output_path.is_none() {
                    output_path = Some(path);
                } else {
                    input_path = Some(path);
                }
            }
        }
    }

    let input = fs::read_to_string(input_path.unwrap()).expect("cant find input file");
    let program = sysy::ProgramParser::new()
        .parse(&input)
        .expect("frontend parse failed");

    let mut mlir_gen = visitor::visitor::MLIRGen::new();
    mlir_gen.visit_compunit(&program.compunit);
    let module = mlir_gen.get_module();

    let mut pm = PassManager::new(&module, opts);
    pm.run();

    print!("{}", module.borrow().print(0));
}
