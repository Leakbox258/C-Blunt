mod interpreter;
mod parser;
mod pass;
mod scf;
mod utils;
mod visitor;

use parser::sysy;
use std::env;
use std::process::exit;

use crate::interpreter::interpreter::Interpreter;
use crate::pass::manager::OptInfo;
use crate::pass::manager::PassManager;
use crate::scf::no_wrap::OperationTrait;
use crate::visitor::visitor::Visitor;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_path = None;
    let mut output_path = Some(String::from("a.out"));
    let mut mlir: bool = false;
    let mut llvm_ir: bool = false;
    let mut asm: bool = false;

    let mut opts = OptInfo::new();

    for i in 0..args.len() {
        let arg_str = args[i].as_str();

        match arg_str {
            "-S" => asm = true,
            "-emit-llvm" => llvm_ir = true,
            "-emit-mlir" => mlir = true, // no CFGflatten
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

    // frontend
    let input = std::fs::read_to_string(input_path.unwrap()).expect("cant find input file");
    let program = sysy::ProgramParser::new()
        .parse(&input)
        .expect("frontend parse failed");

    let mut mlir_gen = visitor::visitor::MLIRGen::new();
    mlir_gen.visit_compunit(&program.compunit);
    let module = mlir_gen.get_module();

    // passes
    let mut pm = PassManager::new(&module, opts);
    pm.run();

    // output
    let mut io_handle: Box<dyn std::io::Write> = if output_path.is_none() {
        Box::new(std::io::stdout()) as Box<dyn std::io::Write>
    } else {
        let path = std::path::Path::new(output_path.as_ref().unwrap());

        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }

        Box::new(std::fs::File::create(path).unwrap()) as Box<dyn std::io::Write>
    };

    if mlir {
        let _ = io_handle.write(module.print(0).as_bytes());
    } else if llvm_ir {
        let _ = Interpreter::interpret(module, io_handle);
    } else if asm {
        todo!("AsmGen not impl");
    }
}
