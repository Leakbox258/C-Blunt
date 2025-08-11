use std::rc::Rc;

use crate::{pass::CFGflatten::CFGflatten, scf::operation::Operation, visitor::visitor::Shared};

pub trait Pass {
    fn get_pass_name(&self) -> String;
    fn run(&mut self);

    // add default anaylis passes
}

pub trait PassFactory {
    type PassType;

    fn new(module: Shared<Operation>) -> Box<Self::PassType>
    where
        Self: Sized;
}

pub struct OptInfo {
    pub cfgflatten: bool,
}

impl OptInfo {
    pub fn new() -> OptInfo {
        OptInfo { cfgflatten: true }
    }
}

pub struct PassManager {
    module: Shared<Operation>,
    passes: Vec<Box<dyn Pass>>,
    ctl: OptInfo,
}

impl PassManager {
    pub fn new(module: &Shared<Operation>, opts: OptInfo) -> PassManager {
        let mut passes: Vec<Box<dyn Pass>> = Vec::new();

        if opts.cfgflatten {
            passes.push(CFGflatten::new(Rc::clone(&module)));
        }
        // TODO: more pass

        PassManager {
            module: Rc::clone(module),
            passes: passes,
            ctl: opts,
        }
    }

    pub fn run(&mut self) {
        for pass in &mut self.passes {
            pass.run();
        }
    }
}
