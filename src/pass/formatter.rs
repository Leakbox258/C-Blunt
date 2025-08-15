use std::rc::Rc;

use crate::{
    pass::manager::{Pass, PassFactory},
    scf::{
        attr::Attr,
        r#macro::*,
        no_wrap::{BlockTrait, OperationTrait, RegionTrait},
        operation::{OpType, Operation},
        region::Region,
    },
    visitor::visitor::Shared,
};

// mov alloc / getarg / ... at the top of the entry block
pub struct Formatter {
    module: Shared<Operation>,
}

impl Pass for Formatter {
    fn run(&mut self) {
        self.r#impl();
    }
    fn get_pass_name(&self) -> String {
        "formatter".to_string()
    }
}

impl PassFactory for Formatter {
    type PassType = Formatter;
    fn new(module: Shared<Operation>) -> Box<Self::PassType>
    where
        Self: Sized,
    {
        Box::new(Formatter {
            module: Rc::clone(&module),
        })
    }
}

impl Formatter {
    pub fn r#impl(&mut self) {
        for func in get_fns!(self.module) {
            self.run_on_func(func);
        }
    }

    pub fn run_on_func(&mut self, func: Shared<Operation>) {
        // self.lift_alloca(&func);
        self.lift_get_arg(&func);
    }

    pub fn lift_alloca(&mut self, func: &Shared<Operation>) {
        let region = func.get_default_region();

        fn run_on_region(region: &Shared<Region>) -> Vec<Shared<Operation>> {
            let mut entry = Vec::new();

            for block in region.get_blocks_as_mut().iter_mut() {
                let mut del = Vec::new();

                for op in block.get_ops().iter() {
                    if optype_checkif!(op, OpType::Alloca) {
                        del.push(Rc::clone(&op));
                        entry.push(Rc::clone(op));
                        continue;
                    }

                    if !op.get_default_region().is_empty() {
                        for region in op.get_regions().iter() {
                            entry.append(&mut run_on_region(region));
                        }
                    }
                }

                let _ = del.into_iter().for_each(|op| block.del_op(&op));
            }

            entry.sort_by(|op1, op2| op1.get_id().cmp(&op2.get_id()));
            entry
        }

        let entry = run_on_region(&region);

        let entry_block = region.get_entry_block_as_mut();
        let mut entry_ops = entry_block.get_ops_as_mut();
        entry_ops.splice(0..0, entry);
    }

    pub fn lift_get_arg(&mut self, func: &Shared<Operation>) {
        let region = func.get_default_region();

        fn run_on_region(region: &Shared<Region>) -> Vec<Shared<Operation>> {
            let mut entry = Vec::new();

            for block in region.get_blocks_as_mut().iter_mut() {
                let mut del = Vec::new();

                for op in block.get_ops().iter() {
                    if optype_checkif!(op, OpType::GetArg) {
                        del.push(Rc::clone(&op));
                        entry.push(Rc::clone(op));
                        continue;
                    }

                    if !op.get_default_region().is_empty() {
                        for region in op.get_regions().iter() {
                            entry.append(&mut run_on_region(region));
                        }
                    }
                }

                let _ = del.into_iter().for_each(|op| block.del_op(&op));
            }

            entry.sort_by(|op1, op2| get_seq!(op1).unwrap().cmp(&get_seq!(op2).unwrap()));
            entry
        }

        let entry = run_on_region(&region);

        let entry_block = region.get_entry_block_as_mut();
        let mut entry_ops = entry_block.get_ops_as_mut();
        entry_ops.splice(0..0, entry);
    }
}
