use crate::pass::manager::{Pass, PassFactory};
use crate::scf::attr::Attr;
use crate::scf::block::Block;
use crate::scf::r#macro::*;
use crate::scf::no_wrap::*;
use crate::scf::operation::OpType;
use crate::scf::operation::Operation;
use crate::scf::value::Type;
use crate::visitor::visitor::{Builder, Shared};
use std::rc::Rc;

pub struct CFGflatten {
    module: Shared<Operation>,
    reorder_id: u64,

    // after flatten, each region (besides func) have only one block as entry
    builder: Builder,
}

impl Pass for CFGflatten {
    fn get_pass_name(&self) -> String {
        "CFGflatten".to_string()
    }

    fn run(&mut self) {
        self.r#impl();
    }
}

impl PassFactory for CFGflatten {
    type PassType = CFGflatten;

    fn new(module: Shared<Operation>) -> Box<Self::PassType>
    where
        Self: Sized,
    {
        let builder = Builder::new();

        Box::new(CFGflatten {
            module: Rc::clone(&module),
            reorder_id: 0,
            builder: builder,
        })
    }
}

impl CFGflatten {
    fn next_id(&mut self) -> u64 {
        let id = self.reorder_id;
        self.reorder_id += 1;
        id
    }

    fn new_op(&mut self, ty: &Type, opty: &OpType) -> Shared<Operation> {
        let id = self.next_id();
        let rid = id.clone(); // rid use as the id of the default region of this op
        self.builder.new_op(id, ty.clone(), opty.clone(), rid)
    }

    fn new_block(&mut self) -> Shared<Block> {
        let bid = self.next_id();
        self.builder.new_block(bid)
    }

    fn add_operation(&mut self, operation: &Shared<Operation>) {
        self.builder.parent_block_mut().add_op(operation);
    }

    fn r#impl(&mut self) {
        for func in get_fns!(self.module) {
            self.run_on_fn(func);
        }
    }

    fn run_on_fn(&mut self, mut func: Shared<Operation>) {
        func.set_id(self.next_id());

        self.builder.push_op(&func);
        self.builder.push_region(&func.get_default_region());

        let entry_block = self.new_block();

        self.builder.push_block(&entry_block);

        let mut blocks = self.run_on_entry(
            func.get_default_region().get_entry_block(),
            entry_block,
            None,
            None,
            None,
        );

        func.get_default_region().replace_block(&mut blocks);

        self.builder.pop_block();
        self.builder.pop_region();
        self.builder.pop_op();
    }

    fn run_on_entry(
        &mut self,
        ori_block: Shared<Block>,
        entry_block: Shared<Block>,
        cond_block: Option<&Shared<Block>>, // handle while
        body_block: Option<&Shared<Block>>, // handle while
        exit_block: Option<&Shared<Block>>, // handle while
    ) -> Vec<Shared<Block>> {
        let mut flatten_blocks = vec![entry_block];

        for op in ori_block.borrow_mut().get_ops_as_mut() {
            if optype_checkif!(op, OpType::If) {
                flatten_blocks.append(&mut self.flat_if(op));
                continue;
            } else if optype_checkif!(op, OpType::IfElse) {
                flatten_blocks.append(&mut self.flat_ifelse(op));
                continue;
            } else if optype_checkif!(op, OpType::While) {
                flatten_blocks.append(&mut self.flat_while(op));
                continue;
            } else if optype_checkif!(op, OpType::Proceed) {
                assert!(cond_block.is_some() && body_block.is_some() && exit_block.is_some());

                let cond_value = op.get_operand(0);

                let mut process_op = self.new_op(&Type::Void, &OpType::Branch);

                process_op
                    .add_operand(cond_value)
                    .set_attr(0, Attr::True(Rc::downgrade(&body_block.unwrap())))
                    .set_attr(1, Attr::False(Rc::downgrade(&exit_block.unwrap())));

                continue;
            } else if optype_checkif!(op, OpType::Break) {
                assert!(cond_block.is_some() && body_block.is_some() && exit_block.is_some());

                let mut break_op = self.new_op(&Type::Void, &OpType::Branch);

                break_op.set_attr(0, Attr::NoCond(Rc::downgrade(&exit_block.unwrap())));

                continue;
            } else if optype_checkif!(op, OpType::Continue) {
                assert!(cond_block.is_some() && body_block.is_some() && exit_block.is_some());

                let mut continue_op = self.new_op(&Type::Void, &OpType::Branch);

                continue_op.set_attr(0, Attr::NoCond(Rc::downgrade(&body_block.unwrap())));

                continue;
            }

            op.set_id(self.next_id());
            self.add_operation(op);
        }

        flatten_blocks
    }

    fn flat_if(&mut self, r#if: &Shared<Operation>) -> Vec<Shared<Block>> {
        let mut flatten_blocks = vec![];

        let cond = r#if.get_operand(0);

        let true_block = self.new_block();
        let exit_block = self.new_block();

        // emit branch in entry block
        let mut branch_op = self.new_op(&Type::Void, &OpType::Branch);
        branch_op
            .add_operand(cond)
            .set_attr(0, Attr::True(Rc::downgrade(&true_block)))
            .set_attr(1, Attr::False(Rc::downgrade(&exit_block)));

        // then block

        self.builder.push_block(&true_block);

        flatten_blocks.append(&mut self.run_on_entry(
            r#if.get_region(0).get_entry_block(),
            true_block,
            None,
            None,
            None,
        ));

        let mut branch_op = self.new_op(&Type::Void, &OpType::Branch);
        branch_op.set_attr(0, Attr::NoCond(Rc::downgrade(&exit_block)));

        self.builder.pop_block();

        // exit block (push only)

        self.builder.push_block(&exit_block);
        flatten_blocks.push(exit_block);

        flatten_blocks
    }

    fn flat_ifelse(&mut self, r#if_else: &Shared<Operation>) -> Vec<Shared<Block>> {
        let mut flatten_blocks = vec![];

        let cond = r#if_else.get_operand(0);

        let true_block = self.new_block();
        let false_block = self.new_block();
        let exit_block = self.new_block();

        // emit branch in entry block
        let mut branch_op = self.new_op(&Type::Void, &OpType::Branch);
        branch_op
            .add_operand(cond)
            .set_attr(0, Attr::True(Rc::downgrade(&true_block)))
            .set_attr(1, Attr::False(Rc::downgrade(&false_block)));

        // then block

        self.builder.push_block(&true_block);

        flatten_blocks.append(&mut self.run_on_entry(
            r#if_else.get_region(0).get_entry_block(),
            true_block,
            None,
            None,
            None,
        ));

        let mut branch_op = self.new_op(&Type::Void, &OpType::Branch);
        branch_op.set_attr(0, Attr::NoCond(Rc::downgrade(&exit_block)));

        self.builder.pop_block();

        // else block

        self.builder.push_block(&false_block);

        flatten_blocks.append(&mut self.run_on_entry(
            r#if_else.get_region(1).get_entry_block(),
            false_block,
            None,
            None,
            None,
        ));

        let mut branch_op = self.new_op(&Type::Void, &OpType::Branch);
        branch_op.set_attr(0, Attr::NoCond(Rc::downgrade(&exit_block)));

        self.builder.pop_block();

        // exit block (push only)

        self.builder.push_block(&exit_block);
        flatten_blocks.push(Rc::clone(&exit_block));

        flatten_blocks
    }

    fn flat_while(&mut self, r#while: &Shared<Operation>) -> Vec<Shared<Block>> {
        let mut flatten_blocks = vec![];

        let header_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        // emit branch in entry block
        let mut branch_op = self.new_op(&Type::Void, &OpType::Branch);
        branch_op.set_attr(0, Attr::NoCond(Rc::downgrade(&header_block)));

        // header block
        self.builder.push_block(&header_block);

        flatten_blocks.append(&mut self.run_on_entry(
            r#while.get_region(0).get_entry_block(),
            Rc::clone(&header_block),
            Some(&header_block),
            Some(&body_block),
            Some(&exit_block),
        ));

        self.builder.pop_block();

        // body block
        self.builder.push_block(&body_block);

        flatten_blocks.append(&mut self.run_on_entry(
            r#while.get_region(1).get_entry_block(),
            Rc::clone(&body_block),
            Some(&header_block),
            Some(&body_block),
            Some(&exit_block),
        ));

        // body may not explicit use 'continue'
        self.new_op(&Type::Void, &OpType::Branch)
            .set_attr(0, Attr::NoCond(Rc::downgrade(&body_block)));

        self.builder.pop_block();

        // exit block (push only)
        self.builder.push_block(&exit_block);
        flatten_blocks.push(exit_block);

        flatten_blocks
    }
}
