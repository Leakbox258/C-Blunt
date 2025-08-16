use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::scf::r#macro::optype_checkif;
use crate::scf::no_wrap::*;
use crate::scf::operation::{OpType, Operation};
use crate::scf::region::Region;
use crate::scf::{Parent, Print};
use crate::visitor::visitor::Shared;

#[derive(Debug)]
pub struct Block {
    id: u64,
    operations: Vec<Rc<RefCell<Operation>>>,

    parent: Weak<RefCell<Region>>,
}

impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Parent for Block {
    type ParentType = Region;

    fn get_parent(&self) -> Rc<RefCell<Self::ParentType>> {
        Weak::upgrade(&self.parent).unwrap()
    }

    fn set_parent(&mut self, new_parent: Rc<RefCell<Self::ParentType>>) {
        self.parent = Rc::downgrade(&new_parent);
    }
}

impl Print for Block {
    fn print(&self, indent: usize) -> String {
        if self.is_empty() {
            "".to_string()
        } else {
            format!(
                "\n{}%{} {{\n{}\n{}}}",
                Block::INDENT.to_string().repeat(indent),
                self.id,
                self.operations
                    .iter()
                    .map(|op| op.borrow().print(indent + 1))
                    .collect::<Vec<_>>()
                    .join("\n"),
                Block::INDENT.to_string().repeat(indent)
            )
        }
    }
}

impl Block {
    pub fn new(id: u64, parent: &Rc<RefCell<Region>>) -> Shared<Block> {
        let new_block = Rc::new(RefCell::new(Block {
            id: id,
            operations: Vec::new(),
            parent: Rc::downgrade(parent),
        }));

        parent.borrow_mut().add_block(Rc::clone(&new_block));
        new_block
    }

    pub fn get_id(&self) -> u64 {
        self.id
    }

    pub fn add_op(&mut self, op: &Shared<Operation>) {
        self.operations.push(Rc::clone(op));
    }

    pub fn del_op(&mut self, op: &Shared<Operation>) {
        self.operations.retain(|elem| elem != op);
    }

    pub fn append_ops(&mut self, ops: &mut Vec<Shared<Operation>>) {
        self.operations.append(ops);
    }

    pub fn get_ops(&self) -> &Vec<Shared<Operation>> {
        &self.operations
    }

    pub fn get_ops_as_mut(&mut self) -> &mut Vec<Shared<Operation>> {
        &mut self.operations
    }

    pub fn ends_with(&self, opty: OpType) -> bool {
        !self.operations.is_empty() && optype_checkif!(self.operations.last().unwrap(), opty)
    }

    pub fn replace_op(&mut self, ops: Vec<Shared<Operation>>) {
        self.operations = ops;
    }

    pub fn is_empty(&self) -> bool {
        self.operations.is_empty()
    }
}
