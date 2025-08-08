use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::scf::block::Block;
use crate::scf::operation::Operation;
use crate::scf::{Parent, Print};
use crate::visitor::visitor::Shared;

#[derive(Debug)]
pub struct Region {
    blocks: Vec<Rc<RefCell<Block>>>,

    parent: Weak<RefCell<Operation>>,
}

impl Region {
    pub fn new(id: u64, parent: &Rc<RefCell<Operation>>) -> Shared<Region> {
        let new_region = Rc::new(RefCell::new(Region {
            blocks: Vec::new(),
            parent: Rc::downgrade(parent),
        }));

        parent.borrow_mut().add_region(Rc::clone(&new_region));

        let _ = Block::new(id, &Rc::clone(&new_region)); // defalut block

        new_region
    }

    pub fn get_entry_block(&self) -> Shared<Block> {
        Rc::clone(&self.blocks[0])
    }

    pub fn add_block(&mut self, new_block: Shared<Block>) {
        let exists = self
            .blocks
            .iter()
            .any(|block| Rc::ptr_eq(block, &new_block));

        if !exists {
            self.blocks.push(new_block);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.blocks.len() <= 1 && self.get_entry_block().borrow().is_empty()
    }
}

impl Parent for Region {
    type ParentType = Operation;

    fn get_parent(&self) -> Rc<RefCell<Self::ParentType>> {
        Weak::upgrade(&self.parent).unwrap()
    }

    fn set_parent(&mut self, new_parent: Rc<RefCell<Self::ParentType>>) {
        self.parent = Rc::downgrade(&new_parent);
    }
}

impl Print for Region {
    fn print(&self, indent: usize) -> String {
        if self.is_empty() {
            "".to_string()
        } else {
            format!(
                "{}",
                self.blocks
                    .iter()
                    .map(|block| block.borrow().print(indent))
                    .collect::<Vec<_>>()
                    .join("")
            )
        }
    }
}
