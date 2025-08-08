use std::cell::RefCell;
use std::rc::Rc;

use crate::visitor::visitor::Shared;

pub mod attr;
pub mod block;
pub mod operation;
pub mod region;
pub mod value;

pub trait Parent {
    type ParentType;
    fn set_parent(&mut self, new_parent: Rc<RefCell<Self::ParentType>>);
    fn get_parent(&self) -> Rc<RefCell<Self::ParentType>>;
}

pub trait SharedFromSelf {
    type SelfType;
    fn share_from_self(&self) -> Shared<Self::SelfType>;
}

pub trait Print {
    const INDENT: &str = "  ";
    fn print(&self, indent: usize) -> String;
}
