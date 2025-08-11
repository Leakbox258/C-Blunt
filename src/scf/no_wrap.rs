use crate::scf::Print;
use crate::scf::attr::Attr;
use crate::scf::block::Block;
use crate::scf::operation::{OpType, Operation};
use crate::scf::region::Region;
use crate::scf::value::Value;
use crate::visitor::visitor::Shared;
use std::cell::RefCell;
use std::rc::Rc;

macro_rules! define_methods_trait {
    ($struct_name:ident, $struct_methods:ident,
     immutable: [$($immut_method:ident($($immut_params:ident : $immut_param_ty:ty),*) $(-> $immut_ret:ty)?),*],
     mutable: [$($mut_method:ident($($mut_params:ident : $mut_param_ty:ty),*) $(-> $mut_ret:ty)?),*],
     chaining_mutable: [$($chain_mut_method:ident($($chain_mut_params:ident : $chain_mut_param_ty:ty),*)),*]) => {

        pub trait $struct_methods {
            $(fn $immut_method(&self, $($immut_params : $immut_param_ty),*) $(-> $immut_ret)?;)*

            $(fn $mut_method(&mut self, $($mut_params : $mut_param_ty),*) $(-> $mut_ret)?;)*

            $(fn $chain_mut_method(&mut self, $($chain_mut_params : $chain_mut_param_ty),*) -> &mut Self;)*
        }

        impl $struct_methods for Rc<RefCell<$struct_name>> {
            $(fn $immut_method(&self, $($immut_params : $immut_param_ty),*) $(-> $immut_ret)? {
                self.borrow().$immut_method($($immut_params)*)
            })*

            $(fn $mut_method(&mut self, $($mut_params : $mut_param_ty),*) $(-> $mut_ret)? {
                self.borrow_mut().$mut_method($($mut_params)*)
            })*

            $(fn $chain_mut_method(&mut self, $($chain_mut_params : $chain_mut_param_ty),*) -> &mut Self {
                self.borrow_mut().$chain_mut_method($($chain_mut_params,)*);
                self
            })*
        }
    };
}

define_methods_trait! {
    Operation,
    OperationTrait,
    immutable :         [
                            get_id() -> u64,
                            get_operand(seq : usize) -> Value,
                            get_default_region() -> Shared<Region>,
                            get_region(seq : usize) -> Shared<Region>,
                            get_optype() -> OpType,
                            print(indent : usize) -> String
    ],
    mutable :           [
                            add_use(new_use : Shared<Operation>),
                            set_id(id : u64)
    ],
    chaining_mutable :  [
                            add_operand(new_operand : Value),
                            add_operand_shared(new_operand : Shared<Value>),
                            append_operands(operands : &mut Vec<Value>),
                            set_operand(seq : usize, new_operand : Shared<Value>),
                            add_region(new_region : Shared<Region>),
                            set_attr(seq : usize, attr : Attr)
    ]
}

define_methods_trait! {
    Region,
    RegionTrait,
    immutable :         [
                            get_entry_block() -> Shared<Block>,
                            is_empty() -> bool,
                            print(indent : usize) -> String

    ],
    mutable :           [
                            add_block(new_block : Shared<Block>),
                            replace_block(new_blocks : &mut Vec<Shared<Block>>)
    ],
    chaining_mutable :  [

    ]
}

define_methods_trait! {
    Block,
    BlockTrait,
    immutable :         [
                            // get_ops() -> &Vec<Shared<Operation>>,
                            get_id() -> u64,
                            is_empty() -> bool,
                            print(indent : usize) -> String

    ],
    mutable :           [
                            add_op(op : &Shared<Operation>),
                            del_op(op : &Shared<Operation>),
                            append_ops(ops : &mut Vec<Shared<Operation>>)
                            // get_ops_as_mut() -> &mut Vec<Shared<Operation>>
    ],
    chaining_mutable :  [

    ]
}
