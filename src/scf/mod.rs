use std::cell::RefCell;
use std::rc::Rc;

use crate::visitor::visitor::Shared;

pub mod attr;
pub mod block;
pub mod no_wrap;
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

#[macro_use]
pub mod r#macro {

    macro_rules! optype_checkif {
        ($operation : expr, $optype : expr) => {
            $operation.get_optype() == $optype
        };
    }

    macro_rules! optype_assert {
        ($operation : expr, $optype : expr) => {
            assert_eq!($operation.get_optype(), $optype)
        };
    }

    macro_rules! type_checkif {
        ($operation : expr, $type : expr) => {
            $operation.get_type() == $type
        };
    }

    macro_rules! op_ptr_defref {
        ($op : expr) => {{
            let optype = $op.get_optype();
            match optype {
                OpType::Store => $op.get_operand(2).get_type().deref(),
                OpType::Load => $op.get_operand(1).get_type().deref(),
                OpType::Int2Ptr => $op.get_type().deref(),
                OpType::Ptr2Int => $op.get_operand(1).get_type().deref(),
                OpType::DeclGlobal => $op.get_type().deref(),
                OpType::Alloca => $op.get_type().deref(),
                OpType::GetArg => $op.get_type().deref(),
                _ => panic!("op_ptr_defref!: get unexpected op {}", optype.to_string()),
            }
        }};
    }

    macro_rules! value_ptr_defref {
        ($op : expr) => {{
            let optype = $op.def.get_optype();
            match optype {
                OpType::Store => $op.def.get_operand(2).get_type().deref(),
                OpType::Load => $op.def.get_operand(1).get_type().deref(),
                OpType::Int2Ptr => $op.get_type().deref(),
                OpType::Ptr2Int => $op.def.get_operand(1).get_type().deref(),
                OpType::DeclGlobal => $op.get_type().deref(),
                OpType::Alloca => $op.get_type().deref(),
                OpType::GetArg => $op.get_type().deref(),
                _ => panic!(
                    "value_ptr_defref!: get unexpected op {}",
                    optype.to_string()
                ),
            }
        }};
    }

    macro_rules! value_type {
        ($op : expr) => {{ $op.get_type() }};
    }

    macro_rules! get_fns {
        ($module : expr) => {{
            optype_assert!($module, OpType::Module);

            let mut fns = Vec::new();

            for func in $module
                .get_default_region()
                .get_entry_block()
                .borrow()
                .get_ops()
            {
                if !optype_checkif!(func, OpType::Function) {
                    continue;
                }
                fns.push(Rc::clone(&func));
            }

            fns
        }};
    }

    macro_rules! get_decl {
        ($module : expr) => {{
            optype_assert!($module, OpType::Module);

            let mut decls = Vec::new();

            for decl in $module
                .get_default_region()
                .get_entry_block()
                .borrow()
                .get_ops()
            {
                if !optype_checkif!(decl, OpType::DeclGlobal) {
                    continue;
                }
                decls.push(Rc::clone(&decl));
            }

            decls
        }};
    }

    macro_rules! fn_decl_only {
        ($func : expr) => {{
            optype_assert!($func, OpType::Function);

            if matches!($func.get_attr(2), Attr::DeclOnly) {
                true
            } else {
                false
            }
        }};
    }

    macro_rules! fn_name {
        ($func : expr) => {{
            optype_assert!($func, OpType::Function);

            match $func.get_attr(0).clone() {
                Attr::Name(fn_name) => fn_name,
                _ => panic!(),
            }
        }};
    }

    macro_rules! fn_format_args {
        ($func : expr) => {{
            optype_assert!($func, OpType::Function);

            match $func.get_attr(1).clone() {
                Attr::Args(args) => args,
                _ => panic!(),
            }
        }};
    }

    macro_rules! get_name {
        ($op : expr) => {{
            let mut name = None;
            for attr in $op.borrow().get_attrs() {
                match attr.clone() {
                    Attr::Name(n) => name = Some(n),
                    _ => continue,
                }
            }

            name
        }};
    }

    macro_rules! get_align {
        ($op : expr) => {{
            let mut align = None;
            for attr in $op.borrow().get_attrs() {
                match attr.clone() {
                    Attr::Align(a) => align = Some(a),
                    _ => continue,
                }
            }

            align
        }};
    }

    macro_rules! get_int {
        ($op : expr) => {{
            let mut r#i32 = None;
            for attr in $op.borrow().get_attrs() {
                match attr {
                    Attr::Int32(i) => r#i32 = Some(*i),
                    _ => continue,
                }
            }

            r#i32
        }};
    }

    macro_rules! get_long {
        ($op : expr) => {{
            let mut r#i64 = None;
            for attr in $op.borrow().get_attrs() {
                match attr {
                    Attr::Int64(l) => r#i64 = Some(*l),
                    _ => continue,
                }
            }

            r#i64
        }};
    }

    macro_rules! get_float {
        ($op : expr) => {{
            let mut r#f32 = None;
            for attr in $op.borrow().get_attrs() {
                match attr {
                    Attr::Float(f) => r#f32 = Some(*f),
                    _ => continue,
                }
            }

            r#f32
        }};
    }

    macro_rules! get_cond {
        ($op : expr) => {{
            let mut cond = None;
            for attr in $op.borrow().get_attrs() {
                match attr {
                    Attr::Cond(c) => cond = Some(c.clone()),
                    _ => continue,
                }
            }

            cond
        }};
    }

    macro_rules! get_true {
        ($op : expr) => {{
            let mut label = None;
            for attr in $op.borrow().get_attrs() {
                match attr {
                    Attr::True(l) => label = Some(Weak::upgrade(l).unwrap()),
                    _ => continue,
                }
            }

            label
        }};
    }

    macro_rules! get_false {
        ($op : expr) => {{
            let mut label = None;
            for attr in $op.borrow().get_attrs() {
                match attr {
                    Attr::False(l) => label = Some(Weak::upgrade(l).unwrap()),
                    _ => continue,
                }
            }

            label
        }};
    }

    macro_rules! get_nocond {
        ($op : expr) => {{
            let mut label = None;
            for attr in $op.borrow().get_attrs() {
                match attr {
                    Attr::NoCond(l) => label = Some(Weak::upgrade(l).unwrap()),
                    _ => continue,
                }
            }

            label
        }};
    }

    macro_rules! get_seq {
        ($op : expr) => {{
            let mut seq = None;
            for attr in $op.borrow().get_attrs() {
                match attr {
                    Attr::ArgSeq(s) => seq = Some(s.clone()),
                    _ => continue,
                }
            }

            seq
        }};
    }

    macro_rules! get_format_args {
        ($op : expr) => {{
            let mut args = None;
            for attr in $op.borrow().get_attrs() {
                match attr {
                    Attr::Args(v) => args = Some(v.clone()),
                    _ => continue,
                }
            }

            args
        }};
    }

    macro_rules! is_decl_only {
        ($op : expr) => {{
            let mut decl_only = false;
            for attr in $op.borrow().get_attrs() {
                match attr {
                    Attr::DeclOnly => decl_only = true,
                    _ => continue,
                }
            }
            decl_only
        }};
    }

    pub(crate) use {
        fn_decl_only, fn_format_args, fn_name, get_align, get_cond, get_decl, get_false, get_float,
        get_fns, get_format_args, get_int, get_long, get_name, get_nocond, get_seq, get_true,
        is_decl_only, op_ptr_defref, optype_assert, optype_checkif, type_checkif, value_ptr_defref,
        value_type,
    };
}
