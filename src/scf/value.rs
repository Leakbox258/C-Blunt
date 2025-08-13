use std::usize;

use crate::{
    scf::operation::{OpType, Operation},
    visitor::visitor::Shared,
};

#[derive(Debug, Clone)]
pub struct Value {
    pub def: Shared<Operation>, // be def
}

impl Value {
    pub fn get_type(&self) -> Type {
        self.def.borrow().get_type()
    }

    pub fn get_optype(&self) -> OpType {
        self.def.borrow().get_optype()
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        format!(" %{}", self.def.borrow().get_id())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int32,
    Float32,
    Void,
    Int64,
    Bool,
    Ptr(Box<Type>),
    Array(Box<(Type, Option<usize>)>),
}

impl Type {
    pub fn new_array_type(ty: &Type, subscripts: &Vec<usize>) -> Type {
        assert!(subscripts.len() > 0 && subscripts[0] != 0);

        let mut from_tail = Type::Array(Box::new((ty.clone(), None)));
        for sub in subscripts.iter().rev() {
            from_tail = Type::Array(Box::new((from_tail, Some(*sub))));
        }

        from_tail
    }

    pub fn new_ptr_type(ty: &Type, subscripts: &Vec<usize>) -> Type {
        if subscripts.len() == 0 {
            return Type::Ptr(Box::new(ty.clone()));
        }

        let mut from_tail = ty.clone();

        for _ in subscripts {
            from_tail = Type::Ptr(Box::new(from_tail));
        }

        from_tail
    }

    pub fn is_array(&self) -> bool {
        match self {
            Self::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            Self::Ptr(_) => true,
            _ => false,
        }
    }

    // use to get type pointed by this ptr type
    pub fn deref(&self) -> Type {
        match self {
            Self::Ptr(inner) => inner.as_ref().clone(),
            _ => panic!(),
        }
    }

    // dowmgrade array type to ptr type
    pub fn downgrade(&self) -> Type {
        match self {
            Self::Array(inner_ty) => {
                let (inner_type, capacity) = inner_ty.as_ref();
                if capacity.is_some() {
                    inner_type.downgrade()
                } else {
                    Type::Ptr(Box::new(inner_type.clone()))
                }
            }
            Self::Bool | Self::Int32 | Self::Int64 | Self::Float32 | Self::Ptr(_) => self.clone(),
            Self::Void => panic!(),
        }
    }

    pub fn get_btyes(&self) -> usize {
        match self {
            Type::Int32 => 4,
            Type::Float32 => 4,
            Type::Int64 => 8,
            Type::Bool => 4, // make it ez to align
            Type::Void => panic!(),
            Type::Ptr(_) => 8,
            Type::Array(inner) => {
                let (inner_type, capacity) = inner.as_ref();

                match capacity {
                    Some(cnt) => *cnt * 4 * inner_type.get_btyes(),
                    None => inner_type.get_btyes(),
                }
            }
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            Type::Int32 => 1,
            Type::Float32 => 1,
            Type::Int64 => 1,
            Type::Bool => 1,
            Type::Void => panic!(),
            Type::Ptr(_) => 8,
            Type::Array(inner) => {
                let (inner_type, capacity) = inner.as_ref();

                match capacity {
                    Some(cnt) => *cnt * inner_type.get_size(),
                    None => inner_type.get_size(),
                }
            }
        }
    }

    pub fn get_basic_type(&self) -> Type {
        match self {
            Type::Int32 => Type::Int32,
            Type::Float32 => Type::Float32,
            Type::Int64 => Type::Int64,
            Type::Bool => Type::Bool,
            Type::Void => Type::Void,
            Type::Ptr(_) => Type::Int64,
            Type::Array(inner) => {
                let (inner_type, capacity) = inner.as_ref();

                match capacity {
                    Some(_) => inner_type.get_basic_type(),
                    None => inner_type.clone(),
                }
            }
        }
    }
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Int32 => "i32".to_string(),
            Type::Int64 => "i64".to_string(),
            Type::Float32 => "float".to_string(),
            Type::Void => "void".to_string(),
            Type::Bool => "i1".to_string(),
            Type::Ptr(ty) => format!("{}*", ty.to_string()),
            Type::Array(array_type) => {
                let (inner_type, size) = array_type.as_ref();

                match size {
                    Some(elem_size) => format!("[{} x {}]", elem_size, inner_type.to_string()),
                    None => inner_type.to_string(),
                }
            }
        }
    }
}
