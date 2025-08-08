use std::{cell::RefCell, rc::Rc};

use crate::scf::{block::Block, value::Type};

// this is where to apply most of dialect

#[derive(Debug)]
pub enum CondFlag {
    Ls,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

impl ToString for CondFlag {
    fn to_string(&self) -> String {
        match self {
            CondFlag::Ls => "less".to_string(),
            CondFlag::Le => "less equal".to_string(),
            CondFlag::Gt => "great".to_string(),
            CondFlag::Ge => "great equal".to_string(),
            CondFlag::Eq => "equal".to_string(),
            CondFlag::Ne => "not equal".to_string(),
        }
    }
}

#[derive(Debug)]
pub enum Attr {
    Empty,

    Type(Type),

    Name(String),
    Int32(i32),           // const
    Int64(i64),           //const
    IntArray(Vec<i32>),   // const
    Float(f32),           // const
    FloatArray(Vec<f32>), // const
    Size(usize),
    // IfElse(Rc<RefCell<Block>>, Option<Rc<RefCell<Block>>>),
    // Callers(Vec<String>),
    Args(Vec<(String, Type)>),
    ArgSeq(usize),
    Cond(CondFlag),
}

impl ToString for Attr {
    fn to_string(&self) -> String {
        match self {
            Attr::Empty => "".to_string(),
            Attr::Name(name) => format!(" <name = {}>", name),
            Attr::Type(ty) => format!(" <type = {}>", ty.to_string()),
            Attr::Int32(i32) => format!(" <i32 = {}>", i32),
            Attr::Int64(i64) => format!(" <i64 = {}>", i64),
            Attr::IntArray(array) => format!(
                " <i32s = {}>",
                array
                    .iter()
                    .map(|i32| format!("{}", i32))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Attr::Float(f32) => format!(" <f32 = {}>", f32),
            Attr::FloatArray(array) => format!(
                " <f32s = {}>",
                array
                    .iter()
                    .map(|f32| format!("{}", f32))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Attr::Size(size) => format!(" <size = {}>", size),
            Attr::Args(args) => format!(
                " <args = {}>",
                args.iter()
                    .map(|(name, ty)| format!("{} : {}", name, ty.to_string()))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Attr::ArgSeq(seq) => format!(" <seq = {}>", seq),
            Attr::Cond(cond) => format!(" <cond = {}>", cond.to_string()),
        }
    }
}
