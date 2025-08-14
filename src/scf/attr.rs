use std::{cell::RefCell, fmt::format, rc::Weak};

use crate::{
    scf::{block::Block, no_wrap::BlockTrait, value::Type},
    visitor::visitor::Shared,
};

// this is where to apply most of dialect

#[derive(Debug, Clone)]
pub enum CondFlag {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

impl ToString for CondFlag {
    fn to_string(&self) -> String {
        match self {
            CondFlag::Lt => "less".to_string(),
            CondFlag::Le => "less equal".to_string(),
            CondFlag::Gt => "great".to_string(),
            CondFlag::Ge => "great equal".to_string(),
            CondFlag::Eq => "equal".to_string(),
            CondFlag::Ne => "not equal".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Attr {
    Empty,
    Name(String),
    Int32(i32),           // const
    Int64(i64),           // const
    IntArray(Vec<i32>),   // const
    Float(f32),           // const
    FloatArray(Vec<f32>), // const
    Size(usize),
    ArrayShape(Vec<usize>),
    DeclOnly,
    Args(Vec<(String, Type)>),
    ArgSeq(usize),
    Cond(CondFlag),
    True(Weak<RefCell<Block>>),
    False(Weak<RefCell<Block>>),
    NoCond(Weak<RefCell<Block>>),
    Align(u32),
}

impl ToString for Attr {
    fn to_string(&self) -> String {
        match self {
            Attr::Empty => "".to_string(),
            Attr::Name(name) => format!(" <name = {}>", name),
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
            Attr::DeclOnly => format!(" <decl only>"),
            Attr::Size(size) => format!(" <size = {}>", size),
            Attr::ArrayShape(v) => format!(
                " <shape = {}>",
                v.into_iter()
                    .map(|elm| format!("{}", elm))
                    .collect::<Vec<_>>()
                    .join("x")
            ),
            Attr::Args(args) => format!(
                " <args = {}>",
                args.iter()
                    .map(|(name, ty)| format!("{} : {}", name, ty.to_string()))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Attr::ArgSeq(seq) => format!(" <seq = {}>", seq),
            Attr::Cond(cond) => format!(" <cond = {}>", cond.to_string()),
            Attr::True(bb) => format!(" <true = %{}>", Weak::upgrade(bb).unwrap().get_id()),
            Attr::False(bb) => format!(" <false = %{}>", Weak::upgrade(bb).unwrap().get_id()),
            Attr::NoCond(bb) => format!(" <nocond = %{}>", Weak::upgrade(bb).unwrap().get_id()),
            Attr::Align(align) => format!(" <align = {}>", align),
        }
    }
}
