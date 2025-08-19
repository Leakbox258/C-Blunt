use core::panic;
use std::{ops::Add, ops::Div, ops::Mul, ops::Rem, ops::Sub, rc::Rc};

#[derive(Debug)]
pub struct Program {
    pub compunit: CompUnit,
}

#[derive(Debug)]
pub struct CompUnit {
    pub decls: Vec<VarDecl>,
    pub funcs: Vec<FuncDef>,
}

#[derive(Debug)]
pub struct VarDecl {
    pub ty: BType,
    pub vardefs: Vec<VarDef>,
}

#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub subscripts: Option<Vec<Rc<ConstExpr>>>,
    pub init: Option<InitVal>,
}

#[derive(Debug)]
pub enum InitVal {
    Simple(Rc<Expr>),
    EmptyArray,
    Array(Vec<InitVal>),
}

#[derive(Debug)]
pub enum BType {
    Int,
    Float,
    Void,
}

#[derive(Debug)]
pub struct FuncDef {
    pub ty: BType,
    pub name: String,
    pub params: Vec<FuncFParam>,
    pub body: Option<Vec<BlockItem>>,
}

#[derive(Debug)]
pub enum Dimension {
    Deduce,
    Declare(Rc<Expr>), // variable Expr appear in Function format arg's subscripts seems not very resonable ?
}

#[derive(Debug)]
pub struct FuncFParam {
    pub ty: BType,
    pub name: String,
    pub dimensions: Option<Vec<Dimension>>,
}

#[derive(Debug)]
pub enum BlockItem {
    Declare(VarDecl),
    Statement(Stmt),
}

#[derive(Debug)]
pub struct Block {
    pub items: Option<Vec<BlockItem>>,
}

#[derive(Debug)]
pub enum Stmt {
    NULL,
    Expression(Rc<Expr>),
    Assign(LVal, Rc<Expr>),
    Block(Block),
    IfElse(Rc<Cond>, Box<Stmt>, Option<Box<Stmt>>), // avoid recursive
    While(Rc<Cond>, Box<Stmt>),
    Break,
    Continue,
    Return(Option<Rc<Expr>>),
}

#[derive(Debug, Clone)]
pub enum Operator {
    // Unary
    Pos,
    Neg,
    Not,
    // Binary
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Logical
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    And,
    Or,
}

impl Operator {
    pub fn is_logical(&self) -> bool {
        match self {
            Operator::Lt
            | Operator::Gt
            | Operator::Ge
            | Operator::Le
            | Operator::Eq
            | Operator::Ne
            | Operator::And
            | Operator::Or => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum PrimaryExpr {
    Expression(Rc<Expr>),
    LValue(LVal),
    Number(Num),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Num {
    Int(i32),
    Float(f32),
}

impl Num {
    pub fn get_int(&self) -> Option<i32> {
        match self {
            Num::Int(v) => Some(*v),
            _ => None,
        }
    }

    pub fn get_float(&self) -> Option<f32> {
        match self {
            Num::Float(v) => Some(*v),
            _ => None,
        }
    }

    pub fn reduce(lhs: &Num, rhs: &Num) -> (Num, Num) {
        if Num::same_ty(lhs, rhs) {
            (*lhs, *rhs)
        } else {
            if lhs.get_int().is_some() {
                (Num::Float(lhs.get_int().unwrap() as f32), *rhs)
            } else {
                (*lhs, Num::Float(rhs.get_int().unwrap() as f32))
            }
        }
    }

    pub fn same_ty(lhs: &Num, rhs: &Num) -> bool {
        if matches!(lhs, Num::Int(_)) && matches!(rhs, Num::Int(_)) {
            true
        } else if matches!(lhs, Num::Float(_)) && matches!(rhs, Num::Float(_)) {
            true
        } else {
            false
        }
    }
}

macro_rules! impl_binary_op {
    ($trait:ident, $method:ident, $op:tt) => {
        impl<'a, 'b> $trait<&'b Num> for &'a Num {
            type Output = Num;
            fn $method(self, other: &'b Num) -> Num {
                match (self, other) {
                    (Num::Int(l), Num::Int(r)) => Num::Int(l $op r),
                    (Num::Float(l), Num::Float(r)) => Num::Float(l $op r),
                    (Num::Int(l), Num::Float(r)) => Num::Float(*l as f32 $op *r),
                    (Num::Float(l), Num::Int(r)) => Num::Float(*l $op *r as f32),
                }
            }
        }
    };
}

impl_binary_op!(Add, add, +);
impl_binary_op!(Sub, sub, -);
impl_binary_op!(Mul, mul, *);
impl_binary_op!(Div, div, /);
impl_binary_op!(Rem, rem, %);

macro_rules! impl_logical_op {
    ($num1 : expr, $num2 : expr, $op : tt) => {{}};
}

// Optimized function to perform the operation
pub fn apply_operation(lhs: &Num, rhs: &Num, op: &Operator) -> Num {
    match op {
        Operator::Add => lhs + rhs,
        Operator::Sub => lhs - rhs,
        Operator::Mul => lhs * rhs,
        Operator::Div => lhs / rhs,
        Operator::Mod => lhs % rhs,
        Operator::Eq => {
            let (lhs, rhs) = Num::reduce(lhs, rhs);

            if lhs == rhs { Num::Int(1) } else { Num::Int(0) }
        }
        Operator::Ne => {
            let (lhs, rhs) = Num::reduce(lhs, rhs);

            if lhs != rhs { Num::Int(1) } else { Num::Int(0) }
        }
        Operator::Gt => {
            let (lhs, rhs) = Num::reduce(lhs, rhs);

            if lhs > rhs { Num::Int(1) } else { Num::Int(0) }
        }
        Operator::Ge => {
            let (lhs, rhs) = Num::reduce(lhs, rhs);

            if lhs >= rhs { Num::Int(1) } else { Num::Int(0) }
        }
        Operator::Lt => {
            let (lhs, rhs) = Num::reduce(lhs, rhs);

            if lhs < rhs { Num::Int(1) } else { Num::Int(0) }
        }
        Operator::Le => {
            let (lhs, rhs) = Num::reduce(lhs, rhs);

            if lhs <= rhs { Num::Int(1) } else { Num::Int(0) }
        }
        Operator::And => {
            let (lhs, zero_1) = Num::reduce(lhs, &Num::Int(0));
            let (zero_2, rhs) = Num::reduce(&Num::Int(0), rhs);

            if lhs != zero_1 && zero_2 != rhs {
                Num::Int(1)
            } else {
                Num::Int(0)
            }
        }
        Operator::Or => {
            let (lhs, zero_1) = Num::reduce(lhs, &Num::Int(0));
            let (zero_2, rhs) = Num::reduce(&Num::Int(0), rhs);

            if lhs != zero_1 || zero_2 != rhs {
                Num::Int(1)
            } else {
                Num::Int(0)
            }
        }
        _ => panic!(),
    }
}

#[derive(Debug)]
pub struct CallExpr {
    pub ident: String,
    pub params: Option<Vec<Rc<FuncRParam>>>,
}

#[derive(Debug)]
pub enum ParenExpr {
    FunctionCall(Box<CallExpr>),
    Expression(Box<PrimaryExpr>), // to break recursive use circle
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub inner: Option<ParenExpr>, // boxed, very inner, used when not have a op
    pub op: Option<Operator>,
    pub next: Option<Rc<Self>>, // more inner one
    pub constant: Option<Num>,
}

#[derive(Debug)]
pub struct BinaryExpr<ValueExpr: Sized> {
    pub lhs: Option<Rc<Self>>,
    pub rhs: Rc<ValueExpr>,
    pub op: Option<Operator>,
    pub constant: Option<Num>,
}

pub type MulExpr = BinaryExpr<UnaryExpr>;
pub type AddExpr = BinaryExpr<MulExpr>;
pub type RelExpr = BinaryExpr<AddExpr>;
pub type EqExpr = BinaryExpr<RelExpr>;
pub type LAndExpr = BinaryExpr<EqExpr>;
pub type LOrExpr = BinaryExpr<LAndExpr>;
pub type Expr = AddExpr;
pub type ConstExpr = AddExpr;
pub type FuncRParam = AddExpr;
pub type Cond = LOrExpr;

#[derive(Debug)]
pub struct LVal {
    pub ident: String,
    pub subscripts: Option<Vec<Rc<Expr>>>,
}
