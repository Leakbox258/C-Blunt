use crate::scf::{no_wrap::*, operation};
use crate::{
    scf::{
        Parent, Print, SharedFromSelf,
        attr::Attr,
        block::Block,
        region::Region,
        value::{Type, Value},
    },
    visitor::visitor::Shared,
};
use std::cell::Ref;
use std::fmt;
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

#[derive(Debug)]
pub struct Operation {
    id: u64,
    uses: Vec<Weak<RefCell<Self>>>,
    operands: Vec<Value>,
    regions: Vec<Shared<Region>>,
    parent: Option<Weak<RefCell<Block>>>,
    ty: Type,
    attrs: [Attr; 8],
    opty: OpType,

    self_rc: Option<Weak<RefCell<Self>>>,
}

impl Operation {
    pub fn new_module(id: u64, rid: u64) -> Shared<Operation> {
        let new_op = Rc::new(RefCell::new(Operation {
            id: id,
            uses: Vec::new(),
            operands: Vec::new(),
            regions: Vec::new(),
            parent: None,
            ty: Type::Void,
            attrs: [const { Attr::Empty }; 8],
            opty: OpType::Module,
            self_rc: None,
        }));

        new_op.borrow_mut().self_rc = Some(Rc::downgrade(&new_op));

        let _ = Region::new(rid, &new_op);

        // new_op.borrow_mut().regions.push(Rc::clone(&default_region));
        new_op
    }

    pub fn new(id: u64, ty: Type, opty: OpType, rid: u64) -> Shared<Operation> {
        let new_op = Rc::new(RefCell::new(Operation {
            id: id,
            uses: Vec::new(),
            operands: Vec::new(),
            regions: Vec::new(),
            parent: None,
            ty: ty,
            attrs: [const { Attr::Empty }; 8],
            opty: opty,
            self_rc: None,
        }));

        new_op.borrow_mut().self_rc = Some(Rc::downgrade(&new_op));

        let _ = Region::new(rid, &new_op);

        // new_op.borrow_mut().regions.push(Rc::clone(&default_region));
        new_op
    }

    pub fn set_id(&mut self, id: u64) {
        self.id = id;
    }

    pub fn get_id(&self) -> u64 {
        self.id
    }

    pub fn add_use(&mut self, new_use: Shared<Self>) {
        self.uses.push(Rc::downgrade(&new_use));
    }

    pub fn add_operand(&mut self, new_operand: Value) -> &mut Self {
        self.operands.push(new_operand.clone());
        new_operand.def.borrow_mut().add_use(self.share_from_self());
        self
    }

    pub fn add_operand_shared(&mut self, new_operand: Shared<Value>) -> &mut Self {
        self.operands.push(new_operand.borrow().clone());
        new_operand
            .borrow_mut()
            .def
            .borrow_mut()
            .add_use(self.share_from_self());
        self
    }

    pub fn append_operands(&mut self, operands: &mut Vec<Value>) -> &mut Self {
        self.operands.append(operands);
        self
    }

    pub fn set_operand(&mut self, seq: usize, new_operand: Shared<Value>) -> &mut Self {
        self.operands[seq] = new_operand.borrow().clone();

        self
    }

    pub fn get_operand(&self, seq: usize) -> Value {
        self.operands[seq].clone()
    }

    pub fn get_operands(&self) -> &Vec<Value> {
        &self.operands
    }

    pub fn get_operands_as_mut(&mut self) -> &mut Vec<Value> {
        &mut self.operands
    }

    pub fn add_region(&mut self, new_region: Shared<Region>) -> &mut Self {
        let exists = self
            .regions
            .iter()
            .any(|region| Rc::ptr_eq(region, &new_region));

        if !exists {
            self.regions.push(new_region);
        }
        self
    }

    pub fn get_default_region(&self) -> Shared<Region> {
        Rc::clone(self.regions.get(0).unwrap())
    }

    pub fn get_default_block(&self) -> Ref<Shared<Block>> {
        Ref::map(self.regions.get(0).unwrap().borrow(), |region| {
            region.borrow_entry_block()
        })
    }

    pub fn get_region(&self, seq: usize) -> Shared<Region> {
        Rc::clone(&self.regions.get(seq).unwrap())
    }

    pub fn get_regions(&self) -> &Vec<Shared<Region>> {
        &self.regions
    }

    pub fn set_attr(&mut self, seq: usize, attr: Attr) -> &mut Self {
        self.attrs[seq] = attr;
        self
    }

    pub fn get_attr(&self, seq: usize) -> Attr {
        self.attrs[seq].clone()
    }

    pub fn get_attr_as_ref<'a>(&self, seq: usize) -> &Attr {
        &self.attrs[seq]
    }

    pub fn get_attrs(&self) -> &[Attr; 8] {
        &self.attrs
    }

    pub fn get_type(&self) -> Type {
        self.ty.clone()
    }

    pub fn get_optype(&self) -> OpType {
        self.opty
    }
}

impl PartialEq for Operation {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Operation {}

impl PartialOrd for Operation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl Ord for Operation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl SharedFromSelf for Operation {
    type SelfType = Operation;

    fn share_from_self(&self) -> Shared<Self::SelfType> {
        Weak::upgrade(&self.self_rc.clone().unwrap()).unwrap()
    }
}

impl Parent for Operation {
    type ParentType = Block;

    fn get_parent(&self) -> Rc<RefCell<Self::ParentType>> {
        Weak::upgrade(&self.parent.clone().unwrap()).unwrap()
    }

    // will put op in block as well
    fn set_parent(&mut self, new_parent: Rc<RefCell<Self::ParentType>>) {
        let parent = &self.parent;

        match parent {
            None => {}
            Some(block) => {
                Weak::upgrade(block)
                    .unwrap()
                    .borrow_mut()
                    .del_op(&self.share_from_self());
            }
        }

        self.parent = Some(Rc::downgrade(&new_parent));

        new_parent.borrow_mut().add_op(&self.share_from_self());
    }
}

impl Print for Operation {
    fn print(&self, indent: usize) -> String {
        format!(
            "{}%{} = {}{}{}{} ",
            Operation::INDENT.to_string().repeat(indent),
            self.id,
            self.opty.to_string(),
            self.operands
                .iter()
                .map(|value| value.to_string())
                .collect::<Vec<_>>()
                .join(","),
            self.attrs
                .iter()
                .map(|attr| attr.to_string())
                .collect::<Vec<_>>()
                .join(""),
            self.regions
                .iter()
                .map(|region| region.print(indent + 1))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum OpType {
    Module,
    Function,
    // structor
    If,
    IfElse,
    While,
    Loop,
    // cond
    ICmp,
    LCmp,
    FCmp,
    // flow-control
    Proceed,
    Break,
    Continue,
    Branch,
    Return,
    // mems
    Alloca,
    Store,
    Load,
    DeclGlobal,
    // unary
    Neg,
    FNeg,
    // binary
    Add,
    LAdd,
    FAdd,
    Sub,
    LSub,
    FSub,
    Mul,
    LMul,
    FMul,
    Div,
    LDiv,
    FDiv,
    Mod,
    LMod,
    FMod,
    // bitwise
    BXor, // bit
    Xor,
    LXor,
    // convert
    F2I,
    I2F,
    ZEXT,
    SEXT,
    Ptr2Int,
    Int2Ptr,
    // misc
    FuncCall,
    Phi,
    GetI,
    GetArrayI,
    GetL,
    GetArrayL,
    GetF,
    GetArrayF,
    GetFalse,
    GetTrue,
    SetCond, // true of false
    GetArg,
}

impl fmt::Display for OpType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Module => write!(f, "module"),
            Self::Function => write!(f, "function"),
            Self::If => write!(f, "if"),
            Self::IfElse => write!(f, "if_else"),
            Self::While => write!(f, "while"),
            Self::Loop => write!(f, "loop"),
            Self::ICmp => write!(f, "icmp"),
            Self::LCmp => write!(f, "lcmp"),
            Self::FCmp => write!(f, "fcmp"),
            Self::Proceed => write!(f, "proceed"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Branch => write!(f, "branch"),
            Self::Return => write!(f, "return"),
            Self::Alloca => write!(f, "alloca"),
            Self::Store => write!(f, "store"),
            Self::Load => write!(f, "load"),
            Self::DeclGlobal => write!(f, "decl_global"),
            Self::Neg => write!(f, "neg"),
            Self::FNeg => write!(f, "fneg"),
            Self::Add => write!(f, "add"),
            Self::LAdd => write!(f, "ladd"),
            Self::FAdd => write!(f, "fadd"),
            Self::Sub => write!(f, "sub"),
            Self::LSub => write!(f, "lsub"),
            Self::FSub => write!(f, "fsub"),
            Self::Mul => write!(f, "mul"),
            Self::LMul => write!(f, "lmul"),
            Self::FMul => write!(f, "fmul"),
            Self::Div => write!(f, "div"),
            Self::LDiv => write!(f, "ldiv"),
            Self::FDiv => write!(f, "fdiv"),
            Self::Mod => write!(f, "mod"),
            Self::LMod => write!(f, "lmod"),
            Self::FMod => write!(f, "fmod"),
            Self::BXor => write!(f, "bxor"),
            Self::Xor => write!(f, "xor"),
            Self::LXor => write!(f, "lxor"),
            Self::F2I => write!(f, "f2i"),
            Self::I2F => write!(f, "i2f"),
            Self::ZEXT => write!(f, "zext"),
            Self::SEXT => write!(f, "sext"),
            Self::Ptr2Int => write!(f, "ptrtoint"),
            Self::Int2Ptr => write!(f, "inttoptr"),
            Self::FuncCall => write!(f, "func_call"),
            Self::Phi => write!(f, "phi"),
            Self::GetI => write!(f, "int"),
            Self::GetArrayI => write!(f, "int_array"),
            Self::GetL => write!(f, "long"),
            Self::GetArrayL => write!(f, "long_array"),
            Self::GetF => write!(f, "float"),
            Self::GetArrayF => write!(f, "float_array"),
            Self::GetTrue => write!(f, "true"),
            Self::GetFalse => write!(f, "false"),
            Self::SetCond => write!(f, "set_cond"),
            Self::GetArg => write!(f, "getarg"),
        }
    }
}
