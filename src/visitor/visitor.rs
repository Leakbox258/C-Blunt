use crate::parser::ast::{self};
use crate::scf::attr::CondFlag;
use crate::scf::block::Block;
use crate::scf::r#macro::*;
use crate::scf::no_wrap::*;
use crate::scf::region::Region;
use crate::scf::value::{Type, Value};
use crate::scf::{attr::Attr, operation::*};
use core::panic;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Shared<T> = Rc<RefCell<T>>;
pub type ParseredNode = Option<Value>;

pub trait Visitor {
    fn visit_compunit(&mut self, compunit: &ast::CompUnit);
    fn visit_decl(&mut self, decl: &ast::VarDecl);
    fn visit_var_def(&mut self, ty: &ast::BType, var_def: &ast::VarDef);
    fn visit_func_def(&mut self, func_def: &ast::FuncDef) -> ParseredNode;
    fn visit_func_format_params(&mut self, ffps: &Vec<ast::FuncFParam>) -> Attr;
    fn visit_block(&mut self, block: &ast::Block);
    fn visit_stmt(&mut self, stmt: &ast::Stmt);
    fn visit_unaryexpr(&mut self, unary: &Rc<ast::UnaryExpr>) -> ParseredNode;
    fn visit_binaryexpr<T: Any>(&mut self, binary: &Rc<ast::BinaryExpr<T>>) -> ParseredNode;
    fn visit_const_unaryexpr(&mut self, cunary: &Rc<ast::UnaryExpr>) -> Option<ast::Num>;
    fn visit_const_binaryexpr<T: Any>(
        &mut self,
        cexpr: &Rc<ast::BinaryExpr<T>>,
    ) -> Option<ast::Num>;
    fn visit_lval(&mut self, lval: &ast::LVal) -> (Value, bool); // (value, bool : is_ptr)
}

pub struct Scope {
    pub block: Shared<Block>,
    pub table: HashMap<String, (Value, Type)>, // ident -> alloca or -> constant or -> funcdef
    pub pre_const_fold: HashMap<String, ast::Num>, // infer: Lval -> constant
}

impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        self.block == other.block
    }
}

pub struct SymbolTable {
    tables: Vec<Shared<Scope>>,
}

impl SymbolTable {
    pub fn new(global_block: &Shared<Block>) -> SymbolTable {
        let new_scope = Rc::new(RefCell::new(Scope {
            block: Rc::clone(global_block),
            table: HashMap::new(),
            pre_const_fold: HashMap::new(),
        }));

        let mut tables = Vec::new();
        tables.push(Rc::clone(&new_scope));

        SymbolTable { tables: tables }
    }

    pub fn query(&self, ident: &String) -> Option<(Value, Type)> {
        for scope in self.tables.iter().rev() {
            if let Some(value) = scope.borrow().table.get(ident) {
                return Some(value.clone());
            }
        }

        None
    }

    pub fn push_scope(&mut self, new_block: &Shared<Block>) {
        let new_scope = Rc::new(RefCell::new(Scope {
            block: Rc::clone(new_block),
            table: HashMap::new(),
            pre_const_fold: HashMap::new(),
        }));

        self.tables.push(Rc::clone(&new_scope));
    }

    pub fn pop_scope(&mut self) {
        self.tables.pop();
    }

    pub fn add_symbol(&mut self, ident: String, symbol: Value, ty: Type) {
        self.tables
            .last()
            .unwrap()
            .borrow_mut()
            .table
            .insert(ident, (symbol, ty));
    }

    pub fn modify_symbol_constant_i32(&mut self, ident: &String, new_value: i32) {
        for scope in self.tables.iter().rev() {
            if let Some(value) = scope.borrow_mut().pre_const_fold.get_mut(ident) {
                *value = ast::Num::Int(new_value);
                break;
            }
        }

        // create new
        for scope in self.tables.iter().rev() {
            let exists = { scope.borrow().table.contains_key(ident) };
            if exists {
                scope
                    .borrow_mut()
                    .pre_const_fold
                    .insert(ident.to_string(), ast::Num::Int(new_value));
            }
        }
    }

    pub fn modify_symbol_constant_f32(&mut self, ident: &String, new_value: f32) {
        for scope in self.tables.iter().rev() {
            if let Some(value) = scope.borrow_mut().pre_const_fold.get_mut(ident) {
                *value = ast::Num::Float(new_value);
                break;
            }
        }

        // create new
        for scope in self.tables.iter().rev() {
            let exists = { scope.borrow().table.contains_key(ident) };
            if exists {
                scope
                    .borrow_mut()
                    .pre_const_fold
                    .insert(ident.to_string(), ast::Num::Float(new_value));
            }
        }
    }

    pub fn query_symbol_constant(&self, ident: &String) -> Option<ast::Num> {
        for scope in self.tables.iter().rev() {
            if let Some(value) = scope.borrow().pre_const_fold.get(ident) {
                return Some(value.clone());
            }
        }

        None
    }
}

pub struct Builder {
    ops_stack: Vec<Shared<Operation>>,
    blocks_stack: Vec<Shared<Block>>,
    regions_stack: Vec<Shared<Region>>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            ops_stack: Vec::new(),
            blocks_stack: Vec::new(),
            regions_stack: Vec::new(),
        }
    }

    // operation, though operation get a default region, usually push/pop it explicitly
    pub fn parent_op(&self) -> &Shared<Operation> {
        self.ops_stack.last().unwrap()
    }

    pub fn push_op(&mut self, op: &Shared<Operation>) {
        self.ops_stack.push(Rc::clone(op));
    }

    pub fn pop_op(&mut self) {
        self.ops_stack.pop();
    }

    // block
    pub fn parent_block(&self) -> &Shared<Block> {
        self.blocks_stack.last().unwrap()
    }

    pub fn parent_block_mut(&mut self) -> &mut Shared<Block> {
        self.blocks_stack.last_mut().unwrap()
    }

    pub fn push_block(&mut self, block: &Shared<Block>) {
        self.blocks_stack.push(Rc::clone(block));
    }

    pub fn pop_block(&mut self) {
        self.blocks_stack.pop();
    }

    // region
    pub fn parent_region(&self) -> &Shared<Region> {
        self.regions_stack.last().unwrap()
    }

    pub fn push_region(&mut self, region: &Shared<Region>) {
        self.regions_stack.push(Rc::clone(region));
    }

    pub fn pop_region(&mut self) {
        self.regions_stack.pop();
    }

    pub fn new_op(&mut self, id: u64, ty: Type, opty: OpType, rid: u64) -> Shared<Operation> {
        let mut new_op = Operation::new(id, ty, opty, rid);
        new_op.set_parent(self.parent_block().clone());
        new_op
    }

    pub fn new_block(&mut self, id: u64) -> Shared<Block> {
        let new_block = Block::new(id, &Rc::clone(self.parent_region()));
        new_block
    }

    pub fn new_region(&mut self, id: u64) -> Shared<Region> {
        let new_region = Region::new(id, &Rc::clone(self.parent_op()));
        new_region
    }

    pub fn is_global(&self) -> bool {
        self.parent_op().get_optype() == OpType::Module
    }
}

pub struct MLIRGen {
    module: Rc<RefCell<Operation>>,
    id_cnt: u64,
    builder: Builder,
    symbols: SymbolTable,
}

impl MLIRGen {
    pub fn new() -> MLIRGen {
        let module = Operation::new_module(0, 1);
        let global_region = module.get_default_region();
        let global_block = global_region.get_entry_block();

        let mut builder = Builder::new();

        builder.push_op(&module);
        builder.push_region(&global_region);
        builder.push_block(&global_block);

        MLIRGen {
            module: module,
            id_cnt: 3,
            builder: builder,
            symbols: SymbolTable::new(&global_block),
        }
    }

    pub fn get_module(&self) -> Shared<Operation> {
        Rc::clone(&self.module)
    }

    fn help_flatten_initval(
        &mut self,
        bty: &ast::BType,
        sizes: Vec<usize>,
        init: &ast::InitVal,
    ) -> Vec<Option<Value>> {
        let mut result = Vec::new();
        let total_size: usize = sizes.iter().product();

        // 递归辅助函数
        fn helper(
            obj: &mut MLIRGen,
            val: &ast::InitVal,
            sizes: &[usize],
            curr_layer: usize,
            result: &mut Vec<Option<Value>>,
        ) {
            match val {
                ast::InitVal::Simple(expr) => {
                    // 基本类型直接添加
                    result.push(obj.visit_binaryexpr(expr));
                }
                ast::InitVal::Array(arr) => {
                    if curr_layer >= sizes.len() {
                        return; // 超出维度范围
                    }

                    let expected_size = sizes[curr_layer];
                    let mut filled = 0;

                    // 处理当前层的所有元素
                    for v in arr {
                        helper(obj, v, sizes, curr_layer + 1, result);
                        filled += 1;
                    }

                    // 不足部分补0
                    while filled < expected_size {
                        if curr_layer == sizes.len() - 1 {
                            // 最内层补0
                            result.push(None);
                        } else {
                            // 递归补充子数组的0
                            let sub_size: usize = sizes[curr_layer + 1..].iter().product();
                            for _ in 0..sub_size {
                                result.push(None);
                            }
                        }
                        filled += 1;
                    }
                }
                ast::InitVal::EmptyArray => {
                    // 空数组的情况,补充这一层对应的所有0
                    if curr_layer >= sizes.len() {
                        return;
                    }
                    let sub_size = if curr_layer == sizes.len() - 1 {
                        1
                    } else {
                        sizes[curr_layer + 1..].iter().product()
                    };
                    for _ in 0..sizes[curr_layer] * sub_size {
                        result.push(None);
                    }
                }
            }
        }

        helper(self, init, &sizes, 0, &mut result);

        // 确保结果大小正确
        while result.len() < total_size {
            result.push(None);
        }

        match bty {
            ast::BType::Int => {
                result = result
                    .iter()
                    .map(|ori| {
                        if let Some(init) = ori {
                            match init.get_type() {
                                Type::Int32 => ori.clone(),
                                Type::Float32 => {
                                    let mut convert_op = self.new_op(&Type::Int32, &OpType::F2I);
                                    convert_op.add_operand(init.clone());
                                    Some(Value { def: convert_op })
                                }
                                _ => panic!(),
                            }
                        } else {
                            None
                        }
                    })
                    .collect();
            }
            ast::BType::Float => {
                result = result
                    .iter()
                    .map(|ori| {
                        if let Some(init) = ori {
                            match init.get_type() {
                                Type::Float32 => ori.clone(),
                                Type::Int32 => {
                                    let mut convert_op = self.new_op(&Type::Float32, &OpType::I2F);
                                    convert_op.add_operand(init.clone());
                                    Some(Value { def: convert_op })
                                }
                                _ => panic!(),
                            }
                        } else {
                            None
                        }
                    })
                    .collect()
            }
            _ => panic!(),
        }

        result.truncate(total_size);

        result
    }

    fn help_flatten_const_initval(
        &mut self,
        bty: &ast::BType,
        sizes: &Vec<usize>,
        init: &ast::InitVal,
    ) -> Vec<ast::Num> {
        let mut result = Vec::new();
        let total_size: usize = sizes.iter().product();

        // 递归辅助函数
        fn helper(
            obj: &mut MLIRGen,
            val: &ast::InitVal,
            sizes: &[usize],
            curr_layer: usize,
            result: &mut Vec<ast::Num>,
        ) {
            match val {
                ast::InitVal::Simple(expr) => {
                    // 基本类型直接添加
                    if let Some(v) = obj.visit_const_binaryexpr(expr) {
                        result.push(v);
                    }
                }
                ast::InitVal::Array(arr) => {
                    if curr_layer >= sizes.len() {
                        return; // 超出维度范围
                    }

                    let expected_size = sizes[curr_layer];
                    let mut filled = 0;

                    // 处理当前层的所有元素
                    for v in arr {
                        helper(obj, v, sizes, curr_layer + 1, result);
                        filled += 1;
                    }

                    // 不足部分补0
                    while filled < expected_size {
                        if curr_layer == sizes.len() - 1 {
                            // 最内层补0
                            result.push(ast::Num::Int(0));
                        } else {
                            // 递归补充子数组的0
                            let sub_size: usize = sizes[curr_layer + 1..].iter().product();
                            for _ in 0..sub_size {
                                result.push(ast::Num::Int(0));
                            }
                        }
                        filled += 1;
                    }
                }
                ast::InitVal::EmptyArray => {
                    // 空数组的情况,补充这一层对应的所有0
                    if curr_layer >= sizes.len() {
                        return;
                    }
                    let sub_size = if curr_layer == sizes.len() - 1 {
                        1
                    } else {
                        sizes[curr_layer + 1..].iter().product()
                    };
                    for _ in 0..sizes[curr_layer] * sub_size {
                        result.push(ast::Num::Int(0));
                    }
                }
            }
        }

        helper(self, init, &sizes, 0, &mut result);

        // 确保结果大小正确
        while result.len() < total_size {
            result.push(ast::Num::Int(0));
        }

        match bty {
            ast::BType::Int => {
                result = result
                    .iter()
                    .map(|num| match num {
                        ast::Num::Int(i) => ast::Num::Int(*i),
                        ast::Num::Float(f) => ast::Num::Int(*f as i32),
                    })
                    .collect();
            }
            ast::BType::Float => {
                result = result
                    .iter()
                    .map(|num| match num {
                        ast::Num::Float(f) => ast::Num::Float(*f),
                        ast::Num::Int(i) => ast::Num::Float(*i as f32),
                    })
                    .collect()
            }
            _ => panic!(),
        }

        result.truncate(total_size);

        result
    }

    fn next_id(&mut self) -> u64 {
        let id = self.id_cnt;
        self.id_cnt += 1;
        id
    }

    fn convert_type(&self, oty: &ast::BType) -> Type {
        match oty {
            ast::BType::Float => Type::Float32,
            ast::BType::Int => Type::Int32,
            ast::BType::Void => Type::Void,
        }
    }

    fn convert_op(&self, ty: &Type, op: &ast::Operator) -> OpType {
        match op {
            ast::Operator::Add => match ty {
                Type::Int32 => OpType::Add,
                Type::Int64 => OpType::LAdd,
                Type::Float32 => OpType::FAdd,
                _ => panic!(),
            },
            ast::Operator::Sub => match ty {
                Type::Int32 => OpType::Sub,
                Type::Int64 => OpType::LSub,
                Type::Float32 => OpType::FSub,
                _ => panic!(),
            },
            ast::Operator::Mul => match ty {
                Type::Int32 => OpType::Mul,
                Type::Int64 => OpType::LMul,
                Type::Float32 => OpType::FMul,
                _ => panic!(),
            },
            ast::Operator::Div => match ty {
                Type::Int32 => OpType::Div,
                Type::Int64 => OpType::LDiv,
                Type::Float32 => OpType::FDiv,
                _ => panic!(),
            },
            ast::Operator::Mod => match ty {
                Type::Int32 => OpType::Mod,
                Type::Int64 => OpType::LMod,
                Type::Float32 => OpType::FMod,
                _ => panic!(),
            },
            // need to set attr
            ast::Operator::Ls
            | ast::Operator::Gt
            | ast::Operator::Le
            | ast::Operator::Ge
            | ast::Operator::Eq
            | ast::Operator::Ne => match ty {
                Type::Int32 => OpType::ICmp,
                Type::Int64 => OpType::LCmp,
                Type::Float32 => OpType::FCmp,
                _ => panic!(),
            },
            // Ast::Operator::And => {}
            // Ast::Operator::Or => {}
            _ => panic!("convert_op: unexpected op {:?}", op), // more complex
        }
    }

    fn generate_array_type(&self, oty: &ast::BType, mut sub_defs: Vec<usize>) -> Type {
        if sub_defs.is_empty() {
            Type::Array(Box::new((self.convert_type(oty), None)))
        } else {
            let size = sub_defs.remove(0);
            let inner_type = self.generate_array_type(oty, sub_defs);

            Type::Array(Box::new((inner_type, Some(size))))
        }
    }

    fn new_op(&mut self, ty: &Type, opty: &OpType) -> Shared<Operation> {
        let id = self.next_id();
        let rid = id.clone(); // rid use as the id of the default region of this op
        self.builder.new_op(id, ty.clone(), opty.clone(), rid)
    }

    fn new_int32(&mut self, int: i32) -> Value {
        let mut new_op = self.new_op(&Type::Int32, &OpType::GetI);
        new_op.set_attr(0, Attr::Int32(int));

        Value { def: new_op }
    }

    fn new_int64(&mut self, int: i64) -> Value {
        let mut new_op = self.new_op(&Type::Int32, &OpType::GetL);
        new_op.set_attr(0, Attr::Int64(int));

        Value { def: new_op }
    }

    fn new_type_convert(&mut self, dst: &Type, ori: &Type, val: Value) -> Value {
        let optype;
        if matches!(dst, Type::Ptr(_)) && matches!(ori, Type::Int64) {
            optype = OpType::Int2Ptr;
        } else if matches!(dst, Type::Int64) && matches!(ori, Type::Ptr(_)) {
            optype = OpType::Ptr2Int;
        } else if matches!(dst, Type::Int32) && matches!(ori, Type::Float32) {
            optype = OpType::F2I;
        } else if matches!(dst, Type::Float32) && matches!(ori, Type::Int32) {
            optype = OpType::I2F;
        } else {
            panic!(
                "new_type_convert: cant convert from {} to {}",
                ori.to_string(),
                dst.to_string()
            );
        }

        let mut new_convert = self.new_op(dst, &optype);
        new_convert.add_operand(val);

        Value { def: new_convert }
    }

    fn new_block(&mut self) -> Shared<Block> {
        let bid = self.next_id();
        self.builder.new_block(bid)
    }

    fn new_region(&mut self) -> Shared<Region> {
        let rid = self.next_id();
        self.builder.new_region(rid)
    }

    fn push_region(&mut self, region: &Shared<Region>) {
        self.builder.push_region(region);
        self.push_block(&region.get_entry_block());
    }

    fn push_op(&mut self, op: &Shared<Operation>) {
        self.builder.push_op(op);
    }

    fn push_block(&mut self, block: &Shared<Block>) {
        self.builder.push_block(block);
        self.symbols.push_scope(block);
    }

    fn pop_region(&mut self) {
        self.pop_block();
        self.builder.pop_region();
    }

    fn pop_op(&mut self) {
        self.builder.pop_op();
    }

    fn pop_block(&mut self) {
        self.builder.pop_block();
        self.symbols.pop_scope();
    }

    fn symbol_qeury(&self, ident: &String) -> (Value, Type) {
        match self.symbols.query(ident) {
            Some(pack) => pack,
            None => panic!("symbol_qeury: cant find symbol {}", ident),
        }
    }

    fn symbol_qeury_constant(&self, ident: &String) -> Option<ast::Num> {
        self.symbols.query_symbol_constant(ident)
    }

    fn symbol_add(&mut self, ident: &String, val: Value, ty: &Type) {
        self.symbols.add_symbol(ident.to_string(), val, ty.clone());
    }

    fn symbol_modify_constant_i32(&mut self, ident: &String, constant: i32) {
        self.symbols.modify_symbol_constant_i32(ident, constant);
    }

    fn symbol_modify_constant_f32(&mut self, ident: &String, constant: f32) {
        self.symbols.modify_symbol_constant_f32(ident, constant);
    }

    fn symbol_modify_constant(&mut self, bty: &ast::BType, ident: &String, constant: ast::Num) {
        match bty {
            ast::BType::Int => match constant {
                ast::Num::Int(i32) => {
                    self.symbol_modify_constant_i32(ident, i32);
                }
                ast::Num::Float(f32) => {
                    self.symbol_modify_constant_i32(ident, f32 as i32);
                }
            },
            ast::BType::Float => match constant {
                ast::Num::Int(i32) => {
                    self.symbol_modify_constant_f32(ident, i32 as f32);
                }
                ast::Num::Float(f32) => {
                    self.symbol_modify_constant_f32(ident, f32);
                }
            },
            _ => panic!(),
        }
    }

    fn push_scope(&mut self, new_block: &Shared<Block>) {
        self.symbols.push_scope(new_block);
    }

    fn pop_scope(&mut self) {
        self.symbols.pop_scope();
    }
}

#[macro_export]
macro_rules! fn_decl {
    ( $self : expr, $fn_name : ident ($($arg_name : ident : $arg_ty : expr, )*)) => {{
        let fn_op = $self.new_op(&Type::Void, &OpType::Function);
        fn_op.borrow_mut()
            .set_attr(0, Attr::Name(stringify!($fn_name).to_string()))
            .set_attr(1, Attr::Args(vec![$((stringify!($arg_name).to_string(), $arg_ty)),*]))
            .set_attr(2, Attr::DeclOnly);
        $self.symbol_add(stringify!($fn_name).to_string(), Value { def : fn_op }, &Type::Void);
    }};

    ( $self : expr, $fn_name : ident ($($arg_name : ident : $arg_ty : expr, )*) => $ret : expr ) => {{
        let fn_op = $self.new_op(&$ret, &OpType::Function);
        fn_op.borrow_mut()
            .set_attr(0, Attr::Name(stringify!($fn_name).to_string()))
            .set_attr(1, Attr::Args(vec![$((stringify!($arg_name).to_string(), $arg_ty)),*]))
            .set_attr(2, Attr::DeclOnly);
        $self.symbol_add(&stringify!($fn_name).to_string(), Value { def : fn_op }, &$ret);
    }};

    // for no tailing ','
    ($self:expr, $fn_name:ident ($($arg_name:ident : $arg_ty:expr),*)) => {{
        let fn_op = $self.new_op(&Type::Void, &OpType::Function);
        fn_op.borrow_mut()
            .set_attr(0, Attr::Name(stringify!($fn_name).to_string()))
            .set_attr(1, Attr::Args(vec![$((stringify!($arg_name).to_string(), $arg_ty)),*]))
            .set_attr(2, Attr::DeclOnly);
        $self.symbol_add(&stringify!($fn_name).to_string(), Value { def : fn_op }, &Type::Void);
    }};

    ( $self : expr, $fn_name : ident ($($arg_name : ident : $arg_ty : expr),*) => $ret : expr ) => {{
        let fn_op = $self.new_op(&$ret, &OpType::Function);
        fn_op.borrow_mut()
            .set_attr(0, Attr::Name(stringify!($fn_name).to_string()))
            .set_attr(1, Attr::Args(vec![$((stringify!($arg_name).to_string(), $arg_ty)),*]))
            .set_attr(2, Attr::DeclOnly);
        $self.symbol_add(&stringify!($fn_name).to_string(), Value { def : fn_op }, &$ret);
    }};

    ($self:expr, $fn_name:literal, ($($arg_name:ident : $arg_ty:expr),*)) => {{
        let fn_op = $self.new_op(&Type::Void, &OpType::Function);
        fn_op.borrow_mut()
            .set_attr(0, Attr::Name(stringify!($fn_name)[1..stringify!($fn_name).len() - 1].to_string()))
            .set_attr(1, Attr::Args(vec![$((stringify!($arg_name).to_string(), $arg_ty)),*]))
            .set_attr(2, Attr::DeclOnly);
        $self.symbol_add(&stringify!($fn_name).to_string(), Value { def : fn_op }, &Type::Void);
    }};

}

impl Visitor for MLIRGen {
    fn visit_compunit(&mut self, compunit: &ast::CompUnit) {
        // sylib
        fn_decl!(self, getint() => Type::Int32);
        fn_decl!(self, getch() => Type::Int32);
        fn_decl!(self, getfloat() => Type::Float32);
        fn_decl!(self, putint(a : Type::Int32));
        fn_decl!(self, putch(a : Type::Int32));
        fn_decl!(self, putfloat(a : Type::Float32));
        fn_decl!(self, _sysy_starttime(a : Type::Int32));
        fn_decl!(self, _sysy_stoptime(a : Type::Int32));
        fn_decl!(self, getarray(a : Type::Ptr(Box::new(Type::Int32))) => Type::Int32);
        fn_decl!(self, getfarray(a : Type::Ptr(Box::new(Type::Float32))) => Type::Int32);
        fn_decl!(self, putarray(num : Type::Int32, a : Type::Ptr(Box::new(Type::Int32))));
        fn_decl!(self, putfarray(num : Type::Int32, a : Type::Ptr(Box::new(Type::Float32))));
        // llvm builtin
        fn_decl!(self, "llvm.memset.p0i8.i32",
                (

                    val: Type::Ptr(Box::new(Type::Int8)),
                    fillwith: Type::Int8,
                    size: Type::Int32,
                    isvolatile: Type::Bool
                )
        );

        for node_decl in &compunit.decls {
            self.visit_decl(node_decl);
        }

        for func_def in &compunit.funcs {
            self.visit_func_def(func_def);
        }
    }

    fn visit_decl(&mut self, decl: &ast::VarDecl) {
        let bty = &decl.ty;
        let var_defs = &decl.vardefs;

        for var_def in var_defs {
            self.visit_var_def(bty, var_def);
        }
    }

    fn visit_var_def(&mut self, bty: &ast::BType, var_def: &ast::VarDef) {
        let ident = &var_def.ident;
        let mut subs: Vec<usize> = Vec::new();
        let ty = self.convert_type(bty);

        // handle declare subscripts
        for decl_size in var_def.subscripts.clone().unwrap_or(vec![]) {
            subs.push(
                self.visit_const_binaryexpr(&decl_size)
                    .unwrap()
                    .as_int()
                    .unwrap()
                    .try_into()
                    .unwrap(),
            );
        }

        let total_elems = subs.iter().fold(1, |acc, &elem| acc * elem);

        if self.builder.is_global() {
            // get init attrs

            let decl_init_attr;
            if var_def.init.is_none()
                || matches!(var_def.init.as_ref().unwrap(), ast::InitVal::EmptyArray)
            {
                // default all zero

                match bty {
                    ast::BType::Int => decl_init_attr = Attr::IntArray(vec![0; total_elems]),
                    ast::BType::Float => decl_init_attr = Attr::FloatArray(vec![0.0; total_elems]),
                    _ => panic!(),
                }
            } else if matches!(var_def.init.as_ref().unwrap(), ast::InitVal::Array(_)) {
                let init_array = var_def.init.as_ref().unwrap();

                let flatten_array = self.help_flatten_const_initval(bty, &subs, init_array);

                match bty {
                    ast::BType::Int => {
                        decl_init_attr = Attr::IntArray(
                            flatten_array
                                .iter()
                                .map(|num| match num {
                                    ast::Num::Int(i32) => *i32,
                                    _ => panic!(),
                                })
                                .collect(),
                        )
                    }
                    ast::BType::Float => {
                        decl_init_attr = Attr::FloatArray(
                            flatten_array
                                .iter()
                                .map(|num| match num {
                                    ast::Num::Float(f32) => *f32,
                                    _ => panic!(),
                                })
                                .collect(),
                        )
                    }
                    _ => panic!(),
                };
            } else if let ast::InitVal::Simple(init_expr) = var_def.init.as_ref().unwrap() {
                let num = self.visit_const_binaryexpr(init_expr).unwrap();

                match bty {
                    ast::BType::Int => match num {
                        ast::Num::Int(i32) => {
                            decl_init_attr = Attr::Int32(i32);
                        }
                        ast::Num::Float(f32) => {
                            decl_init_attr = Attr::Int32(f32 as i32);
                        }
                    },
                    ast::BType::Float => match num {
                        ast::Num::Int(i32) => {
                            decl_init_attr = Attr::Float(i32 as f32);
                        }
                        ast::Num::Float(f32) => {
                            decl_init_attr = Attr::Float(f32);
                        }
                    },
                    _ => panic!(),
                }
                self.symbol_modify_constant(bty, ident, num);
            } else {
                panic!();
            }

            let mut global_decl;

            if var_def.subscripts.is_some() {
                global_decl = self.new_op(
                    &Type::new_array_type(&Type::Ptr(Box::new(ty.clone())), &subs),
                    &OpType::DeclGlobal,
                );
                global_decl.set_attr(4, Attr::ArrayShape(subs)); // for lowering to llvm ir
            } else {
                global_decl = self.new_op(
                    &Type::new_ptr_type(&Type::Ptr(Box::new(ty.clone())), &subs),
                    &OpType::DeclGlobal,
                );
            }

            global_decl
                .set_attr(0, Attr::Name(ident.clone()))
                .set_attr(1, Attr::Size(total_elems))
                .set_attr(2, decl_init_attr)
                .set_attr(3, Attr::Align(4));

            self.symbol_add(ident, Value { def: global_decl }, &ty);
        } else {
            // 1. alloca
            let mut alloc_op;

            if var_def.subscripts.is_some() {
                alloc_op = self.new_op(&Type::new_array_type(&ty, &subs), &OpType::Alloca);
            } else {
                alloc_op = self.new_op(&&Type::new_ptr_type(&ty, &subs), &OpType::Alloca)
            }

            alloc_op
                .set_attr(0, Attr::Size(total_elems * 4))
                .set_attr(1, Attr::Align(4));

            self.symbol_add(
                ident,
                Value {
                    def: alloc_op.clone(),
                },
                &self.convert_type(bty),
            );

            // 2. memset or store
            if total_elems > 1 {
                let get_0 = self.new_int32(0);
                let get_sizes = self.new_int32((total_elems * 4) as i32);

                self.new_op(&Type::Void, &OpType::FuncCall)
                    .add_operand(Value {
                        def: alloc_op.clone(),
                    })
                    .add_operand(get_0)
                    .add_operand(get_sizes)
                    .set_attr(0, Attr::Name("memset".to_string()));

                // gep & store
                if var_def.init.is_some()
                    && !matches!(var_def.init.as_ref().unwrap(), ast::InitVal::EmptyArray)
                {
                    let init_array = var_def.init.as_ref().unwrap();

                    assert!(!matches!(init_array, ast::InitVal::Simple(_)));

                    let mut to_int64_op = self.new_op(&Type::Int64, &OpType::Ptr2Int);

                    to_int64_op.add_operand(Value {
                        def: Rc::clone(&alloc_op),
                    });

                    let mut elem_cnt = 0;
                    for init_value in self.help_flatten_initval(bty, subs, init_array) {
                        match init_value {
                            None => {
                                elem_cnt += 1;
                            }
                            Some(value) => {
                                let offset_op = self.new_int64(elem_cnt * 4);
                                let mut address_op = self.new_op(&Type::Int64, &OpType::LAdd);
                                address_op
                                    .add_operand(Value {
                                        def: Rc::clone(&to_int64_op),
                                    })
                                    .add_operand(offset_op);

                                let mut int2ptr_op =
                                    self.new_op(&Type::Ptr(Box::new(ty.clone())), &OpType::Int2Ptr);
                                int2ptr_op.add_operand(Value { def: address_op });

                                self.new_op(&Type::Void, &OpType::Store)
                                    .set_attr(0, Attr::Size(4))
                                    .set_attr(1, Attr::Align(4))
                                    .add_operand(value)
                                    .add_operand(Value { def: int2ptr_op });
                                elem_cnt += 1;
                            }
                        }
                    }
                }
            } else {
                let init_op;
                if var_def.init.is_none() {
                    init_op = self.new_int32(0);
                } else {
                    // extract expr
                    let mut last_init_array = var_def.init.as_ref().unwrap();

                    while let ast::InitVal::Array(init_array) = last_init_array {
                        last_init_array = &init_array[0];
                        // dump others
                    }

                    match last_init_array {
                        ast::InitVal::EmptyArray => {
                            init_op = self.new_int32(0);
                            match bty {
                                ast::BType::Int => self.symbol_modify_constant_i32(ident, 0),
                                ast::BType::Float => self.symbol_modify_constant_f32(ident, 0.0),
                                _ => panic!(),
                            }
                        }
                        ast::InitVal::Simple(expr) => {
                            init_op = self.visit_binaryexpr(expr).unwrap();

                            if let Some(num) = self.visit_const_binaryexpr(expr) {
                                self.symbol_modify_constant(bty, ident, num);
                            }
                        }
                        _ => panic!(),
                    };
                }

                self.new_op(&Type::Void, &OpType::Store)
                    .set_attr(0, Attr::Size(4))
                    .set_attr(1, Attr::Align(4))
                    .add_operand(init_op)
                    .add_operand(Value { def: alloc_op });
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &ast::Stmt) {
        if self.builder.is_global() {
            panic!();
        }

        match stmt {
            ast::Stmt::NULL => {}
            ast::Stmt::Expression(expr) => {
                let _ = self.visit_binaryexpr(expr);
            }
            ast::Stmt::Assign(lval, expr) => {
                assert!(!self.builder.is_global());

                let (lval_op, is_ptr) = self.visit_lval(lval);

                let mut expr_op = self.visit_binaryexpr(expr).unwrap();

                // type convert
                if is_ptr && value_ptr_defref!(lval_op) != value_type!(expr_op) {
                    expr_op = self.new_type_convert(
                        &value_ptr_defref!(lval_op),
                        &value_type!(expr_op),
                        expr_op,
                    );
                } else if !is_ptr && value_type!(lval_op) != value_type!(expr_op) {
                    expr_op = self.new_type_convert(
                        &value_type!(lval_op),
                        &value_type!(expr_op),
                        expr_op,
                    );
                }

                self.new_op(&Type::Void, &OpType::Store)
                    .set_attr(0, Attr::Size(4))
                    .set_attr(1, Attr::Align(4))
                    .add_operand(expr_op)
                    .add_operand(lval_op);

                // try pre-constfolding

                if let Some(new_i32) = self.visit_const_binaryexpr(expr) {
                    self.symbol_modify_constant_i32(&lval.ident, new_i32.as_int().unwrap());
                }
            }
            ast::Stmt::Block(block) => {
                let _ = self.visit_block(block);
            }
            ast::Stmt::IfElse(cond, then, or) => {
                let cmp = self.visit_binaryexpr(cond).unwrap();

                let mut if_else_op = self.new_op(
                    &Type::Void,
                    if or.is_some() {
                        &OpType::IfElse
                    } else {
                        &OpType::If
                    },
                );

                if_else_op.add_operand(cmp);

                self.push_op(&if_else_op);

                // generally the region or if_else should push above regions of then and or
                // but actually you cann't assign lval in cond stmt, so it's not neccessary

                // then region
                // let then_region = self.new_region();
                let then_region = if_else_op.get_default_region();

                self.push_region(&then_region);

                self.visit_stmt(&then);

                self.pop_region();

                // or region
                if or.is_some() {
                    let or_region = self.new_region();

                    self.push_region(&or_region);

                    self.visit_stmt(&or.as_ref().unwrap());

                    self.pop_region();
                }

                self.pop_op();
            }
            ast::Stmt::While(cond, body) => {
                let while_op = self.new_op(&Type::Void, &OpType::While);

                self.push_op(&while_op);

                // header region
                let header_region = while_op.get_default_region();

                self.push_region(&header_region);

                let cmp = self.visit_binaryexpr(cond).unwrap();

                self.new_op(&Type::Void, &OpType::Proceed).add_operand(cmp);

                self.pop_region();

                // body region
                let body_region = self.new_region();

                self.push_region(&body_region);

                let _ = self.visit_stmt(&body);

                self.pop_region();

                // clean up
                self.pop_op();
            }
            ast::Stmt::Break => {
                let _ = self.new_op(&Type::Void, &OpType::Break);
            }
            ast::Stmt::Continue => {
                let _ = self.new_op(&Type::Void, &OpType::Continue);
            }
            ast::Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let return_val = self.visit_binaryexpr(expr).unwrap();

                    self.new_op(&return_val.get_type(), &OpType::Return)
                        .add_operand(return_val);
                } else {
                    let _ = self.new_op(&Type::Void, &OpType::Return);
                }
            }
        };
    }

    fn visit_func_def(&mut self, func_def: &ast::FuncDef) -> ParseredNode {
        let mut new_func = self.new_op(&self.convert_type(&func_def.ty), &OpType::Function);

        let default_region = new_func.get_default_region();

        self.push_op(&new_func);
        self.push_region(&default_region);

        new_func
            .set_attr(0, Attr::Name(func_def.name.clone()))
            .set_attr(1, self.visit_func_format_params(&func_def.params));

        for block_item in func_def.body.as_ref().unwrap_or(&vec![]) {
            match block_item {
                ast::BlockItem::Declare(decl) => {
                    let _ = self.visit_decl(&decl);
                }
                ast::BlockItem::Statement(stmt) => {
                    let _ = self.visit_stmt(&stmt);
                }
            }
        }

        self.pop_region();
        self.pop_op();

        let value = Value { def: new_func };

        self.symbol_add(
            &func_def.name,
            value.clone(),
            &self.convert_type(&func_def.ty),
        );

        Some(value)
    }

    // get Attr & get symbols
    fn visit_func_format_params(&mut self, ffps: &Vec<ast::FuncFParam>) -> Attr {
        let mut args = Vec::new();

        let mut seq = 0;
        for format_arg in ffps {
            let mut sizes: Vec<usize> = Vec::new();

            for size in format_arg.dimensions.as_ref().unwrap_or(&vec![]) {
                match size {
                    ast::Dimension::Deduce => {
                        // deduce
                        sizes.push(i32::MAX as usize);
                    }
                    ast::Dimension::Declare(cexpr) => {
                        // handle as const expr
                        sizes.push(
                            self.visit_const_binaryexpr(cexpr)
                                .unwrap()
                                .as_int()
                                .unwrap() as usize,
                        );
                    }
                }
            }

            let ty;
            if sizes.is_empty() {
                ty = self.convert_type(&format_arg.ty);
            } else {
                ty = self.generate_array_type(&format_arg.ty, sizes);
            }

            let ident = format_arg.name.clone();

            let mut getarg_op = self.new_op(&ty, &OpType::GetArg);
            getarg_op.set_attr(0, Attr::ArgSeq(seq));

            self.symbol_add(&ident, Value { def: getarg_op }, &ty);
            args.push((ident, ty));
            seq += 1;
        }

        Attr::Args(args)
    }

    fn visit_const_unaryexpr(&mut self, cunary: &Rc<ast::UnaryExpr>) -> Option<ast::Num> {
        let inner = &cunary.inner;
        let next = &cunary.next;
        let op = &cunary.op;

        let infered_constant;
        match inner {
            Some(paren) => match paren {
                ast::ParenExpr::FunctionCall(_) => {
                    infered_constant = None;
                }
                ast::ParenExpr::Expression(prime) => match prime.as_ref() {
                    ast::PrimaryExpr::Expression(cexpr) => {
                        infered_constant = self.visit_const_binaryexpr(cexpr);
                    }
                    ast::PrimaryExpr::LValue(lval) => {
                        // check symbol table
                        let ident = &lval.ident;

                        infered_constant = self.symbol_qeury_constant(ident);
                    }
                    ast::PrimaryExpr::Number(num) => match num {
                        ast::Num::Int(i32) => infered_constant = Some(ast::Num::Int(*i32)),
                        ast::Num::Float(f32) => infered_constant = Some(ast::Num::Float(*f32)),
                    },
                },
            },
            None => {
                // handle inner unary
                match op.as_ref().unwrap() {
                    ast::Operator::Pos => {
                        match self.visit_const_unaryexpr(&next.as_ref().unwrap()).unwrap() {
                            ast::Num::Int(i32) => infered_constant = Some(ast::Num::Int(i32)),
                            ast::Num::Float(f32) => infered_constant = Some(ast::Num::Float(f32)),
                        }
                    }
                    ast::Operator::Neg => {
                        match self.visit_const_unaryexpr(&next.as_ref().unwrap()).unwrap() {
                            ast::Num::Int(i32) => infered_constant = Some(ast::Num::Int(i32 * -1)),
                            ast::Num::Float(f32) => {
                                infered_constant = Some(ast::Num::Float(f32 * -1.0))
                            }
                        }
                    }
                    ast::Operator::Not => {
                        match self.visit_const_unaryexpr(&next.as_ref().unwrap()).unwrap() {
                            ast::Num::Int(i32) => infered_constant = Some(ast::Num::Int(!i32)),
                            ast::Num::Float(f32) => {
                                infered_constant =
                                    Some(ast::Num::Float(if f32 == 0.0 { 1.0 } else { 0.0 }))
                            }
                        }
                    }

                    _ => panic!(),
                }
            }
        };

        infered_constant
    }

    fn visit_const_binaryexpr<T: Any>(
        &mut self,
        cexpr: &Rc<ast::BinaryExpr<T>>,
    ) -> Option<ast::Num> {
        let lhs_ast = &cexpr.lhs;
        let rhs_ast = &cexpr.rhs as &dyn Any;

        let mut lhs = None;
        if lhs_ast.is_some() {
            lhs = self.visit_const_binaryexpr(lhs_ast.as_ref().unwrap());
        }

        let rhs;
        if let Some(unary_expr) = rhs_ast.downcast_ref::<Rc<ast::UnaryExpr>>() {
            rhs = self.visit_const_unaryexpr(unary_expr);
        } else if let Some(mul_expr) = rhs_ast.downcast_ref::<Rc<ast::MulExpr>>() {
            rhs = self.visit_const_binaryexpr(mul_expr);
        } else if let Some(add_expr) = rhs_ast.downcast_ref::<Rc<ast::AddExpr>>() {
            rhs = self.visit_const_binaryexpr(add_expr);
        } else if let Some(rel_expr) = rhs_ast.downcast_ref::<Rc<ast::RelExpr>>() {
            rhs = self.visit_const_binaryexpr(rel_expr);
        } else if let Some(eq_expr) = rhs_ast.downcast_ref::<Rc<ast::EqExpr>>() {
            rhs = self.visit_const_binaryexpr(eq_expr);
        } else if let Some(land_expr) = rhs_ast.downcast_ref::<Rc<ast::LAndExpr>>() {
            rhs = self.visit_const_binaryexpr(land_expr);
        } else {
            panic!()
        }

        if lhs.is_none() {
            return rhs;
        }

        let op = cexpr.op.as_ref().unwrap();

        Some(ast::apply_binary_operation(
            lhs.as_ref().unwrap(),
            rhs.as_ref().unwrap(),
            op,
        ))
    }

    fn visit_block(&mut self, block: &ast::Block) {
        // TODO: try flat block into the default region and its block

        let new_block = self.new_block();
        self.push_scope(&new_block);

        if block.items.is_some() {
            for block_item in block.items.as_ref().unwrap() {
                match block_item {
                    ast::BlockItem::Declare(decl) => {
                        let _ = self.visit_decl(&decl);
                    }
                    ast::BlockItem::Statement(stmt) => {
                        let _ = self.visit_stmt(&stmt);
                    }
                }
            }
        }

        self.pop_scope();
    }

    fn visit_unaryexpr(&mut self, unary: &Rc<ast::UnaryExpr>) -> ParseredNode {
        let inner = &unary.inner;
        let op = &unary.op;

        match inner {
            Some(paren) => match paren {
                ast::ParenExpr::FunctionCall(call) => {
                    let func_def_op = self.symbol_qeury(&call.ident).0;

                    let mut param_exprs = vec![];
                    if call.params.is_some() {
                        for real_arg in call.params.as_ref().unwrap() {
                            let arg_op = self.visit_binaryexpr(&real_arg).unwrap();

                            param_exprs.push(arg_op);
                            // call_op.borrow_mut().add_operands(arg_op);
                        }
                    }

                    let mut call_op = self.new_op(&func_def_op.get_type(), &OpType::FuncCall);

                    call_op
                        .add_operand(func_def_op)
                        .set_attr(0, Attr::Name(call.ident.clone()));

                    call_op.append_operands(&mut param_exprs);

                    Some(Value { def: call_op })
                }
                ast::ParenExpr::Expression(prime) => match prime.as_ref() {
                    ast::PrimaryExpr::Expression(expr) => self.visit_binaryexpr(expr),
                    ast::PrimaryExpr::LValue(lval) => {
                        let (lval_op, is_ptr) = self.visit_lval(lval);

                        if is_ptr {
                            let mut load = self.new_op(&value_ptr_defref!(lval_op), &OpType::Load);
                            load.set_attr(0, Attr::Size(4))
                                .set_attr(1, Attr::Align(4))
                                .add_operand(lval_op);
                            Some(Value { def: load })
                        } else {
                            Some(lval_op)
                        }
                    }
                    ast::PrimaryExpr::Number(num) => match num {
                        ast::Num::Int(i32) => {
                            let mut get_int_op = self.new_op(&Type::Int32, &OpType::GetI);
                            get_int_op.set_attr(0, Attr::Int32(*i32));

                            Some(Value { def: get_int_op })
                        }
                        ast::Num::Float(f32) => {
                            let mut get_float_op = self.new_op(&Type::Float32, &OpType::GetF);
                            get_float_op.set_attr(0, Attr::Float(*f32));

                            Some(Value { def: get_float_op })
                        }
                    },
                },
            },
            None => {
                // handle inner unary
                match op.as_ref().unwrap() {
                    ast::Operator::Pos => self.visit_unaryexpr(&unary.next.as_ref().unwrap()),
                    ast::Operator::Neg => {
                        let next_unary =
                            self.visit_unaryexpr(&unary.next.as_ref().unwrap()).unwrap();

                        let opty;
                        match next_unary.get_type() {
                            Type::Int32 => {
                                opty = OpType::Neg;
                            }
                            Type::Float32 => {
                                opty = OpType::FNeg;
                            }
                            _ => panic!(),
                        };

                        let mut new_unary = self.new_op(&next_unary.get_type(), &opty);
                        new_unary.add_operand(next_unary);

                        Some(Value { def: new_unary })
                    }
                    ast::Operator::Not => {
                        let next_unary = self.visit_unaryexpr(&unary).unwrap();

                        let opty;
                        let ty;
                        match next_unary.get_type() {
                            Type::Int32 => {
                                opty = OpType::ICmp;
                                ty = Type::Int32;
                            }
                            Type::Float32 => {
                                opty = OpType::FCmp;
                                ty = Type::Float32
                            }
                            _ => panic!(),
                        };

                        let mut cmp = self.new_op(&Type::Bool, &opty);
                        cmp.add_operand(next_unary);

                        let mut xor = self.new_op(&Type::Bool, &OpType::Xor);
                        xor.add_operand(Value { def: cmp });

                        let mut zext = self.new_op(&Type::Int32, &OpType::ZEXT);
                        zext.add_operand(Value { def: xor });

                        if ty == Type::Float32 {
                            let mut i2f = self.new_op(&Type::Float32, &OpType::I2F);
                            i2f.add_operand(Value { def: zext });
                            return Some(Value { def: i2f });
                        }

                        Some(Value { def: zext })
                    }

                    _ => panic!(),
                }
            }
        }
    }

    // TODO: consider insert type convert operations
    fn visit_binaryexpr<T: Any>(&mut self, binary: &Rc<ast::BinaryExpr<T>>) -> ParseredNode {
        let lhs_ast = &binary.lhs;
        let rhs_ast = &binary.rhs as &dyn Any;

        let mut rhs_op: Value;

        if let Some(land_expr) = rhs_ast.downcast_ref::<Rc<ast::LAndExpr>>() {
            if lhs_ast.is_none() {
                return self.visit_binaryexpr(land_expr);
            } else {
                // apply short circuit: ||
                let mut alloc_op = self.new_op(&Type::Ptr(Box::new(Type::Bool)), &OpType::Alloca);
                alloc_op
                    .set_attr(0, Attr::Size(4))
                    .set_attr(1, Attr::Align(4));

                let lcmp_op = self.visit_binaryexpr(lhs_ast.as_ref().unwrap()).unwrap(); // bool

                let mut if_else_op = self.new_op(&Type::Ptr(Box::new(Type::Bool)), &OpType::IfElse);

                if_else_op.add_operand(lcmp_op);

                self.push_op(&if_else_op);

                // then_region
                let then_region = self.new_region();

                self.push_region(&then_region);

                let true_op = self.new_op(&Type::Bool, &OpType::GetTrue);

                let mut store_op = self.new_op(&Type::Void, &OpType::Store);
                store_op
                    .set_attr(0, Attr::Size(4))
                    .set_attr(1, Attr::Align(4))
                    .add_operand(Value { def: true_op })
                    .add_operand(Value {
                        def: Rc::clone(&alloc_op),
                    });

                self.pop_region();
                // or_region
                let or_region = self.new_region();

                self.push_region(&or_region);

                let rcmp_op = self.visit_binaryexpr(land_expr).unwrap();

                let mut store_op = self.new_op(&Type::Void, &OpType::Store);
                store_op
                    .set_attr(0, Attr::Size(4))
                    .set_attr(1, Attr::Align(4))
                    .add_operand(rcmp_op)
                    .add_operand(Value {
                        def: Rc::clone(&alloc_op),
                    });

                self.pop_region();

                self.pop_op();

                let mut load_op = self.new_op(&Type::Bool, &OpType::Load);
                load_op
                    .set_attr(0, Attr::Size(4))
                    .set_attr(1, Attr::Align(4))
                    .add_operand(Value {
                        def: Rc::clone(&alloc_op),
                    });

                return Some(Value { def: load_op });
            }
        } else if let Some(eq_expr) = rhs_ast.downcast_ref::<Rc<ast::EqExpr>>() {
            if lhs_ast.is_none() {
                return self.visit_binaryexpr(eq_expr);
            } else {
                // apply short circuit: &&
                let mut alloc_op = self.new_op(&Type::Ptr(Box::new(Type::Bool)), &OpType::Alloca);
                alloc_op
                    .set_attr(0, Attr::Size(4))
                    .set_attr(1, Attr::Align(4));

                let lcmp_op = self.visit_binaryexpr(lhs_ast.as_ref().unwrap()).unwrap(); // bool

                let mut if_else_op = self.new_op(&Type::Void, &OpType::IfElse);

                if_else_op.add_operand(lcmp_op);

                self.push_op(&if_else_op);

                // then_region
                let then_region = self.new_region();

                self.push_region(&then_region);

                let rcmp_op = self.visit_binaryexpr(eq_expr).unwrap();

                let mut store_op = self.new_op(&Type::Void, &OpType::Store);
                store_op
                    .set_attr(0, Attr::Size(4))
                    .set_attr(1, Attr::Align(4))
                    .add_operand(rcmp_op)
                    .add_operand(Value {
                        def: Rc::clone(&alloc_op),
                    });

                self.pop_region();
                // or_region
                let or_region = self.new_region();

                self.push_region(&or_region);

                let false_op = self.new_op(&Type::Bool, &OpType::GetFalse);

                let mut store_op = self.new_op(&Type::Void, &OpType::Store);
                store_op
                    .set_attr(0, Attr::Size(4))
                    .set_attr(1, Attr::Align(4))
                    .add_operand(Value { def: false_op })
                    .add_operand(Value {
                        def: Rc::clone(&alloc_op),
                    });

                self.pop_region();

                self.pop_op();

                let mut load_op = self.new_op(&Type::Bool, &OpType::Load);
                load_op
                    .set_attr(0, Attr::Size(4))
                    .set_attr(1, Attr::Align(4))
                    .add_operand(Value {
                        def: Rc::clone(&alloc_op),
                    });

                return Some(Value { def: load_op });
            }
        } else if let Some(unary_expr) = rhs_ast.downcast_ref::<Rc<ast::UnaryExpr>>() {
            rhs_op = self.visit_unaryexpr(unary_expr).unwrap();
        } else if let Some(mul_expr) = rhs_ast.downcast_ref::<Rc<ast::MulExpr>>() {
            rhs_op = self.visit_binaryexpr(mul_expr).unwrap();
        } else if let Some(add_expr) = rhs_ast.downcast_ref::<Rc<ast::AddExpr>>() {
            rhs_op = self.visit_binaryexpr(add_expr).unwrap();
        } else if let Some(rel_expr) = rhs_ast.downcast_ref::<Rc<ast::RelExpr>>() {
            rhs_op = self.visit_binaryexpr(rel_expr).unwrap();
        } else {
            panic!()
        }

        if lhs_ast.is_none() {
            return Some(rhs_op);
        }

        let mut lhs_op = self.visit_binaryexpr(&lhs_ast.clone().unwrap()).unwrap();
        let op = binary.as_ref().op.clone().unwrap();
        let mut ty = Type::Int32;

        // convert to more precise type
        if value_type!(lhs_op) != value_type!(rhs_op) {
            if value_type!(lhs_op) == Type::Int32 {
                lhs_op = self.new_type_convert(&Type::Float32, &Type::Int32, lhs_op);
            } else {
                rhs_op = self.new_type_convert(&Type::Float32, &Type::Int32, rhs_op);
            }

            ty = Type::Float32;
        }

        let mut new_op;

        if op.is_logical() {
            new_op = self.new_op(&Type::Bool, &self.convert_op(&ty, &op));
            new_op.add_operand(lhs_op).add_operand(rhs_op);

            match op {
                ast::Operator::Ls => new_op.set_attr(0, Attr::Cond(CondFlag::Lt)),
                ast::Operator::Gt => new_op.set_attr(0, Attr::Cond(CondFlag::Gt)),
                ast::Operator::Ge => new_op.set_attr(0, Attr::Cond(CondFlag::Ge)),
                ast::Operator::Eq => new_op.set_attr(0, Attr::Cond(CondFlag::Eq)),
                ast::Operator::Ne => new_op.set_attr(0, Attr::Cond(CondFlag::Ne)),
                ast::Operator::Le => new_op.set_attr(0, Attr::Cond(CondFlag::Le)),

                _ => panic!(),
            };
        } else {
            new_op = self.new_op(&ty, &self.convert_op(&ty, &op));
            new_op.add_operand(lhs_op).add_operand(rhs_op);
        }

        Some(Value { def: new_op })
    }

    fn visit_lval(&mut self, lval: &ast::LVal) -> (Value, bool) {
        let ident = &lval.ident;
        let subs = &lval.subscripts;
        let (lval_value, ty) = self.symbol_qeury(&ident);

        if subs.is_none() {
            if lval_value.get_optype() == OpType::GetArg {
                (lval_value, false)
            } else {
                (lval_value, true)
            }
        } else {
            // emit gep

            let array_ty = lval_value.get_type();

            match array_ty {
                Type::Array(_) => {
                    let mut array_type = array_ty;
                    // let mut last_op: Value = lval_value;
                    let mut last_op = Value {
                        def: Rc::clone(
                            self.new_op(&Type::Int64, &OpType::Ptr2Int)
                                .add_operand(lval_value),
                        ),
                    };
                    let mut cnt = 0;

                    while array_type.is_array() {
                        match array_type {
                            Type::Array(ref inner) => {
                                let inner_type = &inner.as_ref().0;
                                let size = inner.as_ref().1;

                                if size.is_none() {
                                    break;
                                }

                                let size = size.unwrap();
                                let sub =
                                    self.visit_binaryexpr(&subs.clone().unwrap()[cnt]).unwrap();

                                let get_size = self.new_int64(size as i64);

                                let mut lmul = self.new_op(&Type::Int64, &OpType::LMul);
                                lmul.add_operand(sub).add_operand(get_size);

                                let mut ladd = self.new_op(&inner_type, &OpType::LAdd);
                                ladd.add_operand(Value { def: lmul }).add_operand(last_op);

                                // let mut int2ptr =
                                //     self.new_op(&Type::Ptr(Box::new(ty.clone())), &OpType::Int2Ptr);
                                // int2ptr.add_operand(Value { def: ladd });

                                last_op = Value { def: ladd };

                                array_type = inner_type.clone();

                                cnt += 1;
                            }
                            _ => panic!(),
                        }
                    }

                    last_op = Value {
                        def: Rc::clone(
                            self.new_op(&Type::Ptr(Box::new(ty.clone())), &OpType::Int2Ptr)
                                .add_operand(last_op),
                        ),
                    };

                    (last_op, true)
                }
                _ => panic!("visit_lval: expect array type but find : {:?}", array_ty),
            }
        }
    }
}
