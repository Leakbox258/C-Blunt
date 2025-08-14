use crate::scf::attr::*;
use crate::scf::r#macro::*;
use crate::scf::no_wrap::*;
use crate::scf::operation::OpType;
use crate::scf::value::Type;
use crate::scf::value::Value;
use crate::{scf::operation::Operation, visitor::visitor::Shared};
use std::rc::Rc;
use std::rc::Weak;

pub struct Interpreter {
    module: Shared<Operation>,
    io: Box<dyn std::io::Write>,
}

impl Interpreter {
    pub fn interpret(module: Shared<Operation>, io: Box<dyn std::io::Write>) {
        let mut interpreter = Interpreter {
            module: module,
            io: io,
        };

        interpreter.write();
    }

    pub fn write(&mut self) {
        for decl in get_decl!(self.module) {
            let _ = self.io.write(self.global_decl(decl).as_bytes());
        }

        for func in get_fns!(self.module) {
            if !fn_decl_only!(func) {
                let _ = self.io.write(self.fn_def(func).as_bytes());
                let _ = self.io.write("\n".as_bytes());
            }
        }

        for func in get_fns!(self.module) {
            if fn_decl_only!(func) {
                let _ = self.io.write(self.fn_decl(func).as_bytes());
            }
        }
    }

    pub fn fn_decl(&self, r#fn: Shared<Operation>) -> String {
        let mut str = format!(
            "declare {} @{}(",
            r#fn.get_type().to_string(),
            fn_name!(r#fn).to_string()
        );

        for (_, arg_type) in fn_format_args!(r#fn) {
            str += &(arg_type.to_string() + " noundef, ");
        }

        if str.ends_with(", ") {
            let _ = str.split_off(str.len() - 2);
        }

        str += ")\n";

        str
    }

    pub fn global_decl(&self, var: Shared<Operation>) -> String {
        let decl_init_attr = var.get_attr_as_ref(2);

        let shape = match var.get_attr(4) {
            Attr::ArrayShape(s) => s,
            Attr::Empty => vec![1 as usize], // wont use actually
            _ => unreachable!(),
        };

        let format_init = match *decl_init_attr {
            Attr::Int32(r#i32) => format!("{}", r#i32),
            Attr::Float(r#f32) => format!("0X{:016X}", (r#f32 as f64).to_bits()),
            Attr::IntArray(ref iarray) => format_array_i32(&shape, &iarray),
            Attr::FloatArray(ref farray) => format_array_f32(&shape, &farray),
            _ => unreachable!(),
        };

        let str = format!(
            "@{} = dso_local global {} {}, align {}", // TODO: global or constant
            get_name!(var).unwrap(),
            if var.get_type().is_ptr() {
                var.get_type().deref().to_string()
            } else {
                var.get_type().to_string()
            },
            format_init,
            get_align!(var).unwrap(),
        );
        format!("{}\n", str)
    }

    pub fn fn_def(&self, r#fn: Shared<Operation>) -> String {
        let mut args = String::new();
        let mut body = String::new();

        for block in r#fn.get_default_region().get_blocks().iter() {
            body += format!("{}:\n", block.get_id()).as_str();
            for op in block.borrow().get_ops() {
                // handle args
                if optype_checkif!(op, OpType::GetArg) {
                    let arg_ty = op.get_type().downgrade();
                    let arg_id = op.get_id();
                    args += format!("{} noundef %{}, ", arg_ty.to_string(), arg_id).as_str();
                    continue;
                }

                match self.operation(op) {
                    Some(op_str) => body += format!("    {}", op_str).as_str(),
                    None => continue,
                }
            }
        }

        if args.len() > 0 {
            args = args.split_off(args.len() - 2);
        }

        let decl = format!(
            "define dso_local {} @{}({})",
            r#fn.get_type().to_string(),
            get_name!(r#fn).unwrap(),
            args
        );

        format!("{} {{\n{}}}\n", decl, body)
    }

    pub fn operation(&self, op: &Shared<Operation>) -> Option<String> {
        match op.get_optype() {
            OpType::Module
            | OpType::Function
            | OpType::If
            | OpType::IfElse
            | OpType::While
            | OpType::Proceed
            | OpType::Break
            | OpType::Continue
            | OpType::DeclGlobal
            | OpType::GetArg => {
                unreachable!(
                    "lowering operation: unexpected OpType: {:?}",
                    op.get_optype()
                )
            }
            OpType::ICmp | OpType::LCmp | OpType::FCmp => {
                let (inst, ty) = match op.get_optype() {
                    OpType::ICmp => ("icmp", "i32"),
                    OpType::LCmp => ("icmp", "i64"),
                    OpType::FCmp => ("fcmp", "float"),
                    _ => unreachable!(),
                };

                Some(format!(
                    "%{} = {} {} {} {}, {}\n",
                    op.get_id(),
                    inst,
                    cond_to_str(&op),
                    ty,
                    op_to_literal(&op.get_operand(0)),
                    op_to_literal(&op.get_operand(1))
                ))
            }
            OpType::Branch => match get_false!(op) {
                Some(false_label) => Some(format!(
                    "br i1 %{}, lable %{}, lable %{}\n",
                    op.get_operand(0).def.get_id(),
                    get_true!(op).unwrap().get_id(),
                    false_label.get_id()
                )),
                None => Some(format!("br label %{}\n", get_nocond!(op).unwrap().get_id())),
            },
            OpType::Return => match op.get_type() {
                Type::Void => Some(format!("ret {}\n", op.get_type().to_string())),
                _ => Some(format!(
                    "ret {} {}\n",
                    op.get_type().to_string(),
                    op_to_literal(&op.get_operand(0))
                )),
            },
            OpType::Alloca => {
                Some(format!(
                    "%{} = alloca {}, align {}\n",
                    op.get_id(),
                    op.get_type().to_string(), // op_ptr_defref!(op).to_string(),
                    get_align!(op).unwrap()
                ))
            }
            OpType::Store => Some(format!(
                "store {} {}, {} {}, align {}\n",
                op.get_operand(0).get_type().to_string(),
                op_to_literal(&op.get_operand(0)),
                op.get_operand(1).get_type().to_string(),
                op_to_literal(&op.get_operand(1)),
                get_align!(op).unwrap()
            )),
            OpType::Load => Some(format!(
                "%{} = load {}, {} {}, align {}\n",
                op.get_id(),
                op.get_type().to_string(),
                op.get_operand(0).get_type().to_string(),
                op_to_literal(&op.get_operand(0)),
                get_align!(op).unwrap()
            )),
            OpType::Neg => {
                Some(format!(
                    "%{} = sub {}, 0, {}\n",
                    op.get_id(),
                    op.get_operand(0).get_type().to_string(),
                    op_to_literal(&op.get_operand(0))
                )) // expected: i32 / i64
            }
            OpType::FNeg => Some(format!(
                "%{} = fneg float {}\n",
                op.get_id(),
                op_to_literal(&op.get_operand(0))
            )),
            OpType::Add
            | OpType::LAdd
            | OpType::FAdd
            | OpType::Sub
            | OpType::LSub
            | OpType::FSub
            | OpType::Mul
            | OpType::LMul
            | OpType::FMul
            | OpType::Div
            | OpType::LDiv
            | OpType::FDiv
            | OpType::Mod
            | OpType::LMod
            | OpType::FMod
            | OpType::Xor
            | OpType::LXor => {
                let (inst, ty) = match op.get_optype() {
                    OpType::Add => ("add", "i32"),
                    OpType::LAdd => ("add", "i64"),
                    OpType::FAdd => ("fadd", "float"),
                    OpType::Sub => ("sub", "i32"),
                    OpType::LSub => ("sub", "i64"),
                    OpType::FSub => ("fsub", "float"),
                    OpType::Mul => ("mul", "i32"),
                    OpType::LMul => ("mul", "i64"),
                    OpType::FMul => ("fmul", "float"),
                    OpType::Div => ("sdiv", "i32"),
                    OpType::LDiv => ("sdiv", "i64"),
                    OpType::FDiv => ("fdiv", "float"),
                    OpType::Mod => ("srem", "i32"),
                    OpType::LMod => ("srem", "i64"),
                    OpType::FMod => ("frem", "float"),
                    OpType::Xor => ("xor", "i32"),
                    OpType::LXor => ("lxor", "i64"),
                    _ => unreachable!(
                        "lowering operation: unexpected OpType: {:?}",
                        op.get_optype()
                    ),
                };

                Some(format!(
                    "%{} = {} {} {}, {}\n",
                    op.get_id(),
                    inst,
                    ty,
                    op_to_literal(&op.get_operand(0)),
                    op_to_literal(&op.get_operand(1))
                ))
            }
            OpType::F2I => Some(format!(
                "%{} = fptosi float {} to i32\n",
                op.get_id(),
                op_to_literal(&op.get_operand(0))
            )),
            OpType::I2F => Some(format!(
                "%{} = sitofp i32 {} to float\n",
                op.get_id(),
                op_to_literal(&op.get_operand(0))
            )),
            OpType::ZEXT => Some(format!(
                "%{} = zext i32 {} to i64\n",
                op.get_id(),
                op_to_literal(&op.get_operand(0))
            )),
            OpType::SEXT => Some(format!(
                "%{} = sext i32 {} to i64\n",
                op.get_id(),
                op_to_literal(&op.get_operand(0))
            )),
            OpType::Ptr2Int => Some(format!(
                "%{} = ptrtoint {} {} to i64\n",
                op.get_id(),
                op.get_operand(0).get_type().downgrade().to_string(),
                op_to_literal(&op.get_operand(0))
            )),
            OpType::Int2Ptr => Some(format!(
                "%{} = inttoptr i64 {} to {}\n",
                op.get_id(),
                op_to_literal(&op.get_operand(0)),
                op.get_type().downgrade().to_string()
            )),
            OpType::FuncCall => {
                if get_name!(op).unwrap() == "memset" {
                    return Some(format!(
                        "call void @llvm.memset.p0i8.i32(ptr align 4 {}, i8 {}, i32 {}, i1 false)\n",
                        op_to_literal(&op.get_operand(0)),
                        op_to_literal(&op.get_operand(1)),
                        op_to_literal(&op.get_operand(2))
                    ));
                }

                let mut str = match op.get_type() {
                    Type::Void => format!(
                        "call {} @{}(",
                        op.get_type().to_string(),
                        get_name!(op).unwrap()
                    ),
                    _ => format!(
                        "%{} = call {} @{}(",
                        op.get_id(),
                        op.get_type().to_string(),
                        get_name!(op).unwrap()
                    ),
                };

                for (i, arg) in op.borrow().get_operands().iter().enumerate() {
                    if i > 0 {
                        str += ", ";
                    }
                    str += &(arg.get_type().to_string() + " " + &op_to_literal(arg));
                }

                str += ")\n";
                Some(str)
            }
            OpType::SetCond => todo!(),
            // OpType::SetCond => {
            //     format!(
            //         "  %{} = select i1 %{}, i1 1, i1 0",
            //         op.get_id(),
            //         op_to_literal(&op.get_operand(0))
            //     );
            // }
            OpType::Phi => todo!(),
            OpType::GetI
            | OpType::GetL
            | OpType::GetF
            | OpType::GetArrayI
            | OpType::GetArrayL
            | OpType::GetArrayF
            | OpType::GetTrue
            | OpType::GetFalse => None,
        }
    }
}

fn format_array_i32(shape: &[usize], values: &[i32]) -> String {
    fn build_type(shape: &[usize]) -> String {
        if shape.is_empty() {
            "i32".to_string()
        } else {
            format!("[{} x {}]", shape[0], build_type(&shape[1..]))
        }
    }

    let total_size = shape.iter().product::<usize>();
    let values = if values.len() < total_size {
        let mut v = values.to_vec();
        v.resize(total_size, 0);
        v
    } else {
        values.to_vec()
    };

    if values.iter().all(|&v| v == 0) && !shape.is_empty() {
        return format!("zeroinitializer");
    }

    if shape.is_empty() {
        return format!("i32 {}", values[0]);
    }

    let dim = shape[0];
    let sub_shape = &shape[1..];
    let sub_size = sub_shape.iter().product::<usize>();
    let sub_type = build_type(sub_shape);

    let mut parts = Vec::new();
    for i in 0..dim {
        let sub_values = &values[i * sub_size..(i + 1) * sub_size];
        let part = if sub_values.iter().all(|&v| v == 0) && !sub_shape.is_empty() {
            format!("{} zeroinitializer", sub_type)
        } else {
            let inner = format_array_i32(sub_shape, sub_values);
            format!("{} [{}]", sub_type, inner)
        };
        parts.push(part);
    }
    parts.join(", ")
}

fn format_array_f32(shape: &[usize], values: &[f32]) -> String {
    fn build_type(shape: &[usize]) -> String {
        if shape.is_empty() {
            "float".to_string()
        } else {
            format!("[{} x {}]", shape[0], build_type(&shape[1..]))
        }
    }

    let total_size = shape.iter().product::<usize>();
    let values = if values.len() < total_size {
        let mut v = values.to_vec();
        v.resize(total_size, 0.0);
        v
    } else {
        values.to_vec()
    };

    if values.iter().all(|&v| v == 0.0) && !shape.is_empty() {
        return format!("zeroinitializer");
    }

    if shape.is_empty() {
        return format!("float 0X{:016X}", (values[0] as f64).to_bits());
    }

    let dim = shape[0];
    let sub_shape = &shape[1..];
    let sub_size = sub_shape.iter().product::<usize>();
    let sub_type = build_type(sub_shape);

    let mut parts = Vec::new();
    for i in 0..dim {
        let sub_values = &values[i * sub_size..(i + 1) * sub_size];
        let part = if sub_values.iter().all(|&v| v == 0.0) && !sub_shape.is_empty() {
            format!("{} zeroinitializer", sub_type)
        } else {
            let inner = format_array_f32(sub_shape, sub_values);
            format!("{} [{}]", sub_type, inner)
        };
        parts.push(part);
    }
    parts.join(", ")
}

// %0 = getint <114514>
// %2 = add %1, %0
// ---> %2 = add %1, 114514
fn op_to_literal(operand: &Value) -> String {
    match operand.get_optype() {
        OpType::GetI => get_int!(operand.def).unwrap().to_string(),
        OpType::GetL => get_long!(operand.def).unwrap().to_string(),
        OpType::GetF => format!(
            "0X{:016X}",
            (get_float!(operand.def).unwrap() as f64).to_bits()
        ),
        OpType::GetTrue => "1".to_string(),
        OpType::GetFalse => "0".to_string(),
        OpType::DeclGlobal => format!("@{}", get_name!(operand.def).unwrap().to_string()), // only bty ptr
        _ => format!("%{}", operand.def.get_id()),
    }
}

fn cond_to_str(op: &Shared<Operation>) -> String {
    match op.get_optype() {
        OpType::ICmp | OpType::LCmp => match get_cond!(op).unwrap() {
            CondFlag::Eq => "eq".to_string(),
            CondFlag::Ne => "ne".to_string(),
            CondFlag::Le => "sle".to_string(),
            CondFlag::Gt => "sgt".to_string(),
            CondFlag::Ge => "sge".to_string(),
            CondFlag::Lt => "slt".to_string(),
        },
        OpType::FCmp => match get_cond!(op).unwrap() {
            CondFlag::Eq => "oeq".to_string(),
            CondFlag::Ne => "one".to_string(),
            CondFlag::Le => "ole".to_string(),
            CondFlag::Gt => "ogt".to_string(),
            CondFlag::Ge => "oge".to_string(),
            CondFlag::Lt => "olt".to_string(),
        },
        _ => panic!(),
    }
}
