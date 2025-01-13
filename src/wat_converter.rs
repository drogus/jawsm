use std::{cell::RefCell, rc::Rc};
use tarnik_ast::{
    InstructionsList, InstructionsListWrapped, Nullable, Signature, WasmType, WatFunction,
    WatInstruction as W,
};
use wast::{
    core::{Instruction, RefType, ValType},
    token::Index,
};

#[derive(Debug, Clone)]
enum BlockType {
    Function {
        instructions: Rc<RefCell<InstructionsList>>,
    },
    Block {
        instruction: W,
        instructions: Rc<RefCell<InstructionsList>>,
    },
    If {
        instruction: W,
        then_instructions: Rc<RefCell<InstructionsList>>,
        else_instructions: Rc<RefCell<InstructionsList>>,
        in_else: bool,
    },
    Loop {
        instruction: W,
        instructions: Rc<RefCell<InstructionsList>>,
    },
    Try {
        instruction: W,
        try_instructions: Rc<RefCell<InstructionsList>>,
        catch_instructions: Rc<RefCell<InstructionsList>>,
        catch_label: Option<String>,
        in_catch: bool,
    },
}

pub struct WatConverter {
    block_stack: Vec<BlockType>,
}

pub fn parse_wat_function(wat_func: &str) -> WatFunction {
    use wast::parser::{self, ParseBuffer};
    use wast::Wat;

    // Wrap the function in a module
    let wat = format!("(module {})", wat_func);
    let buf = ParseBuffer::new(&wat).unwrap();
    let module = parser::parse::<Wat>(&buf).unwrap();

    if let wast::Wat::Module(module) = module {
        if let wast::core::ModuleKind::Text(items) = module.kind {
            for item in items {
                if let wast::core::ModuleField::Func(func) = item {
                    dbg!(&func);
                    let name = func.id.unwrap().name();
                    let mut tarnik_func = WatFunction::new(name);

                    if let Some(inline) = func.ty.inline {
                        tarnik_func.params = inline
                            .params
                            .iter()
                            .map(|(id, _, valtype)| {
                                (
                                    id.map(|id| with_dollar(id.name())),
                                    convert_wasm_type(valtype),
                                )
                            })
                            .collect();

                        tarnik_func.results =
                            inline.results.iter().map(convert_wasm_type).collect();

                        if let wast::core::FuncKind::Inline { locals, expression } = func.kind {
                            for local in locals {
                                tarnik_func.add_local_exact(
                                    with_dollar(local.id.unwrap().name()),
                                    convert_wasm_type(&local.ty),
                                );
                            }

                            let instructions = Rc::new(RefCell::new(Vec::new()));
                            let mut converter = WatConverter::new(instructions.clone());

                            for instruction in expression.instrs {
                                if let Some(wat_instruction) =
                                    converter.convert_instruction(instruction)
                                {
                                    converter.add_instruction(wat_instruction);
                                }
                            }

                            tarnik_func.set_body(instructions.borrow().to_vec());
                        }
                    }
                    return tarnik_func;
                }
            }
        }
    }
    panic!("No function found in WAT code");
}

fn convert_ref_type(ref_type: &RefType) -> WasmType {
    let nullable = if ref_type.nullable {
        Nullable::True
    } else {
        Nullable::False
    };
    match &ref_type.heap {
        wast::core::HeapType::Abstract { shared: _, ty } => match ty {
            wast::core::AbstractHeapType::Any => WasmType::Anyref,
            wast::core::AbstractHeapType::I31 => WasmType::I31Ref,
            // wast::core::AbstractHeapType::Func => WasmType::Funcref,
            // wast::core::AbstractHeapType::Extern => WasmType::Externref,
            _ => todo!(),
        },
        wast::core::HeapType::Concrete(idx) => {
            WasmType::Ref(with_dollar(&get_id_from_index(idx).unwrap()), nullable)
        }
    }
}

fn convert_wasm_type(ty: &wast::core::ValType) -> WasmType {
    match ty {
        wast::core::ValType::I32 => WasmType::I32,
        wast::core::ValType::I64 => WasmType::I64,
        wast::core::ValType::F32 => WasmType::F32,
        wast::core::ValType::F64 => WasmType::F64,
        wast::core::ValType::V128 => unimplemented!("V128 type not supported"),
        wast::core::ValType::Ref(ref_type) => convert_ref_type(ref_type),
    }
}

fn get_id_from_index(index: &Index<'_>) -> Option<String> {
    return match index {
        wast::token::Index::Num(_, span) => todo!(),
        wast::token::Index::Id(id) => Some(id.name().to_string()),
    };
}

fn with_dollar(name: &str) -> String {
    if name.starts_with('$') {
        name.to_string()
    } else {
        format!("${}", name)
    }
}

impl WatConverter {
    pub fn new(function_instructions: Rc<RefCell<InstructionsList>>) -> Self {
        Self {
            block_stack: vec![BlockType::Function {
                instructions: function_instructions,
            }],
        }
    }

    pub fn convert_instruction(&mut self, instruction: Instruction) -> Option<W> {
        match instruction {
            // Block type instructions that need special handling
            Instruction::Block(block_type) => {
                let label = block_type.label.unwrap().name();
                let instructions = Rc::new(RefCell::new(Vec::new()));
                let block = W::Block {
                    label: with_dollar(label),
                    signature: Signature::default(),
                    instructions: instructions.clone(),
                };

                self.block_stack.push(BlockType::Block {
                    instruction: block,
                    instructions,
                });
                None
            }
            Instruction::If(block_type) => {
                let then_instructions = Rc::new(RefCell::new(Vec::new()));
                let if_instr = W::If {
                    then: then_instructions.clone(),
                    r#else: None,
                };

                self.block_stack.push(BlockType::If {
                    instruction: if_instr,
                    then_instructions,
                    else_instructions: Default::default(),
                    in_else: false,
                });
                None
            }
            Instruction::Loop(block_type) => {
                let label = with_dollar(block_type.label.unwrap().name());
                let instructions = Rc::new(RefCell::new(Vec::new()));
                let loop_instr = W::Loop {
                    label,
                    instructions: instructions.clone(),
                };

                self.block_stack.push(BlockType::Loop {
                    instruction: loop_instr,
                    instructions,
                });
                None
            }
            Instruction::Else(_) => {
                if let Some(BlockType::If {
                    ref mut else_instructions,
                    ref mut in_else,
                    ..
                }) = self.block_stack.last_mut()
                {
                    *in_else = true;
                }
                None
            }
            Instruction::Try(_block_type) => {
                let try_instructions = Rc::new(RefCell::new(Vec::new()));
                let catch_instructions = Rc::new(RefCell::new(Vec::new()));
                let try_instr = W::Try {
                    try_block: try_instructions.clone(),
                    catches: vec![],
                    catch_all: None,
                };

                self.block_stack.push(BlockType::Try {
                    instruction: try_instr,
                    try_instructions,
                    catch_instructions,
                    catch_label: None,
                    in_catch: false,
                });
                None
            }
            Instruction::Catch(idx) => {
                let exception = with_dollar(&get_id_from_index(&idx).unwrap());
                if let Some(BlockType::Try {
                    ref mut in_catch,
                    ref mut catch_label,
                    ..
                }) = self.block_stack.last_mut()
                {
                    *in_catch = true;
                    *catch_label = Some(exception);
                }
                None
            }
            Instruction::End(_) => {
                if let Some(block_type) = self.block_stack.pop() {
                    match block_type {
                        BlockType::Block { instruction, .. }
                        | BlockType::Loop { instruction, .. }
                        | BlockType::If { instruction, .. } => {
                            self.add_instruction(instruction);
                        }
                        BlockType::Try {
                            try_instructions,
                            catch_instructions,
                            catch_label,
                            ..
                        } => {
                            // TODO: this is just weird, I'm adding W::Try as an `instruction` on
                            // BlockType, but here I have to construct it anyway. This should be
                            // refactored later
                            let instruction = W::Try {
                                try_block: try_instructions,
                                catches: vec![(
                                    catch_label.unwrap_or_default(),
                                    catch_instructions,
                                )],
                                catch_all: None,
                            };
                            self.add_instruction(instruction);
                        }
                        BlockType::Function { .. } => {}
                    }
                }
                None
            }

            // Direct conversions
            Instruction::Nop => Some(W::Nop),
            Instruction::Unreachable => None,
            Instruction::Return => Some(W::Return),
            Instruction::Drop => Some(W::Drop),

            // Numeric operations
            Instruction::I32Const(n) => Some(W::I32Const(n)),
            Instruction::I64Const(n) => Some(W::I64Const(n)),
            // TODO: this is wrong, but I don't care for now
            Instruction::F32Const(n) => Some(W::F32Const(n.bits as f32)),
            Instruction::F64Const(n) => Some(W::F64Const(n.bits as f64)),

            // Local operations
            Instruction::LocalGet(idx) => {
                Some(W::LocalGet(with_dollar(&get_id_from_index(&idx).unwrap())))
            }
            Instruction::LocalSet(idx) => {
                Some(W::LocalSet(with_dollar(&get_id_from_index(&idx).unwrap())))
            }
            Instruction::LocalTee(idx) => {
                Some(W::LocalTee(with_dollar(&get_id_from_index(&idx).unwrap())))
            }

            // Branch instructions
            Instruction::Br(idx) => Some(W::Br(with_dollar(&get_id_from_index(&idx).unwrap()))),
            Instruction::BrIf(idx) => Some(W::BrIf(with_dollar(&get_id_from_index(&idx).unwrap()))),

            // Function calls
            Instruction::Call(idx) => Some(W::Call(with_dollar(&get_id_from_index(&idx).unwrap()))),
            Instruction::CallIndirect(_) => todo!(),

            // Reference instructions
            Instruction::RefNull(heap_type) => Some(W::RefNull(convert_heap_type(&heap_type))),
            Instruction::RefFunc(idx) => {
                Some(W::RefFunc(with_dollar(&get_id_from_index(&idx).unwrap())))
            }

            // Struct/Array operations
            Instruction::StructNew(idx) => Some(W::StructNew(format!(
                "${}",
                get_id_from_index(&idx).unwrap()
            ))),
            Instruction::StructGet(access) => Some(W::StructGet(
                with_dollar(&get_id_from_index(&access.r#struct).unwrap()),
                with_dollar(&get_id_from_index(&access.field).unwrap()),
            )),
            Instruction::ArrayNew(idx) => {
                Some(W::ArrayNew(with_dollar(&get_id_from_index(&idx).unwrap())))
            }
            Instruction::BrTable(_) => Some(W::Empty),
            Instruction::ReturnCall(idx) => Some(W::ReturnCall(with_dollar(
                &get_id_from_index(&idx).unwrap(),
            ))),
            Instruction::ReturnCallIndirect(_) => Some(W::Empty),
            Instruction::CallRef(idx) => {
                Some(W::CallRef(with_dollar(&get_id_from_index(&idx).unwrap())))
            }
            Instruction::ReturnCallRef(_) => Some(W::Empty),
            Instruction::Select(_) => Some(W::Empty),
            Instruction::GlobalGet(idx) => {
                Some(W::GlobalGet(with_dollar(&get_id_from_index(&idx).unwrap())))
            }
            Instruction::GlobalSet(idx) => {
                Some(W::GlobalSet(with_dollar(&get_id_from_index(&idx).unwrap())))
            }
            Instruction::TableGet(_) => Some(W::Empty),
            Instruction::TableSet(_) => Some(W::Empty),
            Instruction::I32Load(_) => Some(W::I32Load(None)),
            Instruction::I64Load(_) => Some(W::I64Load(None)),
            Instruction::F32Load(_) => Some(W::F32Load(None)),
            Instruction::F64Load(_) => Some(W::F64Load(None)),
            Instruction::I32Load8s(_) => Some(W::I32Load8S(None)),
            Instruction::I32Load8u(_) => Some(W::I32Load8U(None)),
            Instruction::I32Load16s(_) => Some(W::I32Load16S(None)),
            Instruction::I32Load16u(_) => Some(W::I32Load16U(None)),
            Instruction::I64Load8s(_) => Some(W::I64Load8S(None)),
            Instruction::I64Load8u(_) => Some(W::I64Load8U(None)),
            Instruction::I64Load16s(_) => Some(W::I64Load16S(None)),
            Instruction::I64Load16u(_) => Some(W::I64Load16U(None)),
            Instruction::I64Load32s(_) => Some(W::I64Load32S(None)),
            Instruction::I64Load32u(_) => Some(W::I64Load32U(None)),
            Instruction::I32Store(_) => Some(W::I32Store(None)),
            Instruction::I64Store(_) => Some(W::I64Store(None)),
            Instruction::F32Store(_) => Some(W::F32Store(None)),
            Instruction::F64Store(_) => Some(W::F64Store(None)),
            Instruction::I32Store8(_) => Some(W::I32Store8(None)),
            Instruction::I32Store16(_) => Some(W::I32Store16(None)),
            Instruction::I64Store8(_) => Some(W::I64Store8(None)),
            Instruction::I64Store16(_) => Some(W::I64Store16(None)),
            Instruction::I64Store32(_) => Some(W::I64Store32(None)),
            Instruction::I32Add => Some(W::I32Add),
            Instruction::I32LtS => Some(W::I32LtS),
            Instruction::RefEq => Some(W::RefEq),
            Instruction::RefTest(ref_test) => Some(W::RefTest(convert_ref_type(&ref_test.r#type))),
            Instruction::RefCast(ref_cast) => Some(W::RefCast(convert_ref_type(&ref_cast.r#type))),
            instr => todo!("could not convert {instr:?}"),
        }
    }
    // #[derive(Debug, Clone, PartialEq)]
    // pub enum WatInstruction {
    //     Nop,
    //     Local {
    //         name: String,
    //         ty: WasmType,
    //     },
    //     GlobalGet(String),
    //     GlobalSet(String),
    //     LocalGet(String),
    //     LocalSet(String),
    //     Call(String),
    //     CallRef(String),
    //
    //     I32Const(i32),
    //     I64Const(i64),
    //     F32Const(f32),
    //     F64Const(f64),
    //
    //     F32Neg,
    //     F64Neg,
    //
    //     I32Eqz,
    //     I64Eqz,
    //     F32Eqz,
    //     F64Eqz,
    //
    //     I32Eq,
    //     I64Eq,
    //     F32Eq,
    //     F64Eq,
    //
    //     I32Ne,
    //     I64Ne,
    //     F32Ne,
    //     F64Ne,
    //
    //     I32Add,
    //     I64Add,
    //     F32Add,
    //     F64Add,
    //
    //     I32Sub,
    //     I64Sub,
    //     F32Sub,
    //     F64Sub,
    //
    //     I32Mul,
    //     I64Mul,
    //     F32Mul,
    //     F64Mul,
    //
    //     I32DivS,
    //     I64DivS,
    //     I32DivU,
    //     I64DivU,
    //     F32Div,
    //     F64Div,
    //
    //     I32RemS,
    //     I64RemS,
    //     I32RemU,
    //     I64RemU,
    //
    //     I32And,
    //     I64And,
    //
    //     I32Or,
    //     I64Or,
    //
    //     I32Xor,
    //     I64Xor,
    //
    //     I32LtS,
    //     I64LtS,
    //     I32LtU,
    //     I64LtU,
    //     F32Lt,
    //     F64Lt,
    //
    //     I32LeS,
    //     I64LeS,
    //     I32LeU,
    //     I64LeU,
    //     F32Le,
    //     F64Le,
    //
    //     I32GeS,
    //     I64GeS,
    //     I32GeU,
    //     I64GeU,
    //     F32Ge,
    //     F64Ge,
    //
    //     I32GtS,
    //     I64GtS,
    //     I32GtU,
    //     I64GtU,
    //     F32Gt,
    //     F64Gt,
    //
    //     I32Shl,
    //     I64Shl,
    //
    //     I32ShrS,
    //     I64ShrS,
    //     I32ShrU,
    //     I64ShrU,
    //
    //     I64ExtendI32S,
    //     I32WrapI64,
    //     F64PromoteF32,
    //     F32DemoteF64,
    //
    //     F32ConvertI32S,
    //     F32ConvertI32U,
    //     F32ConvertI64S,
    //     F32ConvertI64U,
    //
    //     F64ConvertI32S,
    //     F64ConvertI32U,
    //     F64ConvertI64S,
    //     F64ConvertI64U,
    //
    //     I32TruncF32S,
    //     I32TruncF32U,
    //     I32TruncF64S,
    //     I32TruncF64U,
    //
    //     I64TruncF32S,
    //     I64TruncF32U,
    //     I64TruncF64S,
    //     I64TruncF64U,
    //
    //     I32ReinterpretF32,
    //     F32ReinterpretI32,
    //     I64ReinterpretF64,
    //     F64ReinterpretI64,
    //
    //     I31GetS,
    //     I31GetU,
    //
    //     I32Store(Option<String>),
    //     I64Store(Option<String>),
    //     F32Store(Option<String>),
    //     F64Store(Option<String>),
    //     I32Store8(Option<String>),
    //     I32Store16(Option<String>),
    //     I64Store8(Option<String>),
    //     I64Store16(Option<String>),
    //     I64Store32(Option<String>),
    //
    //     I32Load(Option<String>),
    //     I64Load(Option<String>),
    //     F32Load(Option<String>),
    //     F64Load(Option<String>),
    //     I32Load8S(Option<String>),
    //     I32Load8U(Option<String>),
    //     I32Load16S(Option<String>),
    //     I32Load16U(Option<String>),
    //     I64Load8S(Option<String>),
    //     I64Load8U(Option<String>),
    //     I64Load16S(Option<String>),
    //     I64Load16U(Option<String>),
    //     I64Load32S(Option<String>),
    //     I64Load32U(Option<String>),
    //
    //     StructNew(String),
    //     StructGet(String, String),
    //     StructSet(String, String),
    //     ArrayNew(String),
    //     ArrayNewFixed(String, u16),
    //     ArrayLen,
    //     ArrayGet(String),
    //     ArrayGetU(String),
    //     ArraySet(String),
    //     RefNull(WasmType),
    //     RefCast(WasmType),
    //     RefTest(WasmType),
    //     Ref(String),
    //     RefFunc(String),
    //     Type(String),
    //     Return,
    //     ReturnCall(String),
    //     Block {
    //         label: String,
    //         signature: Signature,
    //         instructions: InstructionsListWrapped,
    //     },
    //     Loop {
    //         label: String,
    //         instructions: InstructionsListWrapped,
    //     },
    //     If {
    //         then: InstructionsListWrapped,
    //         r#else: Option<InstructionsListWrapped>,
    //     },
    //     BrIf(String),
    //     Br(String),
    //     Empty,
    //     Log,
    //     Identifier(String),
    //     Drop,
    //     LocalTee(String),
    //     RefI31,
    //     Throw(String),
    //     Try {
    //         try_block: InstructionsListWrapped,
    //         catches: Vec<(String, InstructionsListWrapped)>,
    //         catch_all: Option<InstructionsListWrapped>,
    //     },
    //     RefEq,
    // }

    // TODO: I really don't like this, refactor
    pub fn add_instruction(&mut self, instruction: W) {
        match self.block_stack.pop() {
            Some(BlockType::Function { instructions }) => {
                instructions.borrow_mut().push(instruction);
                self.block_stack.push(BlockType::Function { instructions });
            }
            Some(BlockType::Block {
                instructions,
                instruction: wat_instruction,
            }) => {
                instructions.borrow_mut().push(instruction);
                self.block_stack.push(BlockType::Block {
                    instruction: wat_instruction,
                    instructions,
                });
            }
            Some(BlockType::If {
                then_instructions,
                else_instructions,
                in_else,
                instruction: wat_instruction,
            }) => {
                let wat_instruction = if let W::If { r#then, r#else } = wat_instruction {
                    let r#else = if in_else {
                        Some(else_instructions.clone())
                    } else {
                        r#else
                    };
                    W::If { r#then, r#else }
                } else {
                    wat_instruction
                };

                if in_else {
                    else_instructions.borrow_mut().push(instruction);
                } else {
                    then_instructions.borrow_mut().push(instruction);
                }

                self.block_stack.push(BlockType::If {
                    then_instructions,
                    else_instructions,
                    in_else,
                    instruction: wat_instruction,
                });
            }
            Some(BlockType::Try {
                try_instructions,
                catch_instructions,
                instruction: wat_instruction,
                catch_label,
                in_catch,
            }) => {
                if in_catch {
                    catch_instructions.borrow_mut().push(instruction);
                } else {
                    try_instructions.borrow_mut().push(instruction);
                }

                self.block_stack.push(BlockType::Try {
                    try_instructions,
                    catch_instructions,
                    instruction: wat_instruction,
                    catch_label,
                    in_catch,
                });
            }
            Some(BlockType::Loop {
                instructions,
                instruction: wat_instruction,
            }) => {
                instructions.borrow_mut().push(instruction);
                self.block_stack.push(BlockType::Loop {
                    instruction: wat_instruction,
                    instructions,
                });
            }
            None => panic!("No active block to add instruction to"),
        }
    }
}

fn convert_heap_type(heap_type: &wast::core::HeapType) -> WasmType {
    match heap_type {
        wast::core::HeapType::Abstract { shared: _, ty } => match ty {
            // wast::core::AbstractHeapType::Func => WasmType::Funcref,
            // wast::core::AbstractHeapType::Extern => WasmType::Externref,
            wast::core::AbstractHeapType::Any => WasmType::Anyref,
            wast::core::AbstractHeapType::I31 => WasmType::I31Ref,
            _ => todo!(),
        },
        wast::core::HeapType::Concrete(idx) => WasmType::Ref(
            format!("${}", get_id_from_index(&idx).unwrap()),
            Nullable::False,
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wast::parser::{self, ParseBuffer};

    #[test]
    fn test_basic_conversion() {
        let wat = r#"
            (func $test (param $x i32) (result i32)
                i32.const 42
                local.set $x
                block $b1
                    i32.const 1
                    br_if $b1
                    i32.const 2
                end
                local.get $x
            )"#;

        let func = parse_wat_function(wat);

        assert_eq!(func.name, "test");
        assert_eq!(func.params.len(), 1);
        assert_eq!(func.results.len(), 1);

        let body = func.body.borrow();
        assert!(body.len() > 0);

        // Verify specific instructions
        assert!(matches!(body[0], W::I32Const(42)));
        assert!(matches!(body[1], W::LocalSet(ref name) if name == "$x"));
        assert!(matches!(body[body.len()-1], W::LocalGet(ref name) if name == "$x"));
    }

    #[test]
    fn test_complex_function() {
        let wat = r#"
            (func $complex (param $a anyref) (param $b i32) (result anyref)
                local.get $a
                local.get $b
                i32.const 1
                i32.add
                call $some_func
            )"#;

        let func = parse_wat_function(wat);

        assert_eq!(func.name, "complex");
        assert_eq!(func.params.len(), 2);
        assert_eq!(func.results.len(), 1);

        let body = func.body.borrow();
        assert!(matches!(body[0], W::LocalGet(ref name) if name == "$a"));
        assert!(matches!(body[1], W::LocalGet(ref name) if name == "$b"));
        assert!(matches!(body[2], W::I32Const(1)));
        assert!(matches!(body[3], W::I32Add));
        assert!(matches!(body[4], W::Call(ref name) if name == "$some_func"));
    }
}
