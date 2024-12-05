use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{self, write};

use boa_ast::statement::LabelledItem;

pub type InstructionsList = Vec<Box<WatInstruction>>;

#[derive(Debug, Clone)]
pub enum WatInstruction {
    Nop,
    Local {
        name: String,
        type_: String,
    },
    GlobalGet {
        name: String,
    },
    LocalGet {
        name: String,
    },
    LocalSet {
        name: String,
    },
    Call {
        name: String,
    },
    I32Const {
        value: i32,
    },
    I32Eqz,
    F64Const {
        value: f64,
    },
    StructNew {
        name: String,
    },
    ArrayNew {
        name: String,
        init: Box<WatInstruction>,
        length: Box<WatInstruction>,
    },
    RefNull {
        type_: String,
    },
    Ref(String),
    RefFunc {
        name: String,
    },
    Type {
        name: String,
    },
    Return,
    ReturnCall(String),
    Block {
        label: String,
        instructions: InstructionsList,
    },
    Loop {
        label: String,
        instructions: InstructionsList,
    },
    If {
        condition: Option<Box<WatInstruction>>,
        then: InstructionsList,
        r#else: Option<InstructionsList>,
    },
    BrIf(String),
    Br(String),
    Instruction {
        name: String,
        args: InstructionsList,
    },
    Empty,
    Log,
    Identifier(String),
    Drop,
    LocalTee(String),
    RefI31(Box<WatInstruction>),
    Throw(String),
    Try {
        try_block: InstructionsList,
        catches: Vec<InstructionsList>,
        catch_all: Option<InstructionsList>,
    },
    Catch(String, InstructionsList),
    CatchAll(Box<WatInstruction>),
}

impl WatInstruction {
    pub fn local(name: impl Into<String>, type_: impl Into<String>) -> Box<Self> {
        Box::new(Self::Local {
            name: name.into(),
            type_: type_.into(),
        })
    }

    pub fn global_get(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::GlobalGet { name: name.into() })
    }

    pub fn local_get(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::LocalGet { name: name.into() })
    }

    pub fn local_set(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::LocalSet { name: name.into() })
    }

    pub fn local_tee(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::LocalTee(name.into()))
    }

    pub fn call(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::Call { name: name.into() })
    }

    pub fn i32_const(value: i32) -> Box<Self> {
        Box::new(Self::I32Const { value })
    }

    pub fn f64_const(value: f64) -> Box<Self> {
        Box::new(Self::F64Const { value })
    }

    pub fn struct_new(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::StructNew { name: name.into() })
    }

    pub fn array_new(
        name: impl Into<String>,
        init: Box<WatInstruction>,
        length: Box<WatInstruction>,
    ) -> Box<Self> {
        Box::new(Self::ArrayNew {
            name: name.into(),
            init,
            length,
        })
    }

    pub fn ref_null(type_: impl Into<String>) -> Box<Self> {
        Box::new(Self::RefNull {
            type_: type_.into(),
        })
    }

    pub fn ref_func(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::RefFunc { name: name.into() })
    }

    pub fn type_(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::Type { name: name.into() })
    }

    pub fn r#return() -> Box<Self> {
        Box::new(Self::Return)
    }

    pub fn return_call(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::ReturnCall(name.into()))
    }

    pub fn block(label: impl Into<String>, instructions: InstructionsList) -> Box<Self> {
        Box::new(Self::Block {
            label: label.into(),
            instructions,
        })
    }

    pub fn r#loop(label: String, instructions: InstructionsList) -> Box<Self> {
        Box::new(Self::Loop {
            label,
            instructions,
        })
    }

    pub fn r#if(
        condition: Option<Box<WatInstruction>>,
        then: InstructionsList,
        r#else: Option<InstructionsList>,
    ) -> Box<Self> {
        Box::new(Self::If {
            condition,
            then,
            r#else,
        })
    }

    pub fn br_if(label: impl Into<String>) -> Box<Self> {
        Box::new(Self::BrIf(label.into()))
    }

    pub fn br(label: impl Into<String>) -> Box<Self> {
        Box::new(Self::Br(label.into()))
    }

    pub fn instruction(name: impl Into<String>, args: InstructionsList) -> Box<Self> {
        Box::new(Self::Instruction {
            name: name.into(),
            args,
        })
    }

    pub fn empty() -> Box<Self> {
        Box::new(Self::Empty)
    }

    pub fn drop() -> Box<Self> {
        Box::new(Self::Drop)
    }

    pub fn i32_eqz() -> Box<Self> {
        Box::new(Self::I32Eqz)
    }

    pub fn ref_i31(instruction: Box<WatInstruction>) -> Box<Self> {
        Box::new(Self::RefI31(instruction))
    }

    pub fn throw(label: impl Into<String>) -> Box<Self> {
        Box::new(Self::Throw(label.into()))
    }

    pub fn r#type(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::Type { name: name.into() })
    }

    pub fn r#try(
        try_block: InstructionsList,
        catches: Vec<InstructionsList>,
        catch_all: Option<InstructionsList>,
    ) -> Box<Self> {
        Box::new(Self::Try {
            try_block,
            catches,
            catch_all,
        })
    }

    pub fn catch(label: impl Into<String>, instr: InstructionsList) -> InstructionsList {
        vec![Box::new(Self::Catch(label.into(), instr))]
    }

    pub fn is_return(&self) -> bool {
        matches!(self, Self::Return)
    }

    pub fn is_call(&self) -> bool {
        matches!(self, Self::Call { .. })
    }
}

impl fmt::Display for WatInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WatInstruction::Nop => Ok(()),
            WatInstruction::Local { name, type_ } => write!(f, "(local {} {})", name, type_),
            WatInstruction::GlobalGet { name } => write!(f, "(global.get {})", name),
            WatInstruction::LocalGet { name } => write!(f, "(local.get {})", name),
            WatInstruction::LocalSet { name } => write!(f, "(local.set {})", name),
            WatInstruction::Call { name } => writeln!(f, "(call {})", name),
            WatInstruction::I32Const { value } => write!(f, "(i32.const {})", value),
            WatInstruction::F64Const { value } => write!(f, "(f64.const {})", value),
            WatInstruction::StructNew { name } => write!(f, "(struct.new {})", name),
            WatInstruction::ArrayNew { name, init, length } => {
                write!(f, "(array.new {} {} {})", name, init, length)
            }
            WatInstruction::RefNull { type_ } => write!(f, "(ref.null {})", type_),
            WatInstruction::RefFunc { name } => write!(f, "(ref.func ${})", name),
            WatInstruction::Return => write!(f, "return"),
            WatInstruction::ReturnCall(name) => write!(f, "(return_call {name})"),
            WatInstruction::Block {
                label,
                instructions,
            } => {
                writeln!(f, "(block {label}")?;
                for instruction in instructions {
                    writeln!(f, "  {}", instruction)?;
                }
                write!(f, ")")
            }
            WatInstruction::Loop {
                label,
                instructions,
            } => {
                writeln!(f, "(loop {label}")?;
                for instruction in instructions {
                    writeln!(f, "  {}", instruction)?;
                }
                write!(f, ")")
            }
            WatInstruction::If {
                condition,
                then,
                r#else,
            } => {
                let condition = if let Some(c) = condition {
                    format!("{c}")
                } else {
                    "".to_string()
                };
                write!(f, "(if {} (then", condition)?;
                for instruction in then {
                    write!(f, " {}", instruction)?;
                }
                write!(f, ")")?;
                if let Some(else_block) = r#else {
                    write!(f, " (else")?;
                    for instruction in else_block {
                        write!(f, " {}", instruction)?;
                    }
                    write!(f, ")")?;
                }
                write!(f, ")")
            }
            WatInstruction::BrIf(label) => write!(f, "(br_if {})", label),
            WatInstruction::Br(label) => write!(f, "(br {})", label),
            WatInstruction::Instruction { name, args } => {
                write!(f, "({}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
            WatInstruction::Type { name } => write!(f, "{}", name),
            WatInstruction::Empty => Ok(()),
            WatInstruction::Log => {
                writeln!(f, "(call $log)")
            }
            WatInstruction::Identifier(s) => write!(f, "{}", s),
            WatInstruction::Ref(s) => write!(f, "(ref ${})", s),
            WatInstruction::Drop => writeln!(f, "(drop)"),
            WatInstruction::LocalTee(name) => write!(f, "(local.tee {})", name),
            WatInstruction::I32Eqz => write!(f, "(i32.eqz)"),
            WatInstruction::RefI31(instruction) => write!(f, "(ref.i31 {instruction})"),
            WatInstruction::Throw(label) => write!(f, "(throw {label})"),
            WatInstruction::Try {
                try_block,
                catches,
                catch_all,
            } => {
                let try_block_str = try_block
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join("");
                writeln!(
                    f,
                    "\ntry\n{try_block_str}{}{}\nend",
                    catches
                        .iter()
                        .map(|c| c
                            .iter()
                            .map(|i| i.to_string())
                            .collect::<Vec<String>>()
                            .join(""))
                        .collect::<Vec<String>>()
                        .join(""),
                    catch_all
                        .clone()
                        .map(|c| c
                            .iter()
                            .map(|i| i.to_string())
                            .collect::<Vec<String>>()
                            .join(""))
                        .unwrap_or("".to_string())
                )
            }
            WatInstruction::Catch(label, instr) => {
                let instr_str = instr
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<String>>()
                    .join("");
                writeln!(f, "\ncatch {label}\n{instr_str}")
            }
            WatInstruction::CatchAll(instr) => writeln!(f, "\ncatch_all\n{instr}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct WatFunction {
    pub name: String,
    pub params: Vec<(String, String)>,
    pub results: Vec<String>,
    pub locals: HashMap<String, String>,
    pub locals_counters: HashMap<String, u32>,
    pub body: VecDeque<Box<WatInstruction>>,
}

impl WatFunction {
    pub fn new(name: String) -> Self {
        WatFunction {
            name,
            params: Vec::new(),
            results: Vec::new(),
            locals: HashMap::new(),
            locals_counters: HashMap::new(),
            body: VecDeque::new(),
        }
    }

    pub fn add_param(&mut self, name: impl Into<String>, type_: impl Into<String>) {
        self.params.push((name.into(), type_.into()));
    }

    pub fn add_result(&mut self, type_: impl Into<String>) {
        self.results.push(type_.into());
    }

    pub fn add_local_exact(&mut self, name: impl Into<String>, r#type: impl Into<String>) {
        self.locals.insert(name.into(), r#type.into());
    }

    pub fn add_local(&mut self, name: impl Into<String>, r#type: impl Into<String>) -> String {
        let r#type = r#type.into();
        let name = name.into();

        let counter = self.locals_counters.entry(name.clone()).or_insert(0);
        *counter += 1;
        let name = format!("{name}-{counter}");
        self.locals.insert(name.clone(), r#type);

        name
    }

    pub fn add_instruction(&mut self, instruction: Box<WatInstruction>) {
        self.body.push_back(instruction);
    }

    pub fn add_instructions(&mut self, instructions: InstructionsList) {
        for instruction in instructions {
            self.body.push_back(instruction);
        }
    }
}

impl fmt::Display for WatFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(func ${}", self.name)?;
        for (name, type_) in &self.params {
            write!(f, " (param {} {})", name, type_)?;
        }
        for result in &self.results {
            write!(f, " (result {})", result)?;
        }
        writeln!(f)?;
        for (name, type_) in &self.locals {
            writeln!(f, "  (local {} {})", name, type_)?;
        }
        for instruction in &self.body {
            writeln!(f, "  {}", instruction)?;
        }
        writeln!(f, ")")
    }
}

#[derive(Debug, Clone, Default)]
pub struct WatModule {
    pub types: HashMap<String, Vec<String>>,
    pub imports: Vec<(String, String, String)>,
    pub functions: Vec<WatFunction>,
    pub exports: Vec<(String, String)>,
    pub globals: Vec<(String, String, WatInstruction)>,
}

impl WatModule {
    pub fn new() -> Self {
        WatModule {
            types: HashMap::new(),
            imports: Vec::new(),
            functions: Vec::new(),
            exports: Vec::new(),
            globals: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: WatFunction) {
        self.functions.push(function);
    }

    pub fn get_function_mut(&mut self, name: &str) -> Option<&mut WatFunction> {
        self.functions.iter_mut().find(|f| f.name == name)
    }
}

impl fmt::Display for WatModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Types
        for (name, params) in &self.types {
            write!(f, "  (type ${} (func", name)?;
            for param in params {
                write!(f, " {}", param)?;
            }
            writeln!(f, "))")?;
        }

        // Imports
        for (module, name, type_) in &self.imports {
            writeln!(f, "  (import \"{}\" \"{}\" {})", module, name, type_)?;
        }

        // Function declarations
        for function in &self.functions {
            write!(f, "(elem declare func ${})\n", function.name)?;
        }

        // Functions
        for function in &self.functions {
            write!(f, "  {}", function)?;
        }

        // Exports
        for (name, internal_name) in &self.exports {
            writeln!(f, "  (export \"{}\" {})", name, internal_name)?;
        }

        // Globals
        for (name, type_, init) in &self.globals {
            writeln!(f, "  (global ${} {} {})", name, type_, init)?;
        }

        Ok(())
    }
}
