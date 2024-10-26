use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{self, write};

#[derive(Debug, Clone)]
pub enum WatInstruction {
    Local { name: String, type_: String },
    GlobalGet { name: String },
    LocalGet { name: String },
    LocalSet { name: String },
    Call { name: String, args: Vec<Box<WatInstruction>> },
    I32Const { value: i32 },
    I32Eqz,
    F64Const { value: f64 },
    StructNew { name: String },
    ArrayNew { name: String, init: Box<WatInstruction>, length: Box<WatInstruction> },
    RefNull { type_: String },
    Ref(String),
    RefFunc { name: String },
    Type { name: String },
    Return,
    Block { label: String, instructions: Vec<Box<WatInstruction>> },
    Loop { label: String, instructions: Vec<Box<WatInstruction>> },
    If { condition: Option<Box<WatInstruction>>, then: Vec<Box<WatInstruction>>, r#else: Option<Vec<Box<WatInstruction>>> },
    BrIf(String),
    Br(String),
    Instruction { name: String, args: Vec<Box<WatInstruction>> },
    Empty,
    List { instructions: Vec<Box<WatInstruction>> },
    Log,
    Identifier(String),
    Drop,
    LocalTee(String),
    RefI31(Box<WatInstruction>),
}

impl WatInstruction {
    pub fn local(name: impl Into<String>, type_: impl Into<String>) -> Box<Self> {
        Box::new(Self::Local { name: name.into(), type_: type_.into() })
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

    pub fn call(name: impl Into<String>, args: Vec<Box<WatInstruction>>) -> Box<Self> {
        Box::new(Self::Call { name: name.into(), args })
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

    pub fn array_new(name: impl Into<String>, init: Box<WatInstruction>, length: Box<WatInstruction>) -> Box<Self> {
        Box::new(Self::ArrayNew { name: name.into(), init, length })
    }

    pub fn ref_null(type_: impl Into<String>) -> Box<Self> {
        Box::new(Self::RefNull { type_: type_.into() })
    }

    pub fn ref_func(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::RefFunc { name: name.into() })
    }

    pub fn type_(name: impl Into<String>) -> Box<Self> {
        Box::new(Self::Type { name: name.into() })
    }

    pub fn return_() -> Box<Self> {
        Box::new(Self::Return)
    }

    pub fn block(label: String, instructions: Vec<Box<WatInstruction>>) -> Box<Self> {
        Box::new(Self::Block { label, instructions })
    }

    pub fn r#loop(label: String, instructions: Vec<Box<WatInstruction>>) -> Box<Self> {
        Box::new(Self::Loop { label, instructions })
    }

    pub fn r#if(condition: Option<Box<WatInstruction>>, then: Vec<Box<WatInstruction>>, r#else: Option<Vec<Box<WatInstruction>>>) -> Box<Self> {
        Box::new(Self::If { condition, then, r#else })
    }

    pub fn br_if(label: impl Into<String>) -> Box<Self> {
        Box::new(Self::BrIf(label.into()))
    }

    pub fn br(label: impl Into<String>) -> Box<Self> {
        Box::new(Self::Br(label.into()))
    }

    pub fn instruction(name: impl Into<String>, args: Vec<Box<WatInstruction>>) -> Box<Self> {
        Box::new(Self::Instruction { name: name.into(), args })
    }

    pub fn empty() -> Box<Self> {
        Box::new(Self::Empty)
    }

    pub fn list(instructions: Vec<Box<WatInstruction>>) -> Box<Self> {
        Box::new(Self::List { instructions })
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
}

impl fmt::Display for WatInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WatInstruction::Local { name, type_ } => write!(f, "(local {} {})", name, type_),
            WatInstruction::GlobalGet { name } => write!(f, "(global.get {})", name),
            WatInstruction::LocalGet { name } => write!(f, "(local.get {})", name),
            WatInstruction::LocalSet { name } => write!(f, "(local.set {})", name),
            WatInstruction::Call { name, args } => {
                write!(f, "(call {}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            },
            WatInstruction::I32Const { value } => write!(f, "(i32.const {})", value),
            WatInstruction::F64Const { value } => write!(f, "(f64.const {})", value),
            WatInstruction::StructNew { name } => write!(f, "(struct.new {})", name),
            WatInstruction::ArrayNew { name, init, length } => write!(f, "(array.new {} {} {})", name, init, length),
            WatInstruction::RefNull { type_ } => write!(f, "(ref.null {})", type_),
            WatInstruction::RefFunc { name } => write!(f, "(ref.func ${})", name),
            WatInstruction::Return => write!(f, "return"),
            WatInstruction::Block { label, instructions } => {
                writeln!(f, "(block {label}")?;
                for instruction in instructions {
                    writeln!(f, "  {}", instruction)?;
                }
                write!(f, ")")
            },
            WatInstruction::Loop { label, instructions } => {
                writeln!(f, "(loop {label}")?;
                for instruction in instructions {
                    writeln!(f, "  {}", instruction)?;
                }
                write!(f, ")")
            },
            WatInstruction::If { condition, then, r#else } => {
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
            },
            WatInstruction::BrIf(label) => write!(f, "(br_if {})", label),
            WatInstruction::Br(label) => write!(f, "(br {})", label),
            WatInstruction::Instruction { name, args } => {
                write!(f, "({}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            },
            WatInstruction::Type { name } => write!(f, "${}", name),
            WatInstruction::Empty => Ok(()),
            WatInstruction::List { instructions } => {
                for instruction in instructions {
                    writeln!(f, "  {}", instruction)?;
                }
                Ok(())
            },
            WatInstruction::Log => {
                writeln!(f, "(call $log)")
            },
            WatInstruction::Identifier(s) => write!(f, "{}", s),
            WatInstruction::Ref(s) => write!(f, "(ref ${})", s),
            WatInstruction::Drop => writeln!(f, "(drop)"),
            WatInstruction::LocalTee(name) => write!(f, "(local.set {})", name),
            WatInstruction::I32Eqz => write!(f, "(i32.eqz)"),
            WatInstruction::RefI31(instruction) => write!(f, "(ref.i31 {instruction})"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct WatFunction {
    pub name: String,
    pub params: Vec<(String, String)>,
    pub results: Vec<String>,
    pub locals: HashSet<(String, String)>,
    pub body: VecDeque<Box<WatInstruction>>,
}

impl WatFunction {
    pub fn new(name: String) -> Self {
        WatFunction {
            name,
            params: Vec::new(),
            results: Vec::new(),
            locals: HashSet::new(),
            body: VecDeque::new(),
        }
    }

    pub fn add_param(&mut self, name: String, type_: String) {
        self.params.push((name, type_));
    }

    pub fn add_result(&mut self, type_: String) {
        self.results.push(type_);
    }

    pub fn add_local(&mut self, name: String, type_: String) {
        self.locals.insert((name, type_));
    }

    pub fn add_instruction(&mut self, instruction: WatInstruction) {
        self.body.push_back(Box::new(instruction));
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

#[derive(Debug, Clone)]
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
