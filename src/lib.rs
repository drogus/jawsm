pub mod hoisting_transformer;
pub mod tail_call_transformer;
pub mod wasm;

#[derive(Clone)]
pub enum VarType {
    Const,
    Let,
    Var,
    Param,
    Function,
}

impl VarType {
    pub fn to_i32(&self) -> i32 {
        match self {
            VarType::Const => 0b00000001,
            VarType::Let => 0b00000010,
            VarType::Var => 0b00000100,
            VarType::Param => 0b00001000,
            VarType::Function => 0b00010000,
        }
    }
}
