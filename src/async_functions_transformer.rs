use std::collections::HashMap;

use tarnik_ast::{
    cursor::InstructionsCursor, FunctionKey, InstructionsList, Nullable, WasmType, WatFunction,
    WatInstruction as W, WatModule,
};
use velcro::vec;

use crate::{gen_function_name, VarType, WasmTranslator};

pub struct AsyncFunctionsTransformer<'a> {
    module: &'a mut WatModule,
    translator: &'a mut WasmTranslator,
}

impl<'a> AsyncFunctionsTransformer<'a> {
    pub fn new(module: &'a mut WatModule, translator: &'a mut WasmTranslator) -> Self {
        Self { module, translator }
    }

    pub fn transform(mut self) {
        self._transform();
    }

    fn _transform(&mut self) {
        let keys: Vec<FunctionKey> = self.module.function_keys();
        let mut cursor = self.module.cursor();
        for key in keys {
            let name = cursor.get_function_by_key_unchecked(key).name.clone();
            if self.translator.async_functions.contains(&name) {
                cursor.set_current_function_by_key(key).unwrap();
                let func = cursor.get_function_by_key_unchecked_mut(key);
                // we can't pass a mutable module further down as `cursor_for_function` borrows it
                // immutably. Does it's eaisest to modify the current function here.
                // TODO: maybe it's best if cursor takes a mutable reference to a module, which
                // would also solve the problem with multiple cursor instances potentially
                // invalidating each other's instruction sets
                let constructor_local =
                    func.add_local("$constructor", WasmType::r#ref("$Function"));
                let locals = func.locals.clone();
                let function = transform_async_function(
                    constructor_local,
                    locals,
                    &mut cursor,
                    self.translator,
                );
                cursor.add_function(function);

                replace_returns(&mut cursor)
            }
        }
    }
}

fn replace_returns(cursor: &mut InstructionsCursor) {
    while let Some(instr) = cursor.next() {
        if instr.is_block_type() {
            let mut run = true;
            cursor.enter_block().unwrap();
            while run {
                replace_returns(cursor);
                run = cursor.next_block_arm();
            }
            cursor.exit_block();
        } else if instr == W::Return {
            cursor.replace_current(vec![
                W::local_set("$resolve-call-argument"),
                W::local_get("$resolve"),
                W::ref_null_any(),
                W::local_get("$resolve-call-argument"),
                W::call("$create_arguments_1"),
                W::call("$call_function"),
                W::ref_null_any(),
                W::Return,
            ]);
        }
    }
}

fn transform_async_function(
    constructor_local: String,
    locals: HashMap<String, WasmType>,
    cursor: &mut InstructionsCursor,
    translator: &mut WasmTranslator,
) -> WatFunction {
    // TODO: I really need to change the implementation to always use $ prefixed names
    let callback_name = gen_function_name(Some("promise-callback".to_string()));
    let mut function = WatFunction::new(&callback_name);
    function.add_param("$parentScope", &WasmType::r#ref("$Scope"));
    function.add_param("$this", &WasmType::Anyref);
    function.add_param("$arguments", &WasmType::r#ref("$JSArgs"));
    function.add_result(WasmType::Anyref);

    function.locals = locals;

    function.add_local_exact("$resolve", WasmType::r#ref("$Function"));
    function.add_local_exact("$resolve-call-argument", WasmType::Anyref);

    while cursor.next() != Some(W::call("$declare_arguments")) {}
    let start = cursor.current_position() + 1;
    while cursor.next().is_some() {}
    let end = cursor.current_position();

    let new_promise_instructions = vec![
        // fetch Promise constructor function
        W::local_get("$scope"),
        W::I32Const(translator.insert_data_string("Promise").0),
        W::call("$get_variable"),
        W::RefCast(WasmType::Ref("$Function".to_string(), Nullable::False)),
        W::local_tee(&constructor_local),
        // argument to call_function: this
        W::ref_null_any(),
        // create Promise arguments
        W::local_get("$scope"),
        W::ref_func(format!("${callback_name}")),
        // the callback function should use local this
        W::local_get("$this"),
        W::call("$new_function"),
        W::ArrayNewFixed("$JSArgs".to_string(), 1),
        W::call("$call_function"),
        W::ref_null_any(),
        W::global_get("$promise_prototype"),
        W::local_get(constructor_local),
        W::call("$return_new_instance_result"),
    ];

    let old_body = cursor
        .replace_range(start, end, new_promise_instructions)
        .unwrap();

    function.add_local_exact("$scope", WasmType::r#ref("$Scope"));
    let callback_body = vec![
        W::local_get("$parentScope"),
        // we don't need to create a new scope as we we want this callback to be
        // called as if it was the original function
        W::local_set("$scope"),
        W::local_get("$arguments"),
        W::I32Const(0),
        W::array_get("$JSArgs"),
        W::RefCast(WasmType::r#ref("$Function")),
        W::local_set("$resolve"),
        ..old_body,
    ];

    function.set_body(callback_body);

    function
}
