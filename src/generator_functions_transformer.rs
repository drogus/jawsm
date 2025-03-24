use std::collections::HashMap;

use tarnik_ast::{
    cursor::InstructionsCursor, FunctionKey, IndexMap, InstructionsList, Nullable, VecDebug,
    WasmType, WatFunction, WatInstruction as W, WatModule,
};
use velcro::vec;

use crate::{VarType, WasmTranslator};

pub struct GeneratorFunctionsTransformer<'a> {
    module: &'a mut WatModule,
    translator: &'a mut WasmTranslator,
}

impl<'a> GeneratorFunctionsTransformer<'a> {
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
            if self.translator.generator_functions.contains(&name) {
                cursor.set_current_function_by_key(key).unwrap();
                let func = cursor.get_function_by_key_unchecked_mut(key);
                // we can't pass a mutable module further down as `cursor_for_function` borrows it
                // immutably. Does it's eaisest to modify the current function here.
                // TODO: maybe it's best if cursor takes a mutable reference to a module, which
                // would also solve the problem with multiple cursor instances potentially
                // invalidating each other's instruction sets
                let constructor_local =
                    func.add_local("$constructor", WasmType::ref_null("$Function"));
                let locals = func.locals.clone();
                let function = transform_generator_function(
                    constructor_local,
                    locals,
                    &mut cursor,
                    self.translator,
                );
                let name = function.name.clone();
                cursor.add_function(function);

                cursor.set_current_function(&name).unwrap();
                replace_returns(&mut cursor)
            }
        }
    }
}

fn replace_returns(cursor: &mut InstructionsCursor) {
    while let Some(instr) = cursor.next() {
        if instr.is_block_type() {
            let mut run = true;
            let mut iterator = cursor.enter_block().unwrap();
            while iterator.next(cursor) {
                replace_returns(cursor);
            }
            cursor.exit_block();
        } else if instr == W::Return {
            cursor.replace_current(vec![W::return_call("$return_generator_callback")]);
        }
    }
}

// TODO: what do we do here?
// We have to return a generator instance here. It has to keep the callback function inside.
// When someone calls next, it should call the function until it yields. Yield will be return
// then, and the return should be two things I think: a continuation and a returned value.
// If next() is called with an argument, it should be passed to the callback. If yield is used
// anywhere, the passed value should be returned then (but this will be handled on yield
// transformations).
fn transform_generator_function(
    constructor_local: String,
    locals: IndexMap<String, WasmType>,
    cursor: &mut InstructionsCursor,
    translator: &mut WasmTranslator,
) -> WatFunction {
    // TODO: I really need to change the implementation to always use $ prefixed names
    let callback_name = translator.gen_function_name(Some("generator-callback".to_string()));
    let mut function = WatFunction::new(&callback_name);
    function.add_param("$parentScope", &WasmType::r#ref("$Scope"));
    function.add_param("$this", &WasmType::Anyref);
    function.add_param("$arguments", &WasmType::r#ref("$JSArgs"));
    function.add_param("$meta", &WasmType::Anyref);
    function.add_result(WasmType::Anyref);

    function.locals = locals;

    function.add_local_exact("$__yield_result__", WasmType::Anyref);
    function.add_local_exact("$resolve-call-argument", WasmType::Anyref);

    // we need to leave arguments declaration so that we don't loose them later
    while cursor.next() != Some(W::call("$arguments_declared")) {}

    let start = cursor.current_position() + 1;
    while cursor.next().is_some() {}
    let end = cursor.current_position();

    let new_generator_instructions = vec![
        W::local_get("$scope"),
        W::ref_func(format!("${callback_name}")),
        // TODO: I'm not sure if this is should be undefined, but for now it seems fine
        W::ref_null_any(),
        W::call("$new_function"),
        W::call("$create_generator"),
    ];

    let old_body = cursor
        .replace_range(start, end, new_generator_instructions)
        .unwrap();

    function.add_local_exact("$scope", WasmType::r#ref("$Scope"));
    let callback_body = vec![
        W::local_get("$parentScope"),
        W::local_set("$scope"),
        ..old_body,
        // if nothing was returned, return an empty generator result
        W::return_call("$empty_generator_callback"),
    ];

    function.set_body(callback_body);

    function
}
