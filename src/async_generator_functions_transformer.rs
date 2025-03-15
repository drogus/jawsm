use std::collections::HashMap;

use tarnik_ast::{
    cursor::InstructionsCursor, FunctionKey, IndexMap, InstructionsList, Nullable, WasmType,
    WatFunction, WatInstruction as W, WatModule,
};
use velcro::vec;

use crate::{VarType, WasmTranslator};

pub struct AsyncGeneratorFunctionsTransformer<'a> {
    module: &'a mut WatModule,
    translator: &'a mut WasmTranslator,
}

impl<'a> AsyncGeneratorFunctionsTransformer<'a> {
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
            if self.translator.async_generator_functions.contains(&name) {
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
            cursor.replace_current(vec![
                W::local_get("$scope"),
                W::local_get("$__generator_resolve__"),
                W::return_call("$resolve_generator_callback"),
            ]);
        }
    }
}

// TODO: what do we do here?
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
    function.add_result(WasmType::Anyref);

    function.locals = locals;

    function.add_local_exact("$__yield_result__", WasmType::Anyref);
    function.add_local_exact("$__generator_resolve__", WasmType::r#ref("$Function"));
    function.add_local_exact("$resolve-call-argument", WasmType::Anyref);

    while cursor.next() != Some(W::call("$arguments_declared")) {}

    let start = cursor.current_position() + 1;
    while cursor.next().is_some() {}
    let end = cursor.current_position();

    let new_generator_instructions = vec![
        W::global_get("$global_scope"),
        W::ref_cast(WasmType::r#ref("$Scope")),
        W::ref_func(format!("${callback_name}")),
        // TODO: I'm not sure if this is should be undefined, but for now it seems fine
        W::ref_null_any(),
        W::call("$new_function"),
        W::call("$create_async_generator"),
    ];

    let old_body = cursor
        .replace_range(start, end, new_generator_instructions)
        .unwrap();

    function.add_local_exact("$scope", WasmType::r#ref("$Scope"));
    let callback_body = vec![
        // TODO: this is very similar to what we do when we split the function at yield
        // in AwaitKeywordTransformer. would be nice to refactor to not repeat the same thing
        W::local_get("$parentScope"),
        W::local_tee("$scope"),
        W::I32Const(-2),
        W::call("$get_variable"),
        W::local_set("$__yield_result__"),
        // first argument to the callback is a reolve function
        W::local_get("$arguments"),
        W::call("$first_argument_or_null"),
        W::ref_cast(WasmType::r#ref("$Function")),
        W::local_set("$__generator_resolve__"),
        ..old_body,
        // if nothing was returned, return an empty generator result
        W::local_get("$scope"),
        W::local_get("$__generator_resolve__"),
        W::return_call("$resolve_empty_generator_callback"),
    ];

    function.set_body(callback_body);

    function
}
