use std::{
    collections::{HashMap, VecDeque},
    mem, usize,
};

use rand::Rng;
use tarnik_ast::{
    cursor::InstructionsCursor, FunctionKey, InstructionsList, Nullable, StructField,
    TypeDefinition, WasmType, WatFunction, WatInstruction as W, WatModule,
};
use velcro::vec;
use walrus::Type;

use crate::{gen_function_name, VarType, WasmTranslator};

pub struct AwaitKeywordTransformer<'a> {
    module: &'a mut WatModule,
    translator: &'a mut WasmTranslator,
}

impl<'a> AwaitKeywordTransformer<'a> {
    pub fn new(module: &'a mut WatModule, translator: &'a mut WasmTranslator) -> Self {
        Self { module, translator }
    }

    pub fn transform(mut self) {
        self._transform();
    }

    fn _transform(&mut self) {
        let keys: Vec<FunctionKey> = self.module.function_keys();
        let mut cursor = self.module.cursor();
        transform(&mut cursor, keys);
    }
}

fn transform<'a>(cursor: &mut InstructionsCursor<'a>, keys: Vec<FunctionKey>) {
    for key in keys {
        transform_inner(cursor, key);
    }
}

fn transform_inner<'a>(cursor: &mut InstructionsCursor<'a>, key: FunctionKey) {
    let mut transformer = FunctionTranformer::new(cursor, key);
    transformer.transform(key);
}

struct FunctionTranformer<'a, 'b> {
    key: FunctionKey,
    cursor: &'a mut InstructionsCursor<'b>,
}

impl<'a, 'b> FunctionTranformer<'a, 'b> {
    fn new(cursor: &'a mut InstructionsCursor<'b>, key: FunctionKey) -> Self {
        FunctionTranformer { cursor, key }
    }

    fn transform(&mut self, key: FunctionKey) {
        self.cursor.set_current_function_by_key(key).unwrap();
        let mut replaced_await = false;
        let additional_keys = self.transform_await_keywords(1, vec![], &mut replaced_await);

        if replaced_await {
            self.cursor
                .current_function_mut()
                .add_instruction(W::ref_null_any());
        }

        if !additional_keys.is_empty() {
            for key in additional_keys {
                self.transform(key);
            }
        }
    }

    fn transform_await_keywords(
        &mut self,
        mut counter: usize,
        mut additional_keys: Vec<FunctionKey>,
        replaced_await: &mut bool,
    ) -> Vec<FunctionKey> {
        while let Some(instr) = self.cursor.next() {
            match instr {
                instr if instr.is_block_type() => {
                    // This looks like it's quite common, might be nice to prepare a standard way to
                    // traversing all of the blocks, maybe sth like a visitor pattern
                    let mut iterator = self.cursor.enter_block().unwrap();

                    while iterator.next(self.cursor) {
                        additional_keys =
                            self.transform_await_keywords(counter, additional_keys, replaced_await);
                    }

                    self.cursor.exit_block();
                }
                W::Call(name) if name == "$__await__" || name == "$__await_drop__" => {
                    // we stumbled upon await. we have to create a break point here. First, let's
                    // create a type that can hold all of the locals in the function, if it doesn't
                    // exist
                    let func = self.cursor.current_function().clone();
                    let type_name = get_type_name(&func.name);
                    let callback_function_name = get_func_name(&func.name, counter);
                    counter += 1;

                    let params: Vec<StructField> = func
                        .params
                        .iter()
                        .map(|(name, ty)| StructField {
                            name: name.clone(),
                            ty: ty.clone(),
                            mutable: false,
                        })
                        .collect();
                    let locals: Vec<StructField> = func
                        .locals
                        .iter()
                        .map(|(name, ty)| StructField {
                            name: Some(name.clone()),
                            ty: ty.clone(),
                            mutable: false,
                        })
                        .collect();

                    let original_position = self.cursor.current_position();
                    let mut target_position = original_position;
                    let mut min_position = self.cursor.earliest_argument().unwrap();
                    while self.cursor.next().is_some() {
                        if let Some(arg_position) = self.cursor.earliest_argument() {
                            if arg_position < min_position {
                                min_position = arg_position;
                                target_position = self.cursor.current_position();
                            }
                        }
                    }

                    // set ourselves just before the $__await__ call
                    // self.cursor.set_position(original_position - 1);
                    // let promise_instructions = self.cursor
                    // .replace_current_call_with_arguments(vec![W::local_get("$resolved_value")]);

                    self.cursor.set_position(target_position);
                    let instructions_list = self.cursor.current_instructions_list();
                    let instructions = instructions_list.borrow().clone();
                    let mut stack = self
                        .cursor
                        .analyze_stack_state(&instructions[min_position..original_position]);

                    let mut stack_types = vec![];
                    stack.pop(); // pop type being the current promise
                    let mut i = 1;
                    while let Some(ty) = stack.pop_front() {
                        stack_types.push(StructField {
                            name: Some(format!("$stack-state-{i}")),
                            ty,
                            mutable: false,
                        });
                        i += 1;
                    }

                    let ty = if let Some(TypeDefinition::Type(_, ty)) =
                        self.cursor.module().types.iter().find(
                            |t| matches!(t, TypeDefinition::Type(name, _) if name == &type_name),
                        ) {
                        ty.clone()
                    } else {
                        let ty = WasmType::Struct(vec![
                            ..stack_types.clone(),
                            ..params.clone(),
                            ..locals.clone(),
                        ]);
                        self.cursor.module_mut().add_type(&type_name, ty.clone());
                        ty
                    };

                    // TODO: at the moment I assume all of the params/locals are always named, which is
                    // the case so far, but it might change in the future, so it might be good to fix
                    // this
                    let params_instructions: InstructionsList = params
                        .iter()
                        .map(|struct_field| W::local_get(struct_field.clone().name.unwrap()))
                        .collect();
                    let locals_instructions: InstructionsList = locals
                        .iter()
                        .map(|struct_field| W::local_get(struct_field.clone().name.unwrap()))
                        .collect();

                    let promise_local = self
                        .cursor
                        .current_function_mut()
                        .add_local("$promise", WasmType::Anyref);

                    let state_local = self
                        .cursor
                        .current_function_mut()
                        .add_local("$state", WasmType::r#ref(&type_name));

                    let mut callback_function_name_without_dollar = callback_function_name.clone();
                    callback_function_name_without_dollar.remove(0);
                    // TODO: non dollar function names bite me again ;(
                    let mut callback_function =
                        WatFunction::new(callback_function_name_without_dollar.clone());
                    callback_function.params = func.params.clone();
                    callback_function.locals = func.locals.clone();
                    callback_function.results = func.results.clone();
                    let resolved_value_local =
                        callback_function.add_local("$resolved_value", WasmType::Anyref);

                    let r: String = rand::thread_rng()
                        .sample_iter(&rand::distributions::Alphanumeric)
                        .take(7)
                        .map(char::from)
                        .collect();
                    let function_state_local = callback_function
                        .add_local(format!("$function_state-{r}"), WasmType::Anyref);

                    self.cursor.set_position(original_position);
                    // replace await
                    let replacement_instruction = if name == "$__await__" {
                        W::local_get(&resolved_value_local)
                    } else {
                        W::Nop
                    };
                    let _ = self.cursor.replace_current(vec![replacement_instruction]);
                    self.cursor.previous().unwrap();
                    *replaced_await = true;

                    let old_instructions = self.cursor.replace_till_the_end_of_function(
                        vec![
                            W::local_set(&promise_local),
                            // save the entire function state in a prepared struct
                            // stack state should be on the stack here
                            ..params_instructions,
                            ..locals_instructions,
                            W::struct_new(&type_name),
                            W::local_set(&state_local),
                            W::local_get(&promise_local),
                            // we're setting scope to global scope for now, it will be fetched from state
                            // anyway
                            W::global_get("$global_scope"),
                            W::ref_cast(WasmType::r#ref("$Scope")),
                            // second argument is the function ref
                            W::ref_func(&callback_function_name),
                            // third argument is `this`, we will keep the state here
                            W::local_get(&state_local),
                            W::call("$new_function"),
                            W::call("$set_then_callback"),
                        ],
                        None,
                    );

                    let stack_extraction: InstructionsList = stack_types
                        .iter()
                        .flat_map(|struct_field| {
                            let name = &struct_field.clone().name.unwrap();
                            vec![
                                W::local_get(&function_state_local),
                                W::ref_cast(WasmType::r#ref(&type_name)),
                                W::struct_get(&type_name, name),
                            ]
                        })
                        .collect();
                    let params_extraction: InstructionsList = params
                        .iter()
                        .flat_map(|struct_field| {
                            let name = &struct_field.clone().name.unwrap();
                            vec![
                                W::local_get(&function_state_local),
                                W::ref_cast(WasmType::r#ref(&type_name)),
                                W::struct_get(&type_name, name),
                                W::local_set(name),
                            ]
                        })
                        .collect();
                    let locals_extraction: InstructionsList = locals
                        .iter()
                        .flat_map(|struct_field| {
                            let name = &struct_field.clone().name.unwrap();
                            vec![
                                W::local_get(&function_state_local),
                                W::ref_cast(WasmType::r#ref(&type_name)),
                                W::struct_get(&type_name, name),
                                W::local_set(name),
                            ]
                        })
                        .collect();

                    let body: InstructionsList = vec![
                        W::local_get("$arguments"),
                        W::I32Const(0),
                        W::call("$get_arguments_element"),
                        W::local_set(&resolved_value_local),
                        W::local_get("$this"),
                        W::local_set(&function_state_local),
                        ..stack_extraction,
                        ..params_extraction,
                        ..locals_extraction,
                        ..old_instructions.unwrap_or_default(),
                    ];

                    callback_function.set_body(body);
                    let key = self.cursor.module_mut().add_function(callback_function);
                    additional_keys.push(key);
                }
                _ => {}
            }
        }

        additional_keys
    }
}

fn get_type_name(name: &str) -> String {
    format!("$function-state-{name}")
}

fn get_func_name(name: &str, counter: usize) -> String {
    format!("$fc-{name}-{counter}")
}
