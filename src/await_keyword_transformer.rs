use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    rc::Rc,
    usize,
};

use rand::Rng;
use tarnik_ast::{
    cursor::{InstructionsCursor, StackState},
    FunctionKey, IndexMap, InstructionsList, Nullable, StructField, TypeDefinition, VecDebug,
    WasmType, WatFunction, WatInstruction as W, WatModule,
};
use velcro::vec;
use walrus::Type;

use crate::{gen_function_name, VarType, WasmTranslator};

// TODO: now that this handles both await and yield keywords, this should be renamed
pub struct AwaitKeywordTransformer<'a> {
    module: &'a mut WatModule,
}

impl<'a> AwaitKeywordTransformer<'a> {
    pub fn new(module: &'a mut WatModule) -> Self {
        Self { module }
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
    if uses_await_or_yield(cursor, Some(key)) {
        let mut transformer = FunctionTranformer::new(cursor, key);
        transformer.transform(key);
    }
}

struct FunctionTranformer<'a, 'b> {
    key: FunctionKey,
    cursor: &'a mut InstructionsCursor<'b>,
    func_name: String,
    state_fields: Vec<StructField>,
    type_name: String,
    counter: usize,
    replaced_await: bool,
    label_to_func: HashMap<String, String>,
    // This is a simplification. If this converter was to work on the most general case
    // it would have to handle multiple catch arms and catch_all. So far we always
    // use only one cactch block with a label
    try_blocks_stack: VecDeque<(InstructionsList, String, InstructionsList)>,
}

impl<'a, 'b> FunctionTranformer<'a, 'b> {
    fn new(cursor: &'a mut InstructionsCursor<'b>, key: FunctionKey) -> Self {
        let func = cursor.module_mut().get_function_by_key_unchecked_mut(key);
        let func_name = func.name.clone();

        let type_name = get_type_name(&func_name);

        // the state type is a struct that keeps the entire state of the function: stack values and
        // all variables. It will be used to save the current state and allow us to restore from
        // the saved state when calling a promise callback or a result of a split function
        let ty = create_state_type(func);

        // now that the state type has been created we can add stuff that doesn't have to be on the
        // state stack, like state elements itselfs
        if func.locals.get("$state").is_none() {
            func.add_local_exact("$stack_state", WasmType::r#ref("$StackArray"));
            func.add_local_exact("$state", WasmType::r#ref(&type_name));
            func.add_local_exact("$stack_temp", WasmType::Anyref);
            cursor.module_mut().add_type(&type_name, ty.clone());
        }

        let WasmType::Struct(fields) = ty else {
            unreachable!("state type is a struct")
        };

        FunctionTranformer {
            cursor,
            key,
            func_name,
            type_name,
            state_fields: fields,
            counter: 0,
            replaced_await: false,
            label_to_func: Default::default(),
            try_blocks_stack: Default::default(),
        }
    }

    fn transform(&mut self, key: FunctionKey) {
        self.cursor.set_current_function_by_key(key).unwrap();

        let additional_keys = self.split_blocks(vec![]);

        // reset the cursor
        self.cursor.set_current_function_by_key(key).unwrap();

        self.transform_brs();

        // reset the cursor
        self.cursor.set_current_function_by_key(key).unwrap();

        let additional_keys = self.transform_await_keywords(additional_keys);

        if self.replaced_await {
            // WHY?
            self.cursor
                .current_function_mut()
                .add_instruction(W::ref_null_any());
        }

        if !additional_keys.is_empty() {
            for key in additional_keys {
                self.replaced_await = false;
                self.transform(key);
            }
        }
    }

    // TODO: do we even need this? it looks like brs are transformed in split_blocks already
    fn transform_brs(&mut self) {
        while let Some(instr) = self.cursor.next() {
            match instr {
                W::If { .. } => {
                    let mut iterator = self.cursor.enter_block().unwrap();

                    while iterator.next(self.cursor) {
                        self.transform_brs();
                    }

                    self.cursor.exit_block();
                }
                W::Try { .. } => {
                    let mut iterator = self.cursor.enter_block().unwrap();

                    while iterator.next(self.cursor) {
                        self.transform_brs();
                    }

                    self.cursor.exit_block();
                }
                W::Loop { .. } => {
                    let mut iterator = self.cursor.enter_block().unwrap();

                    while iterator.next(self.cursor) {
                        self.transform_brs();
                    }

                    self.cursor.exit_block();
                }
                W::Block { .. } => {
                    let mut iterator = self.cursor.enter_block().unwrap();

                    while iterator.next(self.cursor) {
                        self.transform_brs();
                    }

                    self.cursor.exit_block();
                }
                W::BrIf(label) => {
                    if let Some(func_name) = self.label_to_func.get(&label) {
                        let old_position = self.cursor.current_position();
                        self.cursor.replace_current(vec![W::r#if(
                            vec![
                                ..self.save_state_instructions(),
                                W::local_get("$state"),
                                W::return_call(with_dollar(func_name)),
                            ],
                            None,
                        )]);

                        // TODO: why do we have to return to the previous position?
                        self.cursor.set_position(old_position);
                    }
                }
                W::Br(label) => {
                    if let Some(func_name) = self.label_to_func.get(&label) {
                        let old_position = self.cursor.current_position();
                        self.cursor.replace_current(vec![
                            ..self.save_state_instructions(),
                            W::local_get("$state"),
                            W::return_call(with_dollar(func_name)),
                        ]);

                        self.cursor.set_position(old_position);
                    }
                }
                _ => {}
            }
        }
    }

    fn transform_await_keywords(
        &mut self,
        mut additional_keys: Vec<FunctionKey>,
    ) -> Vec<FunctionKey> {
        while let Some(instr) = self.cursor.next() {
            match instr {
                W::If { .. } => {
                    // This looks like it's quite common, might be nice to prepare a standard way to
                    // traversing all of the blocks, maybe sth like a visitor pattern
                    let mut iterator = self.cursor.enter_block().unwrap();

                    while iterator.next(self.cursor) {
                        additional_keys = self.transform_await_keywords(additional_keys);
                    }

                    self.cursor.exit_block();
                }
                W::Try {
                    try_block,
                    mut catches,
                    catch_all: _,
                } => {
                    let mut iterator = self.cursor.enter_block().unwrap();

                    let mut i = 0;
                    while iterator.next(self.cursor) {
                        if i == 0 {
                            // We always have at least one catch block at the moment, so it should
                            // be safe to unwrap()
                            let (catch_label, catch_instructions) = catches.pop().unwrap();

                            self.try_blocks_stack.push_back((
                                try_block.borrow().clone(),
                                catch_label,
                                catch_instructions.borrow().clone(),
                            ));
                        } else {
                            self.try_blocks_stack.pop_back();
                        }
                        additional_keys = self.transform_await_keywords(additional_keys);
                        i += 1;
                    }

                    self.cursor.exit_block();
                }
                W::Loop { .. } => {
                    let mut iterator = self.cursor.enter_block().unwrap();

                    while iterator.next(self.cursor) {
                        additional_keys = self.transform_await_keywords(additional_keys);
                    }

                    self.cursor.exit_block();
                }
                W::Block { .. } => {
                    let mut iterator = self.cursor.enter_block().unwrap();

                    while iterator.next(self.cursor) {
                        additional_keys = self.transform_await_keywords(additional_keys);
                    }

                    self.cursor.exit_block();
                }
                W::Call(name) if name == "$__await__" || name == "$__await_drop__" => {
                    let function = self.split_at_await(name);

                    let key = self.cursor.module_mut().add_function(function);
                    additional_keys.push(key);
                }
                W::Call(name) if name == "$__yield__" || name == "$__yield_drop__" => {
                    let function = self.split_at_yield(name);

                    let key = self.cursor.module_mut().add_function(function);
                    additional_keys.push(key);
                }

                _ => {}
            }
        }

        additional_keys
    }

    fn split_blocks(&mut self, mut additional_keys: Vec<FunctionKey>) -> Vec<FunctionKey> {
        while let Some(instr) = self.cursor.next() {
            match instr {
                W::If { .. } => {
                    // This looks like it's quite common, might be nice to prepare a standard way to
                    // traversing all of the blocks, maybe sth like a visitor pattern
                    let mut iterator = self.cursor.enter_block().unwrap();

                    while iterator.next(self.cursor) {
                        additional_keys = self.split_blocks(additional_keys);
                    }

                    self.cursor.exit_block();
                }
                W::Try { .. } => {
                    let mut iterator = self.cursor.enter_block().unwrap();

                    while iterator.next(self.cursor) {
                        additional_keys = self.split_blocks(additional_keys);
                    }

                    self.cursor.exit_block();
                }
                W::Loop {
                    label,
                    instructions,
                } => {
                    self.label_to_func
                        .insert(label.clone(), loop_start_name(&self.func_name, &label));
                    self.split_loop(label, instructions, &mut additional_keys);
                }
                W::Block {
                    signature: _,
                    label: block_label,
                    instructions: block_instructions,
                } => {
                    self.label_to_func.insert(
                        block_label.clone(),
                        block_end_name(&self.func_name, &block_label),
                    );
                    self.split_block(block_label, block_instructions, &mut additional_keys);
                }
                _ => {}
            }
        }

        additional_keys
    }

    fn get_func_name(&mut self) -> String {
        self.counter += 1;
        format!("fc-{}-{}", self.func_name, self.counter)
    }

    fn split_block(
        &mut self,
        block_label: String,
        instructions: Rc<RefCell<InstructionsList>>,
        mut additional_keys: &mut Vec<FunctionKey>,
    ) {
        // We're splitting a block here, so we need to put a split point at the *end* of the block
        // cause any br or br_if instructions can jump to the end of the block. We don't care as
        // much about the beginning of the block. So in essence we want to replace the block body
        // and everything that came before the block with straight code (like the block haven't
        // happened) and create a new function that will lead to the code after the block
        let block_end_name = block_end_name(&self.func_name, &block_label);
        let func = self.cursor.current_function().clone();
        let mut block_end_func = self.create_block_func(&func, without_dollar(&block_end_name));

        let block_instructions = instructions.borrow().clone();

        let old_position = self.cursor.current_position();

        // Replace the block with the insides of the block
        self.cursor.replace_current(block_instructions);
        // Now the cursor is at the end of the inserted code, so let's hop to the next instruction
        self.cursor.next();

        let new_instructions = vec![
            ..self.save_state_instructions(),
            W::local_get("$state"),
            W::return_call(with_dollar(&block_end_name)),
        ];

        if self.cursor.is_last() {
            // insert something to replace, so we can run the next step, if there's nothing
            // TODO: this could also be done by fixing
            // replace_till_the_end_of_function_try_catch_aware to work properly without a next
            // instruction
            self.cursor.insert_after_current(vec![W::Nop]);
            self.cursor.next();
        }

        // replace the rest of the code with a call to the "end of the block". We will extract all
        // this code into an "end of the block" callback function
        let rest = self
            .cursor
            .replace_till_the_end_of_function_try_catch_aware(new_instructions)
            .unwrap();

        // the end of the block function includes everything that was places after the block
        block_end_func.set_body(vec![
            ..self.extract_state_instructions(),
            ..rest,
            W::ref_null_any(),
        ]);

        self.cursor.set_position(old_position);

        // we will need to transform the newly added function, so pass it to additional_keys
        let key = self.cursor.module_mut().add_function(block_end_func);
        additional_keys.push(key);
    }

    fn split_loop(
        &mut self,
        label: String,
        instructions: Rc<RefCell<InstructionsList>>,
        additional_keys: &mut Vec<FunctionKey>,
    ) {
        // We split the loop in two places - beginning and the end of the loop. I *think* it would
        // be enough to split only in the beginning cause this is where any of the jumps would go,
        // but I was unable to remove the "end of the loop" split without breaking the code
        // TODO: try to remove the end of the loop function
        let loop_start_name = loop_start_name(&self.func_name, &label);
        let loop_end_name = loop_end_name(&self.func_name, &label);

        let func = self.cursor.current_function().clone();

        let mut loop_start_func = self.create_block_func(&func, without_dollar(&loop_start_name));
        let mut loop_end_func = self.create_block_func(&func, without_dollar(&loop_end_name));

        // Get the loop body instructions, append the call to the end function and put them in the
        // context of try/catch parent blocks
        let mut loop_instructions = instructions.borrow().clone();
        loop_instructions.append(&mut vec![
            ..self.save_state_instructions(),
            W::local_get("$state"),
            W::return_call(loop_end_name),
        ]);
        let loop_instructions = self
            .cursor
            .fetch_till_the_end_of_function_try_catch_aware(Some(loop_instructions))
            .unwrap();

        // Replace the loop with a call to the start function
        let old_position = self.cursor.current_position();
        self.cursor.replace_current(vec![
            ..self.save_state_instructions(),
            W::local_get("$state"),
            W::return_call(loop_start_name),
        ]);

        if self.cursor.is_last() {
            // insert something to replace, so we can run the next step, if there's nothing
            // TODO: this could also be done by fixing
            // replace_till_the_end_of_function_try_catch_aware to work properly without a next
            // instruction
            self.cursor.insert_after_current(vec![W::Nop]);
        }
        self.cursor.next();

        let rest = self
            .cursor
            .replace_till_the_end_of_function_try_catch_aware(vec![])
            .unwrap();

        // Set up loop start function body
        loop_start_func.set_body(vec![
            ..self.extract_state_instructions(),
            ..loop_instructions,
        ]);
        // Set up loop start function body
        loop_end_func.set_body(vec![..self.extract_state_instructions(), ..rest]);

        self.cursor.set_position(old_position);

        // Add both functions to module
        let start_key = self.cursor.module_mut().add_function(loop_start_func);
        additional_keys.push(start_key);
        let end_key = self.cursor.module_mut().add_function(loop_end_func);
        additional_keys.push(end_key);
    }

    fn split_at_yield(&mut self, name: String) -> WatFunction {
        // we need to break the function into two
        let func = self.cursor.current_function().clone();
        // new function name
        let callback_function_name = self.get_func_name();
        // type holding the state
        let type_name = &self.type_name;

        let mut stack = {
            let instructions_list = self.cursor.current_instructions_list();
            let instructions = instructions_list.borrow();
            self.cursor
                .analyze_stack_state(&instructions[0..self.cursor.current_position()])
        };

        let mut stack_instructions = vec![
            W::ref_null_any(),
            if stack.is_empty() {
                W::I32Const(0)
            } else {
                W::I32Const((stack.len() - 1) as i32)
            },
            W::array_new("$StackArray"),
            W::local_set("$stack_state"),
        ];
        let mut stack_extraction_instructions = vec![
            W::local_get("$state"),
            W::struct_get(type_name, "$stack"),
            W::local_set("$stack_state"),
        ];

        let mut stack_extraction_temp = VecDeque::new();
        stack.pop(); // pop type being the current promise
        let mut i = 0;
        while let Some(ty) = stack.pop() {
            if ty.is_numeric() {
                // a primitive, first create a heap allocated value
                let primitive_name = match ty {
                    WasmType::I32 => "$StackI32",
                    WasmType::I64 => "$StackI64",
                    WasmType::F32 => "$StackF32",
                    WasmType::F64 => "$StackF64",
                    _ => unreachable!(),
                };
                stack_instructions.append(&mut vec![
                    W::struct_new(primitive_name),
                    W::local_set("$stack_temp"),
                    W::local_get("$stack_state"),
                    W::I32Const(i),
                    W::local_get("$stack_temp"),
                    W::array_set("$StackArray"),
                ]);

                stack_extraction_temp.push_front(vec![
                    W::local_get("$stack_state"),
                    W::I32Const(i),
                    W::array_get("$StackArray"),
                    W::ref_cast(WasmType::r#ref(primitive_name)),
                    W::struct_get(primitive_name, "$value"),
                ]);
            } else {
                // it's an allocated object, just add to the array
                stack_instructions.append(&mut vec![
                    W::local_set("$stack_temp"),
                    W::local_get("$stack_state"),
                    W::I32Const(i),
                    W::local_get("$stack_temp"),
                    W::array_set("$StackArray"),
                ]);

                stack_extraction_temp.push_front(vec![
                    W::local_get("$stack_state"),
                    W::I32Const(i),
                    W::array_get("$StackArray"),
                    W::ref_cast(ty),
                ]);
            }
            i += 1;
        }

        for mut se in stack_extraction_temp {
            stack_extraction_instructions.append(&mut se);
        }

        stack_instructions.push(W::local_get("$stack_state"));

        // TODO: at the moment I assume all of the params/locals are always named, which is
        // the case so far, but it might change in the future, so it might be good to fix
        // this
        let params_and_locals_instructions: InstructionsList = self
            .state_fields
            .iter()
            .filter_map(|struct_field| {
                let name = struct_field.clone().name.unwrap();
                if name != "$stack" {
                    Some(W::local_get(with_dollar(&name)))
                } else {
                    None
                }
            })
            .collect();

        let yield_arg_local = self
            .cursor
            .current_function_mut()
            .add_local("$yield_arg", WasmType::Anyref);

        let mut callback_function = WatFunction::new(callback_function_name.clone());
        callback_function.params = vec![
            (Some("$parentScope".to_string()), WasmType::r#ref("$Scope")),
            (Some("$this".to_string()), WasmType::Anyref),
            (Some("$arguments".to_string()), WasmType::r#ref("$JSArgs")),
        ];
        // we need to filter out locals that may have been defined in parent functions that don't
        // ahdere to the JSFunc format
        callback_function.locals = func
            .locals
            .clone()
            .into_iter()
            .filter(|(name, _)| {
                *name != "$this" && *name != "$arguments" && *name != "$parentScope"
            })
            .collect::<IndexMap<String, WasmType>>();

        if !self.cursor.current_function().local_exists("$state") {
            callback_function
                .locals
                .insert("$state".to_string(), WasmType::r#ref(type_name));
        }

        callback_function.results = vec![WasmType::Anyref];
        callback_function.add_local_exact("$__yield_result__", WasmType::Anyref);

        // replace await
        let replacement_instructions = if name == "$__yield__" {
            vec![
                ..stack_extraction_instructions,
                W::local_get("$__yield_result__"),
            ]
        } else {
            vec![..stack_extraction_instructions, W::Nop]
        };

        let old_position = self.cursor.current_position();
        let _ = self.cursor.replace_current(replacement_instructions);
        self.cursor.set_position(old_position);
        self.replaced_await = true;

        // Here, we have to replace the await by creating a thenable. The old code (ie. the code
        // that will be executed after the thenable resolves) goes into the callback function
        let old_instructions = self
            .cursor
            .replace_till_the_end_of_function_try_catch_aware(vec![
                W::local_set(&yield_arg_local),
                // save the entire function state in a prepared struct
                // stack state should be on the stack here
                ..stack_instructions,
                ..params_and_locals_instructions,
                W::struct_new(type_name),
                W::local_set("$state"),
                // W::local_get(&thenable_obj_local),
                // we're setting scope to global scope for now, it will be fetched from state
                // anyway
                W::global_get("$global_scope"),
                W::ref_cast(WasmType::r#ref("$Scope")),
                // second argument is the function ref
                W::ref_func(format!("${callback_function_name}")),
                // third argument is `this`, we will keep the state here
                W::local_get("$state"),
                // this will be the callback function
                W::call("$new_function"),
                W::local_get(&yield_arg_local),
                W::return_call("$return_custom_generator_callback"),
            ]);

        let params_and_locals_extraction: InstructionsList = self
            .state_fields
            .iter()
            // TODO: remove these clones
            .filter(|s| {
                s.name.clone().unwrap() != "$stack"
                    && s.name.clone().unwrap() != "$__yield_result__"
            })
            .flat_map(|struct_field| {
                let name = &struct_field.clone().name.unwrap();
                vec![
                    W::local_get("$state"),
                    W::struct_get(type_name, name),
                    W::local_set(name),
                ]
            })
            .collect();

        let body: InstructionsList = vec![
            W::local_get("$arguments"),
            W::call("$first_argument_or_null"),
            W::local_set("$__yield_result__"),
            W::local_get("$this"),
            W::ref_cast(WasmType::r#ref(type_name)),
            W::local_set("$state"),
            ..params_and_locals_extraction,
            ..old_instructions.unwrap_or_default(),
            W::return_call("$empty_generator_callback"),
        ];

        self.cursor.set_position(old_position);

        callback_function.set_body(body);
        callback_function
    }

    fn split_at_await(&mut self, name: String) -> WatFunction {
        // we need to break the function into two
        let func = self.cursor.current_function().clone();
        // new function name
        let callback_function_name = self.get_func_name();
        // type holding the state
        let type_name = &self.type_name;

        let mut stack = {
            let instructions_list = self.cursor.current_instructions_list();
            let instructions = instructions_list.borrow();
            self.cursor
                .analyze_stack_state(&instructions[0..self.cursor.current_position()])
        };

        let mut stack_instructions = vec![
            W::ref_null_any(),
            if stack.is_empty() {
                W::I32Const(0)
            } else {
                W::I32Const((stack.len() - 1) as i32)
            },
            W::array_new("$StackArray"),
            W::local_set("$stack_state"),
        ];
        let mut stack_extraction_instructions = vec![
            W::local_get("$state"),
            W::struct_get(type_name, "$stack"),
            W::local_set("$stack_state"),
        ];

        let mut stack_extraction_temp = VecDeque::new();
        stack.pop(); // pop type being the current promise
        let mut i = 0;
        while let Some(ty) = stack.pop() {
            if ty.is_numeric() {
                // a primitive, first create a heap allocated value
                let primitive_name = match ty {
                    WasmType::I32 => "$StackI32",
                    WasmType::I64 => "$StackI64",
                    WasmType::F32 => "$StackF32",
                    WasmType::F64 => "$StackF64",
                    _ => unreachable!(),
                };
                stack_instructions.append(&mut vec![
                    W::struct_new(primitive_name),
                    W::local_set("$stack_temp"),
                    W::local_get("$stack_state"),
                    W::I32Const(i),
                    W::local_get("$stack_temp"),
                    W::array_set("$StackArray"),
                ]);

                stack_extraction_temp.push_front(vec![
                    W::local_get("$stack_state"),
                    W::I32Const(i),
                    W::array_get("$StackArray"),
                    W::ref_cast(WasmType::r#ref(primitive_name)),
                    W::struct_get(primitive_name, "$value"),
                ]);
            } else {
                // it's an allocated object, just add to the array
                stack_instructions.append(&mut vec![
                    W::local_set("$stack_temp"),
                    W::local_get("$stack_state"),
                    W::I32Const(i),
                    W::local_get("$stack_temp"),
                    W::array_set("$StackArray"),
                ]);

                stack_extraction_temp.push_front(vec![
                    W::local_get("$stack_state"),
                    W::I32Const(i),
                    W::array_get("$StackArray"),
                    W::ref_cast(ty),
                ]);
            }
            i += 1;
        }

        for mut se in stack_extraction_temp {
            stack_extraction_instructions.append(&mut se);
        }

        stack_instructions.push(W::local_get("$stack_state"));

        // TODO: at the moment I assume all of the params/locals are always named, which is
        // the case so far, but it might change in the future, so it might be good to fix
        // this
        let params_and_locals_instructions: InstructionsList = self
            .state_fields
            .iter()
            .filter_map(|struct_field| {
                let name = struct_field.clone().name.unwrap();
                if name != "$stack" {
                    Some(W::local_get(with_dollar(&name)))
                } else {
                    None
                }
            })
            .collect();

        let thenable_obj_local = self
            .cursor
            .current_function_mut()
            .add_local("$thenable_obj", WasmType::Anyref);

        let mut callback_function = WatFunction::new(callback_function_name.clone());
        callback_function.params = vec![
            (Some("$parentScope".to_string()), WasmType::r#ref("$Scope")),
            (Some("$this".to_string()), WasmType::Anyref),
            (Some("$arguments".to_string()), WasmType::r#ref("$JSArgs")),
        ];
        // we need to filter out locals that may have been defined in parent functions that don't
        // ahdere to the JSFunc format
        callback_function.locals = func
            .locals
            .clone()
            .into_iter()
            .filter(|(name, _)| {
                *name != "$this" && *name != "$arguments" && *name != "$parentScope"
            })
            .collect::<IndexMap<String, WasmType>>();

        if !self.cursor.current_function().local_exists("$state") {
            callback_function
                .locals
                .insert("$state".to_string(), WasmType::r#ref(type_name));
        }

        callback_function.results = vec![WasmType::Anyref];
        callback_function.add_local_exact("$__resolved_value__", WasmType::Anyref);

        // replace await
        let replacement_instructions = if name == "$__await__" {
            vec![
                ..stack_extraction_instructions,
                W::local_get("$__resolved_value__"),
            ]
        } else {
            vec![..stack_extraction_instructions, W::Nop]
        };

        let old_position = self.cursor.current_position();
        let _ = self.cursor.replace_current(replacement_instructions);
        self.cursor.set_position(old_position);
        self.replaced_await = true;

        // Here, we have to replace the await by creating a thenable. The old code (ie. the code
        // that will be executed after the thenable resolves) goes into the callback function
        let old_instructions = self
            .cursor
            .replace_till_the_end_of_function_try_catch_aware(vec![
                W::local_set(&thenable_obj_local),
                // save the entire function state in a prepared struct
                // stack state should be on the stack here
                ..stack_instructions,
                ..params_and_locals_instructions,
                W::struct_new(type_name),
                W::local_set("$state"),
                // W::local_get(&thenable_obj_local),
                // we're setting scope to global scope for now, it will be fetched from state
                // anyway
                W::global_get("$global_scope"),
                W::ref_cast(WasmType::r#ref("$Scope")),
                // second argument is the function ref
                W::ref_func(format!("${callback_function_name}")),
                // third argument is `this`, we will keep the state here
                W::local_get("$state"),
                // this will be the callback function
                W::call("$new_function"),
                W::local_get(&thenable_obj_local),
                W::return_call("$push_thenable"),
            ]);

        let params_and_locals_extraction: InstructionsList = self
            .state_fields
            .iter()
            // TODO: remove these clones
            .filter(|s| s.name.clone().unwrap() != "$stack")
            .flat_map(|struct_field| {
                let name = &struct_field.clone().name.unwrap();
                vec![
                    W::local_get("$state"),
                    W::struct_get(type_name, name),
                    W::local_set(name),
                ]
            })
            .collect();

        let body: InstructionsList = vec![
            W::local_get("$arguments"),
            W::I32Const(0),
            W::call("$get_arguments_element"),
            W::local_set("$__resolved_value__"),
            W::local_get("$this"),
            W::ref_cast(WasmType::r#ref(type_name)),
            W::local_set("$state"),
            ..params_and_locals_extraction,
            ..old_instructions.unwrap_or_default(),
            // TODO: this instruction creates a problem with nested awaits and it seems the code
            // works without it cause `old_instructions` usually end with a return anyway, but I
            // want to keep an eye on it, if this part breaks
            // W::ref_null_any(),
        ];

        self.cursor.set_position(old_position);

        callback_function.set_body(body);
        callback_function
    }

    fn create_block_func(&mut self, func: &WatFunction, name: String) -> WatFunction {
        let mut block_func = WatFunction::new(name);
        // here the callback function gets the scope, state, and this as arguments
        block_func.params = vec![(Some("$state".to_string()), WasmType::r#ref(&self.type_name))];
        block_func.locals = func
            .locals
            .clone()
            .into_iter()
            .filter(|(name, _)| *name != "$state")
            .collect::<IndexMap<String, WasmType>>();
        block_func
            .locals
            .insert("$arguments".to_string(), WasmType::r#ref("$JSArgs"));
        block_func
            .locals
            .insert("$parentScope".to_string(), WasmType::r#ref("$Scope"));
        block_func
            .locals
            .insert("$this".to_string(), WasmType::Anyref);
        block_func.results = vec![WasmType::Anyref];
        block_func
    }

    fn save_state_instructions(&self) -> InstructionsList {
        // TODO: at the moment I assume all of the params/locals are always named, which is
        // the case so far, but it might change in the future, so it might be good to fix
        // this
        let params_and_locals_instructions: InstructionsList = self
            .state_fields
            .iter()
            .filter_map(|struct_field| {
                let name = struct_field.clone().name.unwrap();
                if name != "$stack" {
                    Some(W::local_get(with_dollar(&name)))
                } else {
                    None
                }
            })
            .collect();

        // We don't need stack, so we create an empty stack
        let stack_instructions = vec![
            W::ref_null_any(),
            W::I32Const(0),
            W::array_new("$StackArray"),
        ];

        vec![
            ..stack_instructions,
            ..params_and_locals_instructions,
            W::struct_new(&self.type_name),
            W::local_set("$state"),
        ]
    }

    fn extract_state_instructions(&self) -> InstructionsList {
        // TODO: this could be optimized a bit. we don't really have to restore the entire state,
        // or even define all of the locals if they're not used
        self.state_fields
            .iter()
            // TODO: remove these clones
            .filter(|s| s.name.clone().unwrap() != "$stack")
            .flat_map(|struct_field| {
                let name = &struct_field.clone().name.unwrap();
                vec![
                    W::local_get("$state"),
                    W::struct_get(&self.type_name, name),
                    W::local_set(name),
                ]
            })
            .collect()
    }
}

fn block_end_name(func_name: &str, block_label: &str) -> String {
    format!(
        "${}-block-{}-end",
        without_dollar(func_name),
        without_dollar(block_label)
    )
}

fn loop_start_name(func_name: &str, label: &str) -> String {
    format!(
        "${}-loop-{}-start",
        without_dollar(func_name),
        without_dollar(label)
    )
}

fn loop_end_name(func_name: &str, label: &str) -> String {
    format!(
        "${}-loop-{}-end",
        without_dollar(func_name),
        without_dollar(label)
    )
}

fn get_type_name(name: &str) -> String {
    format!("$function-state-{name}")
}

fn create_state_type(func: &WatFunction) -> WasmType {
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

    WasmType::Struct(vec![
        // this will hold a list of values from the stack
        StructField {
            name: Some("$stack".to_string()),
            ty: WasmType::r#ref("$StackArray"),
            mutable: false,
        },
        ..params.clone(),
        ..locals.clone(),
    ])
}

fn uses_await_or_yield(cursor: &mut InstructionsCursor, key: Option<FunctionKey>) -> bool {
    if let Some(key) = key {
        cursor.set_current_function_by_key(key).unwrap();
    }

    while let Some(instr) = cursor.next() {
        match instr {
            instr if instr.is_block_type() => {
                let mut iterator = cursor.enter_block().unwrap();

                while iterator.next(cursor) {
                    if uses_await_or_yield(cursor, None) {
                        return true;
                    }
                }

                cursor.exit_block();
            }
            W::Call(name)
                if name == "$__await__"
                    || name == "$__await_drop__"
                    || name == "$__yield__"
                    || name == "$__yield_drop__" =>
            {
                return true;
            }
            _ => {}
        }
    }

    false
}

fn without_dollar(name: &str) -> String {
    if name.starts_with("$") {
        let mut chars = name.chars();
        chars.next();
        chars.as_str().to_string()
    } else {
        format!("{name}")
    }
}

fn with_dollar(name: &str) -> String {
    if name.starts_with("$") {
        name.to_string()
    } else {
        format!("${name}")
    }
}

#[cfg(test)]
mod tests {
    use crate::await_keyword_transformer::with_dollar;

    use super::AwaitKeywordTransformer;
    use tarnik_ast::{
        InstructionsList, Nullable, WasmType, WatFunction, WatInstruction as W, WatModule,
    };
    use wast::{
        core::{Instruction, ValType},
        token::Index,
    };

    use crate::{test_helpers::assert_functions_eq, wat_converter::parse_wat_function};

    #[test]
    fn test_nested_block_transformation() {
        let mut module = WatModule::new();

        // Input function in WAT format
        let input_wat = r#"
            (func $foo 
                (param $parentScope (ref $Scope))
                (param $this anyref)
                (param $arguments (ref $JSArgs))
                (local $local1-1 i32)

                i32.const 10
                local.set $local1-1
                block $outer
                    call $before-middle-block
                    block $middle
                        call $before-inner-block
                        block $inner
                            i32.const 5
                            call $__await_drop__
                            i32.const 1
                            br_if $outer
                            i32.const 20
                        end
                        call $after-inner-block
                    end
                    call $after-middle-block
                end
                local.get $local1-1
                i32.add
            )"#;

        // Expected main function after transformation
        let expected_main_wat = r#"
            (func $foo
                (param $parentScope (ref $Scope))
                (param $this anyref) 
                (param $arguments (ref $JSArgs))
                (local $local1-1 i32)
                (local $stack_state (ref $StackArray))
                (local $state (ref $function-state-foo))
                (local $stack_temp anyref)
                (local $thenable_obj-1 anyref)

                i32.const 10
                local.set $local1-1
                call $before-middle-block
                call $before-inner-block
                i32.const 5
                local.set $thenable_obj-1
                ref.null any
                i32.const 0
                array.new $StackArray
                local.set $stack_state
                local.get $stack_state
                local.get $parentScope
                local.get $this
                local.get $arguments
                local.get $local1-1
                struct.new $function-state-foo
                local.set $state
                global.get $global_scope
                ref.cast (ref  $Scope)
                ref.func $fc-foo-1
                local.get $state
                call $new_function
                local.get $thenable_obj-1
                return_call $push_thenable
                ref.null any
            )"#;

        let expected_foo_callback = r#"
            (func $fc-foo-1
                (param $parentScope (ref  $Scope))
                (param $this anyref)
                (param $arguments (ref  $JSArgs))
                (result anyref )
                (local $local1-1 i32)
                (local $stack_state (ref  $StackArray))
                (local $state (ref  $function-state-foo))
                (local $stack_temp anyref)
                (local $__resolved_value__ anyref)
           
                local.get $arguments
                i32.const 0
                call $get_arguments_element
                local.set $__resolved_value__
                local.get $this
                ref.cast (ref  $function-state-foo)
                local.set $state
                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                local.get $state
                struct.get $function-state-foo $local1-1
                local.set $local1-1
                local.get $state
                struct.get $function-state-foo $stack
                local.set $stack_state
                nop
                i32.const 1
                if
                  ref.null any
                  i32.const 0
                  array.new $StackArray
                  local.get $parentScope
                  local.get $this
                  local.get $arguments
                  local.get $local1-1
                  struct.new $function-state-foo
                  local.set $state
                  local.get $state
                  return_call $foo-block-outer-end
                end
                i32.const 20
                ref.null any
                i32.const 0
                array.new $StackArray
                local.get $parentScope
                local.get $this
                local.get $arguments
                local.get $local1-1
                struct.new $function-state-foo
                local.set $state
                local.get $state
                return_call $foo-block-inner-end
            )
        "#;

        // Expected end block function for $inner-block
        let expected_end_inner_block_wat = r#"
            (func $foo-block-inner-end
                (param $state (ref $function-state-foo))
                (result anyref)
                (local $local1-1 i32)
                (local $stack_state (ref $StackArray))
                (local $stack_temp anyref)
                (local $arguments (ref $JSArgs))
                (local $parentScope (ref $Scope))
                (local $this anyref)

                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                local.get $state
                struct.get $function-state-foo $local1-1
                local.set $local1-1
                call $after-inner-block
                ref.null any
                i32.const 0
                array.new $StackArray
                local.get $parentScope
                local.get $this
                local.get $arguments
                local.get $local1-1
                struct.new $function-state-foo
                local.set $state
                local.get $state
                return_call $foo-block-middle-end
                ref.null any
            )"#;

        let expected_end_middle_block_wat = r#"
            (func $foo-block-middle-end
                (param $state (ref $function-state-foo))
                (result anyref)
                (local $local1-1 i32)
                (local $stack_state (ref $StackArray))
                (local $stack_temp anyref)
                (local $arguments (ref $JSArgs))
                (local $parentScope (ref $Scope))
                (local $this anyref)

                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                local.get $state
                struct.get $function-state-foo $local1-1
                local.set $local1-1
                call $after-middle-block
                ref.null any
                i32.const 0
                array.new $StackArray
                local.get $parentScope
                local.get $this
                local.get $arguments
                local.get $local1-1
                struct.new $function-state-foo
                local.set $state
                local.get $state
                return_call $foo-block-outer-end
                ref.null any
            )"#;

        // Expected end block function for $outer-block
        let expected_end_outer_block_wat = r#"
            (func $foo-block-outer-end
                (param $state (ref $function-state-foo))
                (result anyref)
                (local $local1-1 i32)
                (local $stack_state (ref $StackArray))
                (local $stack_temp anyref)
                (local $arguments (ref $JSArgs))
                (local $parentScope (ref $Scope))
                (local $this anyref)

                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                local.get $state
                struct.get $function-state-foo $local1-1
                local.set $local1-1
                local.get $local1-1
                i32.add
                ref.null any
            )"#;

        // Parse and add input function
        let input_func = parse_wat_function(input_wat);
        module.add_function(input_func);

        // Transform
        let transformer = AwaitKeywordTransformer::new(&mut module);
        transformer.transform();

        // Parse expected functions
        let expected_main = parse_wat_function(expected_main_wat);
        let expected_end_inner_block = parse_wat_function(expected_end_inner_block_wat);
        let expected_end_middle_block = parse_wat_function(expected_end_middle_block_wat);
        let expected_end_outer_block = parse_wat_function(expected_end_outer_block_wat);
        let expected_foo_callback = parse_wat_function(&expected_foo_callback);

        // Compare results
        let main_func = module.get_function("$foo").unwrap();
        println!(
            "{:?}",
            module
                .functions()
                .iter()
                .map(|f| f.name.clone())
                .collect::<Vec<String>>()
        );
        let end_inner_block_func = module.get_function("$foo-block-inner-end").unwrap();
        let end_middle_block_func = module.get_function("$foo-block-middle-end").unwrap();
        let end_outer_block_func = module.get_function("$foo-block-outer-end").unwrap();
        let foo_callback_func = module.get_function("$fc-foo-1").unwrap();

        assert_functions_eq(&expected_main, main_func);
        assert_functions_eq(&expected_end_inner_block, end_inner_block_func);
        assert_functions_eq(&expected_end_middle_block, end_middle_block_func);
        assert_functions_eq(&expected_end_outer_block, end_outer_block_func);
        assert_functions_eq(&expected_foo_callback, foo_callback_func);
    }

    #[test]
    fn test_block_transformation() {
        let mut module = WatModule::new();

        // Input function in WAT format
        let input_wat = r#"
            (func $foo 
                (param $parentScope (ref $Scope))
                (param $this anyref)
                (param $arguments (ref $JSArgs))
                (local $local1-1 i32)

                i32.const 10
                local.set $local1-1
                block $test-block
                    i32.const 101
                    call $__await_drop__
                    i32.const 1
                    br_if $test-block
                    i32.const 20
                end
                local.get $local1-1
                i32.add
            )"#;

        // Expected main function after transformation
        let expected_main_wat = r#"
            (func $foo
                (param $parentScope (ref $Scope))
                (param $this anyref) 
                (param $arguments (ref $JSArgs))
                (local $local1-1 i32)
                (local $stack_state (ref $StackArray))
                (local $state (ref $function-state-foo))
                (local $stack_temp anyref)
                (local $thenable_obj-1 anyref)

                i32.const 10
                local.set $local1-1
                i32.const 101
                local.set $thenable_obj-1
                ref.null any
                i32.const 0
                array.new $StackArray
                local.set $stack_state
                local.get $stack_state
                local.get $parentScope
                local.get $this
                local.get $arguments
                local.get $local1-1
                struct.new $function-state-foo
                local.set $state
                global.get $global_scope
                ref.cast (ref  $Scope)
                ref.func $fc-foo-1
                local.get $state
                call $new_function
                local.get $thenable_obj-1
                return_call $push_thenable
                ref.null any
            )"#;

        let expected_callback_function = r#"
            (func $fc-foo-1
                (param $parentScope (ref  $Scope))
                (param $this anyref)
                (param $arguments (ref  $JSArgs))
                (result anyref )
                (local $local1-1 i32)
                (local $stack_state (ref  $StackArray))
                (local $state (ref  $function-state-foo))
                (local $stack_temp anyref)
                (local $__resolved_value__ anyref)

                local.get $arguments
                i32.const 0
                call $get_arguments_element
                local.set $__resolved_value__
                local.get $this
                ref.cast (ref  $function-state-foo)
                local.set $state
                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                local.get $state
                struct.get $function-state-foo $local1-1
                local.set $local1-1
                local.get $state
                struct.get $function-state-foo $stack
                local.set $stack_state
                nop
                i32.const 1
                if
                  ref.null any
                  i32.const 0
                  array.new $StackArray
                  local.get $parentScope
                  local.get $this
                  local.get $arguments
                  local.get $local1-1
                  struct.new $function-state-foo
                  local.set $state
                  local.get $state
                  return_call $foo-block-test-block-end
                end
                i32.const 20
                ref.null any
                i32.const 0
                array.new $StackArray
                local.get $parentScope
                local.get $this
                local.get $arguments
                local.get $local1-1
                struct.new $function-state-foo
                local.set $state
                local.get $state
                return_call $foo-block-test-block-end
            )"#;

        // Expected end block function
        let expected_end_block_wat = r#"
            (func $foo-block-test-block-end
                (param $state (ref $function-state-foo))
                (result anyref)
                (local $local1-1 i32)
                (local $stack_state (ref $StackArray))
                (local $stack_temp anyref)
                (local $arguments (ref $JSArgs))
                (local $parentScope (ref $Scope))
                (local $this anyref)

                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                local.get $state
                struct.get $function-state-foo $local1-1
                local.set $local1-1
                local.get $local1-1
                i32.add
                ref.null any
            )"#;

        // Parse and add input function
        let input_func = parse_wat_function(input_wat);
        module.add_function(input_func);

        // Transform
        let transformer = AwaitKeywordTransformer::new(&mut module);
        transformer.transform();

        // Parse expected functions
        let expected_main = parse_wat_function(expected_main_wat);
        let expected_end_block = parse_wat_function(expected_end_block_wat);
        let expected_callback_func = parse_wat_function(&expected_callback_function);

        // Compare results
        let main_func = module.get_function("$foo").unwrap();
        let end_block_func = module.get_function("$foo-block-test-block-end").unwrap();
        let callback_func = module.get_function("$fc-foo-1").unwrap();

        assert_functions_eq(&expected_main, main_func);
        assert_functions_eq(&expected_end_block, end_block_func);
        assert_functions_eq(&expected_callback_func, callback_func);
    }

    #[test]
    fn test_loop_transformation() {
        let mut module = WatModule::new();

        // Input function in WAT format
        let input_wat = r#"
            (func $foo 
                (param $parentScope (ref $Scope))
                (param $this anyref)
                (param $arguments (ref $JSArgs))
                (local $counter i32)

                i32.const 0
                local.set $counter
                loop $test_loop
                    local.get $counter
                    i32.const 1
                    i32.add
                    local.set $counter
                    i32.const 101
                    call $__await_drop__
                    local.get $counter
                    i32.const 5
                    i32.lt_s
                    br_if $test_loop
                    call $after_br_if
                end
                local.get $counter
                drop
            )"#;

        // Expected main function after transformation
        let expected_main_wat = r#"
            (func $foo
                (param $parentScope (ref $Scope))
                (param $this anyref)
                (param $arguments (ref $JSArgs))
                (local $counter i32)
                (local $stack_state (ref $StackArray))
                (local $state (ref $function-state-foo))
                (local $stack_temp anyref)

                i32.const 0
                local.set $counter
                ref.null any
                i32.const 0
                array.new $StackArray
                local.get $parentScope
                local.get $this
                local.get $arguments
                local.get $counter
                struct.new $function-state-foo
                local.set $state
                local.get $state
                return_call $foo-loop-test_loop-start
            )"#;

        // Expected loop function after transformation
        let expected_loop_wat = r#"
            (func $foo-loop-test_loop-start
                (param $state (ref $function-state-foo))
                (result anyref)
                (local $counter i32)
                (local $stack_state (ref $StackArray))
                (local $stack_temp anyref)
                (local $arguments (ref $JSArgs))
                (local $parentScope (ref $Scope))
                (local $this anyref)
                (local $thenable_obj-1 anyref)

                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                local.get $state
                struct.get $function-state-foo $counter
                local.set $counter
                local.get $counter
                i32.const 1
                i32.add
                local.set $counter
                i32.const 101
                local.set $thenable_obj-1
                ref.null any
                i32.const 0
                array.new $StackArray
                local.set $stack_state
                local.get $stack_state
                local.get $parentScope
                local.get $this
                local.get $arguments
                local.get $counter
                struct.new $function-state-foo
                local.set $state
                global.get $global_scope
                ref.cast (ref  $Scope)
                ref.func $fc-foo-1
                local.get $state
                call $new_function
                local.get $thenable_obj-1
                return_call $push_thenable
                ref.null any
            )"#;

        let expected_callback_wat = r#"
            (func $fc-foo-1
                (param $parentScope (ref  $Scope))
                (param $this anyref)
                (param $arguments (ref  $JSArgs))
                (result anyref )
                (local $counter i32)
                (local $stack_state (ref  $StackArray))
                (local $stack_temp anyref)
                (local $state (ref  $function-state-foo))
                (local $__resolved_value__ anyref)

                local.get $arguments
                i32.const 0
                call $get_arguments_element
                local.set $__resolved_value__
                local.get $this
                ref.cast (ref  $function-state-foo)
                local.set $state
                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                local.get $state
                struct.get $function-state-foo $counter
                local.set $counter
                local.get $state
                struct.get $function-state-foo $stack
                local.set $stack_state
                nop
                local.get $counter
                i32.const 5
                i32.lt_s
                if
                  ref.null any
                  i32.const 0
                  array.new $StackArray
                  local.get $parentScope
                  local.get $this
                  local.get $arguments
                  local.get $counter
                  struct.new $function-state-foo
                  local.set $state
                  local.get $state
                  return_call $foo-loop-test_loop-start
                end
                call $after_br_if
                ref.null any
                i32.const 0
                array.new $StackArray
                local.get $parentScope
                local.get $this
                local.get $arguments
                local.get $counter
                struct.new $function-state-foo
                local.set $state
                local.get $state
                return_call $foo-loop-test_loop-end
            )"#;

        // Parse and add input function
        let input_func = parse_wat_function(input_wat);
        module.add_function(input_func);

        // Transform
        let transformer = AwaitKeywordTransformer::new(&mut module);
        transformer.transform();

        // Parse expected functions
        let expected_main = parse_wat_function(expected_main_wat);
        let expected_loop = parse_wat_function(expected_loop_wat);
        let expected_callback = parse_wat_function(&expected_callback_wat);

        // Compare results
        let main_func = module.get_function("$foo").unwrap();
        let loop_func = module.get_function("$foo-loop-test_loop-start").unwrap();
        let callback_func = module.get_function("$fc-foo-1").unwrap();

        assert_functions_eq(&expected_main, main_func);
        assert_functions_eq(&expected_loop, loop_func);
        assert_functions_eq(&expected_callback, callback_func);
    }

    #[test]
    fn test_loop_transformation_with_try_catch() {
        let mut module = WatModule::new();

        // Input function in WAT format
        let input_wat = r#"
            (func $foo 
                (param $parentScope (ref $Scope))
                (param $this anyref)
                (param $arguments (ref $JSArgs))
                (local $counter i32)

                try
                    i32.const 0
                    local.set $counter
                    loop $test_loop
                        local.get $counter
                        i32.const 1
                        i32.add
                        local.set $counter
                        i32.const 101
                        call $__await_drop__
                        local.get $counter
                        i32.const 5
                        i32.lt_s
                        br_if $test_loop
                        call $after_br_if
                    end
                    local.get $counter
                    drop
                catch $Exception
                    call $log
                end

                i32.const 202
                call $log
            )"#;

        // Expected main function after transformation
        let expected_main_wat = r#"
            (func $foo
                (param $parentScope (ref $Scope))
                (param $this anyref)
                (param $arguments (ref $JSArgs))
                (local $counter i32)
                (local $stack_state (ref $StackArray))
                (local $state (ref $function-state-foo))
                (local $stack_temp anyref)

                try
                  i32.const 0
                  local.set $counter
                  ref.null any
                  i32.const 0
                  array.new $StackArray
                  local.get $parentScope
                  local.get $this
                  local.get $arguments
                  local.get $counter
                  struct.new $function-state-foo
                  local.set $state
                  local.get $state
                  return_call $foo-loop-test_loop-start
                catch $Exception
                  call $log
                end
                i32.const 202
                call $log
            )"#;

        // Expected loop function after transformation
        let expected_loop_wat = r#"
            (func $foo-loop-test_loop-start
                (param $state (ref $function-state-foo))
                (result anyref)
                (local $counter i32)
                (local $stack_state (ref $StackArray))
                (local $stack_temp anyref)
                (local $arguments (ref $JSArgs))
                (local $parentScope (ref $Scope))
                (local $this anyref)
                (local $thenable_obj-1 anyref)

                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                local.get $state
                struct.get $function-state-foo $counter
                local.set $counter
                try
                  local.get $counter
                  i32.const 1
                  i32.add
                  local.set $counter
                  i32.const 101
                  local.set $thenable_obj-1
                  ref.null any
                  i32.const 0
                  array.new $StackArray
                  local.set $stack_state
                  local.get $stack_state
                  local.get $parentScope
                  local.get $this
                  local.get $arguments
                  local.get $counter
                  struct.new $function-state-foo
                  local.set $state
                  global.get $global_scope
                  ref.cast (ref  $Scope)
                  ref.func $fc-foo-1
                  local.get $state
                  call $new_function
                  local.get $thenable_obj-1
                  return_call $push_thenable
                catch $Exception
                  call $log
                end
                i32.const 202
                call $log
                ref.null any
            )"#;

        let expected_callback_wat = r#"
            (func $fc-foo-1
                (param $parentScope (ref  $Scope))
                (param $this anyref)
                (param $arguments (ref  $JSArgs))
                (result anyref )
                (local $counter i32)
                (local $stack_state (ref  $StackArray))
                (local $stack_temp anyref)
                (local $state (ref  $function-state-foo))
                (local $__resolved_value__ anyref)

                local.get $arguments
                i32.const 0
                call $get_arguments_element
                local.set $__resolved_value__
                local.get $this
                ref.cast (ref  $function-state-foo)
                local.set $state
                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                local.get $state
                struct.get $function-state-foo $counter
                local.set $counter
                try
                  local.get $state
                  struct.get $function-state-foo $stack
                  local.set $stack_state
                  nop
                  local.get $counter
                  i32.const 5
                  i32.lt_s
                  if
                    ref.null any
                    i32.const 0
                    array.new $StackArray
                    local.get $parentScope
                    local.get $this
                    local.get $arguments
                    local.get $counter
                    struct.new $function-state-foo
                    local.set $state
                    local.get $state
                    return_call $foo-loop-test_loop-start
                  end
                  call $after_br_if
                  ref.null any
                  i32.const 0
                  array.new $StackArray
                  local.get $parentScope
                  local.get $this
                  local.get $arguments
                  local.get $counter
                  struct.new $function-state-foo
                  local.set $state
                  local.get $state
                  return_call $foo-loop-test_loop-end
                catch $Exception
                  call $log
                end
                i32.const 202
                call $log
            )"#;

        // Parse and add input function
        let input_func = parse_wat_function(input_wat);
        module.add_function(input_func);

        // Transform
        let transformer = AwaitKeywordTransformer::new(&mut module);
        transformer.transform();

        // Parse expected functions
        let expected_main = parse_wat_function(expected_main_wat);
        let expected_loop = parse_wat_function(expected_loop_wat);
        let expected_callback = parse_wat_function(&expected_callback_wat);

        // Compare results
        let main_func = module.get_function("$foo").unwrap();
        let loop_func = module.get_function("$foo-loop-test_loop-start").unwrap();
        let callback_func = module.get_function("$fc-foo-1").unwrap();

        assert_functions_eq(&expected_main, main_func);
        assert_functions_eq(&expected_loop, loop_func);
        assert_functions_eq(&expected_callback, callback_func);
    }

    #[test]
    fn test_simple_try_catch() {
        let mut module = WatModule::new();

        // Input function in WAT format
        let input_wat = r#"
            (func $foo 
                (param $parentScope (ref $Scope))
                (param $this anyref)
                (param $arguments (ref $JSArgs))

                call $before-try-catch
                try
                    call $before-await
                    call $__await_drop__
                    call $after-await
                catch $Exception
                    call $inside-catch
                end
                call $after-try-catch
            )"#;

        // Expected main function after transformation
        let expected_main_wat = r#"
            (func $foo
                (param $parentScope (ref $Scope))
                (param $this anyref)
                (param $arguments (ref $JSArgs))
                (local $stack_state (ref $StackArray))
                (local $state (ref $function-state-foo))
                (local $stack_temp anyref)
                (local $thenable_obj-1 anyref)

                call $before-try-catch
                try
                  call $before-await
                  local.set $thenable_obj-1
                  ref.null any
                  i32.const 0
                  array.new $StackArray
                  local.set $stack_state
                  local.get $stack_state
                  local.get $parentScope
                  local.get $this
                  local.get $arguments
                  struct.new $function-state-foo
                  local.set $state
                  global.get $global_scope
                  ref.cast (ref  $Scope)
                  ref.func $fc-foo-1
                  local.get $state
                  call $new_function
                  local.get $thenable_obj-1
                  return_call $push_thenable
                catch $Exception
                  call $inside-catch
                end
                call $after-try-catch
                ref.null any
            )"#;

        let expected_callback_wat = r#"
            (func $fc-foo-1
                (param $parentScope (ref  $Scope))
                (param $this anyref)
                (param $arguments (ref  $JSArgs))
                (result anyref )
                (local $stack_state (ref  $StackArray))
                (local $state (ref  $function-state-foo))
                (local $stack_temp anyref)
                (local $__resolved_value__ anyref)
                
                local.get $arguments
                i32.const 0
                call $get_arguments_element
                local.set $__resolved_value__
                local.get $this
                ref.cast (ref  $function-state-foo)
                local.set $state
                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                try
                    local.get $state
                    struct.get $function-state-foo $stack
                    local.set $stack_state
                    nop
                    call $after-await
                catch $Exception
                  call $inside-catch
                end
                call $after-try-catch
            )"#;

        // Parse and add input function
        let input_func = parse_wat_function(input_wat);
        module.add_function(input_func);

        // Transform
        let transformer = AwaitKeywordTransformer::new(&mut module);
        transformer.transform();

        // Parse expected functions
        let expected_main = parse_wat_function(expected_main_wat);
        let expected_callback = parse_wat_function(expected_callback_wat);

        // Compare results
        let main_func = module.get_function("$foo").unwrap();
        let callback_func = module.get_function("$fc-foo-1").unwrap();

        assert_functions_eq(&expected_main, main_func);
        assert_functions_eq(&expected_callback, callback_func);
    }

    #[test]
    fn test_try_catch_in_a_loop() {
        let mut module = WatModule::new();

        // Input function in WAT format
        let input_wat = r#"
            (func $foo 
                (param $parentScope (ref $Scope))
                (param $this anyref)
                (param $arguments (ref $JSArgs))

                call $before-loop
                loop $loop
                    block $break
                        call $before-try-catch
                        try
                            call $before-await
                            call $__await__
                            call $after-await
                        catch $Exception
                            call $inside-catch
                        end
                        call $after-try-catch

                        br $loop
                    end
                end
                call $after-loop
            )"#;

        // Expected main function after transformation
        let expected_main_wat = r#"
            (func $foo
                (param $parentScope (ref $Scope))
                (param $this anyref)
                (param $arguments (ref $JSArgs))
                (local $stack_state (ref $StackArray))
                (local $state (ref $function-state-foo))
                (local $stack_temp anyref)

                call $before-loop
                ref.null any
                i32.const 0
                array.new $StackArray
                local.get $parentScope
                local.get $this
                local.get $arguments
                struct.new $function-state-foo
                local.set $state
                local.get $state
                return_call $foo-loop-loop-start
            )"#;

        let expected_callback_wat = r#"
            (func $fc-foo-1
                (param $parentScope (ref  $Scope))
                (param $this anyref)
                (param $arguments (ref  $JSArgs))
                (result anyref )
                (local $stack_state (ref  $StackArray))
                (local $stack_temp anyref)
                (local $state (ref  $function-state-foo))
                (local $__resolved_value__ anyref)

                local.get $arguments
                i32.const 0
                call $get_arguments_element
                local.set $__resolved_value__
                local.get $this
                ref.cast (ref  $function-state-foo)
                local.set $state
                local.get $state
                struct.get $function-state-foo $parentScope
                local.set $parentScope
                local.get $state
                struct.get $function-state-foo $this
                local.set $this
                local.get $state
                struct.get $function-state-foo $arguments
                local.set $arguments
                try
                    local.get $state
                    struct.get $function-state-foo $stack
                    local.set $stack_state
                    local.get $__resolved_value__
                    call $after-await
                catch $Exception
                  call $inside-catch
                end
                call $after-try-catch
                ref.null any
                i32.const 0
                array.new $StackArray
                local.get $parentScope
                local.get $this
                local.get $arguments
                struct.new $function-state-foo
                local.set $state
                local.get $state
                return_call $foo-loop-loop-start
                ref.null any
                i32.const 0
                array.new $StackArray
                local.get $parentScope
                local.get $this
                local.get $arguments
                struct.new $function-state-foo
                local.set $state
                local.get $state
                return_call $foo-block-break-end
            )"#;

        // Parse and add input function
        let input_func = parse_wat_function(input_wat);
        module.add_function(input_func);

        // Transform
        let transformer = AwaitKeywordTransformer::new(&mut module);
        transformer.transform();

        // Parse expected functions
        let expected_main = parse_wat_function(expected_main_wat);
        let expected_callback = parse_wat_function(expected_callback_wat);

        // Compare results
        let main_func = module.get_function("$foo").unwrap();
        let callback_func = module.get_function("$fc-foo-1").unwrap();

        assert_functions_eq(&expected_main, main_func);
        assert_functions_eq(&expected_callback, callback_func);
    }
}
