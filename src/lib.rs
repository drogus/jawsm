use boa_ast::{
    declaration::{Binding, Declaration, LexicalDeclaration, VarDeclaration, VariableList},
    expression::{
        access::PropertyAccess,
        literal::{
            ArrayLiteral, Literal, ObjectLiteral, PropertyDefinition, TemplateElement,
            TemplateLiteral,
        },
        operator::{
            binary::{ArithmeticOp, BinaryOp, LogicalOp},
            update::UpdateTarget,
            Assign, Binary, Conditional, Unary, Update,
        },
        Await, Call, Expression, Identifier, New, Parenthesized,
    },
    function::{
        ArrowFunction, ClassElement, FormalParameterList, FunctionBody, FunctionExpression,
    },
    pattern::Pattern,
    property::PropertyName,
    statement::{
        Block, Catch, DoWhileLoop, Finally, ForInLoop, ForLoop, ForOfLoop, If, Return, Statement,
        Switch, Throw, Try, WhileLoop,
    },
    visitor::Visitor,
    StatementListItem,
};
use boa_interner::{Interner, Sym, ToInternedString};
use rand::{distributions::Alphanumeric, Rng};
use std::{collections::HashMap, io::Read, ops::ControlFlow};
use tarnik_ast::{
    Global, InstructionsList, Nullable, Signature, WasmType, WatFunction, WatInstruction as W,
    WatModule,
};
use velcro::vec;

pub mod async_functions_transformer;
pub mod await_keyword_transformer;
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

fn drop_if_no_use(mut instructions: InstructionsList, will_use_return: bool) -> InstructionsList {
    if !will_use_return {
        instructions.push(W::Drop);
    }

    instructions
}

pub fn gen_function_name(s: Option<String>) -> String {
    let r: String = rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(7)
        .map(char::from)
        .collect();

    if let Some(s) = s {
        let s = s
            .strip_prefix("get ")
            .or_else(|| s.strip_prefix("set "))
            .unwrap_or(&s);
        format!("{s}-{r}")
    } else {
        format!("function-{r}")
    }
}

pub struct WasmTranslator {
    pub module: WatModule,
    pub function_stack: Vec<WatFunction>,
    pub interner: Interner,
    pub functions: HashMap<String, String>,
    pub init_code: Vec<String>,
    pub string_offsets: HashMap<String, i32>,
    pub data_offset: i32,
    pub identifiers_map: HashMap<i32, i32>,
    pub current_block_number: u32,
    pub async_functions: Vec<String>,
}

impl WasmTranslator {
    pub fn new(interner: Interner, module: WatModule) -> Self {
        let function = WatFunction::new("init".to_string());
        let data_offset = module.data_offset as i32 + 4;
        let string_offsets = module
            .data
            .clone()
            .into_iter()
            .map(|(o, s)| (s, o as i32))
            .collect();
        Self {
            module,
            function_stack: vec![function],
            interner,
            functions: HashMap::new(),
            init_code: Vec::new(),
            string_offsets,
            data_offset: data_offset + 4,
            identifiers_map: HashMap::new(),
            current_block_number: 0,
            async_functions: Vec::new(),
        }
    }

    fn private_field_offset(&mut self, offset: i32) -> i32 {
        // private fields are interned with the same symbols as non private fields
        // I don't want to create special maps for private fields, though, so I will use
        // regular offsets. As offsets are i32 numbers, we can use part of the offset as
        // we won't likely need the entire space
        1_000_000_000 + offset
    }
    fn add_new_symbol(&mut self, sym: Sym, value: &str) -> i32 {
        if let Some(offset) = self.identifiers_map.get(&(sym.get() as i32)) {
            *offset
        } else {
            let (offset, _) = self.module.add_data(value.to_string());
            self.identifiers_map.insert(sym.get() as i32, offset as i32);
            offset as i32
        }
    }

    fn add_symbol(&mut self, sym: Sym) -> i32 {
        self.add_new_symbol(sym, &self.interner.resolve(sym).unwrap().to_string())
    }

    fn add_identifier(&mut self, identifier: &Identifier) -> i32 {
        self.add_symbol(identifier.sym())
    }

    fn current_function(&mut self) -> &mut WatFunction {
        self.function_stack.last_mut().unwrap()
    }

    fn enter_function(&mut self, function: WatFunction) {
        self.function_stack.push(function);
    }

    pub fn exit_function(&mut self) {
        let function = self.function_stack.pop().unwrap();
        self.module.add_function(function);
    }

    fn enter_block(&mut self) {
        self.current_block_number += 1;
    }

    fn exit_block(&mut self) {
        self.current_block_number -= 1;
    }

    fn current_block_name(&self) -> String {
        format!("$block-{}", self.current_block_number)
    }

    fn random_block_name(&self) -> String {
        let r: String = rand::thread_rng()
            .sample_iter(&Alphanumeric)
            .take(7)
            .map(char::from)
            .collect();

        format!("$block-{r}")
    }

    fn current_loop_name(&self) -> String {
        format!("$loop-{}", self.current_block_number)
    }

    fn current_loop_break_name(&self) -> String {
        format!("$break-{}", self.current_block_number)
    }

    fn current_continue_block_name(&self) -> String {
        format!("$continue-{}", self.current_block_number)
    }

    fn translate_return(&mut self, ret: &Return) -> InstructionsList {
        let mut instructions = Vec::new();
        if let Some(target) = ret.target() {
            instructions.append(&mut self.translate_expression(target, true));
        } else {
            instructions.push(W::ref_null_any());
        }
        instructions.push(W::r#return());
        instructions
    }

    fn translate_binding(
        &mut self,
        var_instructions: InstructionsList,
        binding: &Binding,
        init: Option<&Expression>,
        var_type: Option<VarType>,
    ) -> InstructionsList {
        let mut result = vec![];
        let assign = if let Some(init) = init {
            self.translate_expression(init, true)
        } else {
            vec![W::ref_null_any()]
        };
        match binding {
            Binding::Identifier(identifier) => {
                let offset = self.add_identifier(identifier);
                result.append(&mut vec![
                    W::local_get("$scope"),
                    W::i32_const(offset),
                    ..var_instructions,
                    ..assign.clone(),
                    W::call("$coalesce"),
                    W::i32_const(var_type.unwrap().to_i32()),
                    W::call("$declare_variable"),
                ]);
            }
            Binding::Pattern(pattern) => {
                result.append(&mut self.translate_pattern(
                    var_instructions,
                    assign.clone(),
                    pattern,
                    var_type,
                ));
            }
        }

        result
    }

    fn translate_pattern(
        &mut self,
        var_instructions: InstructionsList,
        assign: InstructionsList,
        pattern: &Pattern,
        var_type: Option<VarType>,
    ) -> InstructionsList {
        let current_argument = self
            .current_function()
            .add_local("$current_argument", WasmType::Anyref);
        let mut result = vec![
            ..var_instructions,
            ..assign.clone(),
            W::call("$coalesce"),
            W::call("$clone"),
            W::local_set(&current_argument),
        ];
        match pattern {
            Pattern::Object(object_pattern) => {
                use boa_ast::pattern::ObjectPatternElement;
                for element in object_pattern.bindings() {
                    match element {
                        ObjectPatternElement::SingleName {
                            name,
                            ident,
                            default_init,
                        } => {
                            let var_name_offset = self.add_identifier(ident);
                            let init_instructions = if let Some(init) = default_init {
                                self.translate_expression(init, true)
                            } else {
                                vec![W::ref_null_any()]
                            };
                            let var_type_index = if let Some(var_type) = &var_type {
                                var_type.to_i32()
                            } else {
                                -1
                            };
                            match name {
                                PropertyName::Literal(sym) => {
                                    let offset = self.add_symbol(sym.to_owned());
                                    result.append(&mut vec![
                                        W::local_get(&current_argument),
                                        W::I32Const(offset),
                                        W::I32Const(var_name_offset),
                                        W::local_get("$scope"),
                                        ..init_instructions,
                                        W::I32Const(var_type_index),
                                        W::call("$destructure_property_single_name"),
                                    ]);
                                }
                                PropertyName::Computed(expression) => {
                                    let mut instructions = vec![
                                        W::local_get(&current_argument),
                                        ..self.translate_expression(expression, true),
                                        W::call("$to_string"),
                                        W::I32Const(var_name_offset),
                                        W::local_get("$scope"),
                                        ..init_instructions,
                                        W::I32Const(var_type_index),
                                        W::call("$destructure_property_single_name_str"),
                                    ];
                                    result.append(&mut instructions);
                                }
                            }
                        }
                        ObjectPatternElement::RestProperty { ident } => {
                            let offset = self.add_identifier(ident);
                            result.append(&mut vec![
                                W::local_get("$scope"),
                                W::I32Const(offset),
                                W::local_get(&current_argument),
                                W::I32Const(var_type.clone().unwrap().to_i32()),
                                W::call("$declare_variable"),
                            ]);
                        }
                        ObjectPatternElement::AssignmentPropertyAccess {
                            name,
                            access,
                            default_init,
                        } => {
                            let init = if let Some(init) = default_init {
                                self.translate_expression(init, true)
                            } else {
                                vec![W::ref_null_any()]
                            };
                            let assign_instructions = match name {
                                PropertyName::Literal(sym) => {
                                    let offset = self.add_symbol(sym.to_owned());
                                    vec![
                                        W::local_get(&current_argument),
                                        W::I32Const(offset),
                                        W::call("$get_property_value"),
                                        ..init,
                                        W::call("$coalesce"),
                                    ]
                                }
                                PropertyName::Computed(expression) => {
                                    vec![
                                        W::local_get(&current_argument),
                                        ..self.translate_expression(expression, true),
                                        W::call("$to_string"),
                                        W::call("$get_property_value_str"),
                                        ..init,
                                        W::call("$coalesce"),
                                    ]
                                }
                            };
                            result.append(
                                &mut self
                                    .translate_property_access(access, Some(assign_instructions)),
                            );
                        }
                        ObjectPatternElement::AssignmentRestPropertyAccess { access } => {
                            let assign_instructions = vec![W::local_get(&current_argument)];

                            result.append(
                                &mut self
                                    .translate_property_access(access, Some(assign_instructions)),
                            );
                        }
                        ObjectPatternElement::Pattern {
                            name,
                            pattern,
                            default_init,
                        } => {
                            let var_instructions = match name {
                                PropertyName::Literal(sym) => {
                                    let offset = self.add_symbol(sym.to_owned());
                                    vec![
                                        W::local_get(&current_argument),
                                        W::I32Const(offset),
                                        W::call("$get_property_value"),
                                    ]
                                }
                                PropertyName::Computed(expression) => {
                                    vec![
                                        W::local_get(&current_argument),
                                        ..self.translate_expression(expression, true),
                                        W::call("$to_string"),
                                        W::call("$get_property_value_str"),
                                    ]
                                }
                            };
                            let assign = if let Some(init) = default_init {
                                self.translate_expression(init, true)
                            } else {
                                vec![W::ref_null_any()]
                            };
                            result.append(&mut self.translate_pattern(
                                var_instructions,
                                assign,
                                pattern,
                                var_type.clone(),
                            ));
                        }
                    }
                }
            }
            Pattern::Array(array_pattern) => {
                for (i, element) in array_pattern.bindings().iter().enumerate() {
                    match element {
                        boa_ast::pattern::ArrayPatternElement::Elision => {}
                        boa_ast::pattern::ArrayPatternElement::SingleName {
                            ident,
                            default_init,
                        } => {
                            let init = if let Some(init) = default_init {
                                self.translate_expression(init, true)
                            } else {
                                vec![W::ref_null_any()]
                            };
                            let offset = self.add_identifier(ident);
                            result.append(&mut vec![
                                W::local_get("$scope"),
                                W::I32Const(offset),
                                W::local_get(&current_argument),
                                W::I32Const(i as i32),
                                W::call("$get_array_element"),
                                ..init,
                                W::call("$coalesce"),
                                W::I32Const(var_type.clone().unwrap().to_i32()),
                                W::call("$declare_variable"),
                            ]);
                        }
                        boa_ast::pattern::ArrayPatternElement::PropertyAccess {
                            access,
                            default_init,
                        } => {
                            let init = if let Some(init) = default_init {
                                self.translate_expression(init, true)
                            } else {
                                vec![W::ref_null_any()]
                            };
                            let assign = vec![
                                W::local_get(&current_argument),
                                W::I32Const(i as i32),
                                W::call("$get_array_element"),
                                ..init,
                                W::call("$coalesce"),
                            ];
                            result
                                .append(&mut self.translate_property_access(access, Some(assign)));
                        }
                        boa_ast::pattern::ArrayPatternElement::Pattern {
                            pattern,
                            default_init,
                        } => {
                            let assign = if let Some(init) = default_init {
                                self.translate_expression(init, true)
                            } else {
                                vec![W::ref_null_any()]
                            };

                            let var_instructions = vec![
                                W::local_get(&current_argument),
                                W::I32Const(i as i32),
                                W::call("$get_array_element"),
                            ];

                            result.append(&mut self.translate_pattern(
                                var_instructions,
                                assign,
                                pattern,
                                var_type.clone(),
                            ));
                        }
                        boa_ast::pattern::ArrayPatternElement::SingleNameRest { ident } => {
                            let offset = self.add_identifier(ident);
                            result.append(&mut vec![
                                W::local_get("$scope"),
                                W::I32Const(offset),
                                W::local_get(&current_argument),
                                W::I32Const(i as i32),
                                W::call("$get_array_rest"),
                                W::I32Const(var_type.clone().unwrap().to_i32()),
                                W::call("$declare_variable"),
                            ]);
                        }
                        boa_ast::pattern::ArrayPatternElement::PropertyAccessRest { access } => {
                            let assign = vec![
                                W::local_get(&current_argument),
                                W::I32Const(i as i32),
                                W::call("$get_array_rest"),
                            ];
                            result
                                .append(&mut self.translate_property_access(access, Some(assign)));
                        }
                        boa_ast::pattern::ArrayPatternElement::PatternRest { pattern } => {
                            let var_instructions = vec![
                                W::local_get(&current_argument),
                                W::I32Const(i as i32),
                                W::call("$get_array_rest"),
                            ];
                            result.append(&mut self.translate_pattern(
                                var_instructions,
                                vec![W::ref_null_any()],
                                pattern,
                                var_type.clone(),
                            ));
                        }
                    }
                }
            }
        }

        result
    }

    fn translate_function_generic(
        &mut self,
        name: Option<Identifier>,
        params: &FormalParameterList,
        body: &FunctionBody,
    ) -> InstructionsList {
        let function_name = gen_function_name(name.map(|i| i.to_interned_string(&self.interner)));
        let wat_function = WatFunction::new(function_name.clone());
        self.enter_function(wat_function);

        self.current_function().add_param(
            "$parentScope".to_string(),
            &WasmType::Ref("$Scope".into(), Nullable::False),
        );
        self.current_function()
            .add_param("$this".to_string(), &WasmType::Anyref);
        self.current_function().add_param(
            "$arguments".to_string(),
            &WasmType::Ref("$JSArgs".into(), Nullable::False),
        );
        self.current_function().set_results(vec![WasmType::Anyref]);

        self.current_function()
            .add_local_exact("$scope", WasmType::Ref("$Scope".into(), Nullable::False));
        self.current_function().add_instructions(vec![
            W::local_get("$parentScope"),
            W::call("$new_scope"),
            W::local_set("$scope"),
            W::local_get("$scope"),
            W::local_get("$arguments"),
            W::call("$declare_arguments"),
        ]);

        // set parameters on the scope
        for (i, param) in params.as_ref().iter().enumerate() {
            let var_instructions = vec![
                W::local_get("$arguments"),
                W::i32_const(i as i32),
                W::call("$get_arguments_element"),
            ];
            let instructions = self.translate_binding(
                var_instructions,
                param.variable().binding(),
                param.variable().init(),
                Some(VarType::Param),
            );
            self.current_function().add_instructions(instructions);
        }

        for statement in body.statements() {
            match statement {
                boa_ast::StatementListItem::Statement(statement) => {
                    let res = self.translate_statement(statement);
                    self.current_function().add_instructions(res);
                }
                boa_ast::StatementListItem::Declaration(declaration) => {
                    let declaration = self.translate_declaration(declaration);
                    self.current_function().add_instructions(declaration);
                }
            }
        }

        // This is a bit dumb, but it will work for now - every $JSFunc
        // has to return a value. If we already returned this will get ignored
        // If not, ie. there is no return statement, we will return undefined
        self.current_function()
            .add_instructions(vec![W::ref_null_any(), W::r#return()]);

        self.exit_function();

        vec![
            W::local_get("$scope"),
            W::ref_func(format!("${function_name}")),
            W::ref_null_any(),
            W::call("$new_function"),
        ]
    }

    fn translate_function(&mut self, fun: &FunctionExpression) -> InstructionsList {
        self.translate_function_generic(fun.name(), fun.parameters(), fun.body())
    }

    fn translate_get_function(
        &mut self,
        name: Option<Identifier>,
        params: &FormalParameterList,
        body: &FunctionBody,
    ) -> InstructionsList {
        self.translate_function_generic(name, params, body)
    }

    fn translate_set_function(
        &mut self,
        name: Option<Identifier>,
        params: &FormalParameterList,
        body: &FunctionBody,
    ) -> InstructionsList {
        self.translate_function_generic(name, params, body)
    }

    fn translate_lexical(&mut self, decl: &LexicalDeclaration) -> InstructionsList {
        // println!(
        //     "translate lexical {}",
        //     decl.to_interned_string(&self.interner)
        // );
        match decl {
            LexicalDeclaration::Const(variable_list) => {
                self.translate_let_vars(variable_list, VarType::Const)
            }
            LexicalDeclaration::Let(variable_list) => {
                self.translate_let_vars(variable_list, VarType::Let)
            }
        }
    }

    fn translate_var(&mut self, decl: &VarDeclaration) -> InstructionsList {
        // TODO: variables behave a bit differently when it comes to hoisting
        // for now I just ignore it, but it should be fixed
        // https://developer.mozilla.org/en-US/docs/Glossary/Hoisting
        self.translate_let_vars(&decl.0, VarType::Var)
    }

    fn translate_call(
        &mut self,
        call: &Call,
        get_this: W,
        additional_instructions: Option<InstructionsList>,
        will_use_return: bool,
    ) -> InstructionsList {
        // println!(
        //     "translate_call {}",
        //     call.function().to_interned_string(&self.interner)
        // );
        let function_name = call.function().to_interned_string(&self.interner);
        let mut instructions = Vec::new();

        if function_name == "setTimeout" {
            if let Some(callback) = call.args().first() {
                let callback_var = self
                    .current_function()
                    .add_local("$callback", WasmType::Anyref);
                let duration_var = self
                    .current_function()
                    .add_local("$duration", WasmType::Anyref);
                instructions.append(&mut self.translate_expression(callback, true));
                instructions.push(W::local_set(&callback_var));

                let mut time = if let Some(time) = call.args().get(1) {
                    self.translate_expression(time, true)
                } else {
                    // pass undefined
                    vec![W::ref_null_any()]
                };
                instructions.append(&mut time);
                instructions.push(W::local_set(&duration_var));

                // the rest of arguments doesn't matter
                instructions.append(&mut vec![
                    W::local_get(&callback_var),
                    W::local_get(&duration_var),
                    W::call("$set_timeout"),
                ]);
            } else {
                // TODO: throw TypeError
            }
        } else {
            // Add a local for arguments to the current function
            let call_arguments = self.current_function().add_local(
                "$call_arguments",
                WasmType::Ref("$JSArgs".into(), Nullable::True),
            );
            let temp_arg = self
                .current_function()
                .add_local("$temp_arg", WasmType::Anyref);

            // Create the arguments array
            let args_count = call.args().len() as i32;

            instructions.append(&mut vec![
                W::ref_null_any(),
                W::i32_const(args_count),
                W::array_new("$JSArgs"),
                W::local_set(&call_arguments),
            ]);

            // Populate the arguments array
            for (index, arg) in call.args().iter().enumerate() {
                let mut arg_instruction = self.translate_expression(arg, true);
                instructions.append(&mut arg_instruction);
                instructions.append(&mut vec![
                    W::local_set(&temp_arg),
                    W::local_get(&call_arguments),
                    W::ref_cast(WasmType::r#ref("$JSArgs")),
                    W::i32_const(index as i32),
                    W::local_get(&temp_arg),
                    W::array_set("$JSArgs"),
                ]);
            }

            if function_name == "console.log" {
                instructions.append(&mut vec![
                    W::local_get(&call_arguments),
                    W::ref_cast(WasmType::r#ref("$JSArgs")),
                    W::call("$log"),
                    W::i32_const(1),
                ]);
            } else {
                // Translate the function expression
                let function_local = self
                    .current_function()
                    .add_local("$function", WasmType::Anyref);
                instructions.append(&mut self.translate_expression(call.function(), true));
                if let Some(mut additional_instructions) = additional_instructions {
                    instructions.push(W::local_tee(&function_local));
                    instructions.append(&mut additional_instructions);
                } else {
                    instructions.push(W::local_set(&function_local));
                }

                // Call the function
                instructions.append(&mut vec![
                    W::local_get(&function_local),
                    get_this,
                    W::local_get(&call_arguments),
                    W::ref_cast(WasmType::r#ref("$JSArgs")),
                    W::call("$call_function"),
                ]);
            }
        }

        if !will_use_return {
            instructions.push(W::drop());
        }
        instructions
    }

    fn translate_let_vars(
        &mut self,
        variable_list: &VariableList,
        var_type: VarType,
    ) -> InstructionsList {
        use boa_ast::declaration::Binding;

        let mut instructions = Vec::new();
        for var in variable_list.as_ref() {
            let init_instructions = if let Some(expression) = var.init() {
                self.translate_expression(expression, true)
            } else {
                vec![W::ref_null_any()]
            };
            let mut instr = match var.binding() {
                Binding::Identifier(identifier) => {
                    let offset = self.add_identifier(identifier);
                    vec![
                        W::local_get("$scope"),
                        W::i32_const(offset),
                        ..init_instructions,
                        W::i32_const(var_type.to_i32()),
                        W::call("$declare_variable"),
                    ]
                }
                Binding::Pattern(pattern) => self.translate_pattern(
                    init_instructions,
                    vec![W::ref_null_any()],
                    pattern,
                    Some(var_type.clone()),
                ),
            };
            instructions.append(&mut instr);
        }

        instructions
    }

    fn translate_binary(&mut self, binary: &Binary) -> InstructionsList {
        use boa_ast::expression::operator::binary::RelationalOp;

        // println!("Binary: {binary:#?}");
        match binary.op() {
            BinaryOp::Arithmetic(arithmetic_op) => {
                let func = match arithmetic_op {
                    ArithmeticOp::Add => "$add",
                    ArithmeticOp::Sub => "$sub",
                    ArithmeticOp::Div => "$div",
                    ArithmeticOp::Mul => "$mul",
                    ArithmeticOp::Exp => todo!(),
                    ArithmeticOp::Mod => "$mod_op",
                };
                // TODO: this will probably need translating to
                // multiple lines and saving to local vars
                let mut result = vec![];
                let mut lhs = self.translate_expression(binary.lhs(), true);
                let mut rhs = self.translate_expression(binary.rhs(), true);
                result.append(&mut lhs);
                result.append(&mut rhs);
                result.push(W::call(func.to_string()));
                result
            }
            BinaryOp::Bitwise(bitwise_op) => {
                use boa_ast::expression::operator::binary::BitwiseOp;
                let operation = match bitwise_op {
                    BitwiseOp::And => W::I32And,
                    BitwiseOp::Or => W::I32Or,
                    BitwiseOp::Xor => W::I32Xor,
                    BitwiseOp::Shl => W::I32Shl,
                    BitwiseOp::Shr => W::I32ShrS,
                    BitwiseOp::UShr => W::I32ShrU,
                };
                // TODO: convert args to primitive
                vec![
                    ..self.translate_expression(binary.lhs(), true),
                    W::ref_cast(WasmType::Ref("$Number".to_string(), Nullable::False)),
                    W::struct_get("$Number", "$value"),
                    W::I32TruncF64S,
                    ..self.translate_expression(binary.rhs(), true),
                    W::ref_cast(WasmType::Ref("$Number".to_string(), Nullable::False)),
                    W::struct_get("$Number", "$value"),
                    W::I32TruncF64S,
                    operation,
                    W::F64ConvertI32S,
                    W::call("$new_number"),
                ]
            }
            BinaryOp::Relational(relational_op) => {
                let func_name = match relational_op {
                    RelationalOp::Equal => "$loose_equal",
                    RelationalOp::NotEqual => "$loose_not_equal",
                    RelationalOp::StrictEqual => "$strict_equal",
                    RelationalOp::StrictNotEqual => "$strict_not_equal",
                    RelationalOp::GreaterThan => "$greater_than",
                    RelationalOp::GreaterThanOrEqual => "$greater_than_or_equal",
                    RelationalOp::LessThan => "$less_than",
                    RelationalOp::LessThanOrEqual => "$less_than_or_equal",
                    RelationalOp::In => "$operator_in",
                    RelationalOp::InstanceOf => "$instance_of",
                };
                let rhs = self.current_function().add_local("$rhs", WasmType::Anyref);
                let lhs = self.current_function().add_local("$lhs", WasmType::Anyref);

                vec![
                    ..self.translate_expression(binary.lhs(), true),
                    W::local_set(&lhs),
                    ..self.translate_expression(binary.rhs(), true),
                    W::local_set(&rhs),
                    W::local_get(&lhs),
                    W::local_get(&rhs),
                    W::call(func_name),
                ]
            }
            BinaryOp::Logical(logical_op) => {
                let func_name = match logical_op {
                    LogicalOp::And => "$logical_and",
                    LogicalOp::Or => "$logical_or",
                    LogicalOp::Coalesce => "$logical_coalesce",
                };
                let rhs = self.current_function().add_local("$rhs", WasmType::Anyref);
                let lhs = self.current_function().add_local("$lhs", WasmType::Anyref);

                vec![
                    ..self.translate_expression(binary.lhs(), true),
                    W::local_set(&lhs),
                    ..self.translate_expression(binary.rhs(), true),
                    W::local_set(&rhs),
                    W::local_get(&lhs),
                    W::local_get(&rhs),
                    W::call(func_name),
                ]
            }
            BinaryOp::Comma => {
                vec![
                    ..self.translate_expression(binary.lhs(), false),
                    ..self.translate_expression(binary.rhs(), true),
                ]
            }
        }
    }

    fn translate_identifier(&mut self, identifier: &Identifier) -> InstructionsList {
        let offset = self.add_identifier(identifier);

        if identifier.to_interned_string(&self.interner) == "undefined" {
            vec![W::ref_null_any()]
        } else {
            vec![
                W::local_get("$scope"),
                W::i32_const(offset),
                W::call("$get_variable".to_string()),
            ]
        }
    }

    fn translate_property_access(
        &mut self,
        property_access: &PropertyAccess,
        assign: Option<InstructionsList>,
    ) -> InstructionsList {
        use boa_ast::expression::access::PropertyAccessField;

        // println!("Property access: {:#?}", property_access);

        match property_access {
            PropertyAccess::Simple(simple_property_access) => {
                let mut target = self.translate_expression(simple_property_access.target(), true);
                // println!("TARGET: {target:#?}");
                match simple_property_access.field() {
                    PropertyAccessField::Const(sym) => {
                        let offset = self.add_symbol(*sym);
                        let temp = self.current_function().add_local("$temp", WasmType::Anyref);

                        if let Some(mut assign_instructions) = assign {
                            let mut result = vec![];
                            result.append(&mut assign_instructions);
                            result.push(W::local_set(&temp));
                            result.append(&mut target);
                            result.append(&mut vec![
                                W::i32_const(offset),
                                W::local_get(&temp),
                                W::call("$set_property_value"),
                            ]);
                            result
                        } else {
                            target.append(&mut vec![
                                W::local_tee(&temp),
                                W::i32_const(offset),
                                W::call("$get_property"),
                                W::local_get(&temp),
                                W::call("$get_value_of_property"),
                            ]);
                            target
                        }
                    }
                    PropertyAccessField::Expr(expression) => {
                        let target_local = self
                            .current_function()
                            .add_local("$target", WasmType::Anyref);
                        let mut instructions = vec![
                            ..self.translate_expression(expression, true),
                            W::local_tee(&target_local),
                        ];

                        if let Some(mut assign_instructions) = assign {
                            let temp = self.current_function().add_local("$temp", WasmType::Anyref);
                            vec![
                                ..target,
                                ..instructions,
                                ..assign_instructions,
                                W::call("$set_property_or_array_value"),
                            ]
                        } else {
                            target.append(&mut instructions);
                            target.push(W::call("$get_property_or_array_value"));
                            target
                        }
                    }
                }
            }
            PropertyAccess::Private(private_property_access) => {
                let mut target = self.translate_expression(private_property_access.target(), true);
                let offset = self.add_symbol(private_property_access.field().description());
                let temp = self.current_function().add_local("$temp", WasmType::Anyref);

                if let Some(mut assign_instructions) = assign {
                    let mut result = vec![];
                    result.append(&mut assign_instructions);
                    result.push(W::local_set(&temp));
                    result.append(&mut target);
                    result.append(&mut vec![
                        W::i32_const(self.private_field_offset(offset)),
                        W::local_get(&temp),
                        W::call("$set_property_value"),
                    ]);
                    result
                } else {
                    target.append(&mut vec![
                        W::local_tee(&temp),
                        W::i32_const(self.private_field_offset(offset)),
                        W::call("$get_property"),
                        W::local_get(&temp),
                        W::call("$get_value_of_property"),
                    ]);
                    target
                }
            }
            PropertyAccess::Super(_super_property_access) => todo!(),
        }
    }

    fn translate_expression(
        &mut self,
        expression: &Expression,
        will_use_return: bool,
    ) -> InstructionsList {
        // println!(
        //     "translate expression ({will_use_return}) {} {expression:#?}",
        //     expression.to_interned_string(&self.interner)
        // );
        match expression {
            Expression::This => vec![W::local_get("$this")],
            Expression::Identifier(identifier) => {
                let mut instr = self.translate_identifier(identifier);
                if !will_use_return {
                    instr.push(W::drop());
                }
                instr
            }
            Expression::Literal(literal) => {
                drop_if_no_use(self.translate_literal(literal), will_use_return)
            }
            Expression::RegExpLiteral(_reg_exp_literal) => todo!(),
            Expression::ArrayLiteral(array_literal) => {
                self.translate_array_literal(array_literal, will_use_return)
            }
            Expression::ObjectLiteral(object_literal) => {
                self.translate_object_literal(object_literal, will_use_return)
            }
            Expression::Spread(_spread) => todo!(),
            Expression::FunctionExpression(function) => self.translate_function(function),
            Expression::ArrowFunction(arrow_function) => {
                self.translate_arrow_function(arrow_function)
            }
            Expression::AsyncArrowFunction(_async_arrow_function) => todo!(),
            Expression::GeneratorExpression(_generator) => todo!(),
            Expression::AsyncFunctionExpression(f) => {
                self.translate_async_function(f.name(), f.parameters(), f.body())
            }
            Expression::AsyncGeneratorExpression(_async_generator) => todo!(),
            Expression::ClassExpression(class) => self.translate_class(
                class.name(),
                class.super_ref(),
                class.constructor(),
                class.elements(),
                will_use_return,
            ),
            Expression::TemplateLiteral(template_literal) => {
                self.translate_template_literal(template_literal)
            }
            Expression::PropertyAccess(property_access) => {
                self.translate_property_access(property_access, None)
            }
            Expression::New(new) => self.translate_new(new),
            // TODO: the default this value is a global object
            Expression::Call(call) => {
                self.translate_call(call, W::ref_null_any(), None, will_use_return)
            }
            Expression::SuperCall(_super_call) => todo!(),
            Expression::ImportCall(_import_call) => todo!(),
            Expression::Optional(_optional) => todo!(),
            Expression::TaggedTemplate(_tagged_template) => todo!(),
            Expression::NewTarget => todo!(),
            Expression::ImportMeta => todo!(),
            Expression::Assign(assign) => self.translate_assign(assign),
            Expression::Unary(unary) => self.translate_unary(unary),
            Expression::Update(update) => self.translate_update(update),
            Expression::Binary(binary) => self.translate_binary(binary),
            Expression::BinaryInPrivate(_binary_in_private) => todo!(),
            Expression::Conditional(conditional) => self.translate_conditional(conditional),
            Expression::Await(await_expr) => {
                self.translate_await_expression(await_expr, will_use_return)
            }
            Expression::Yield(_) => todo!(),
            Expression::Parenthesized(parenthesized) => self.translate_parenthesized(parenthesized),
            _ => todo!(),
        }
    }

    fn translate_class(
        &mut self,
        name: Option<Identifier>,
        super_ref: Option<&Expression>,
        constructor: Option<&FunctionExpression>,
        elements: &[ClassElement],
        will_use_return: bool,
    ) -> InstructionsList {
        let (mut constructor_instructions, constructor_function_name) = if let Some(constructor) =
            constructor
        {
            let function_instructions =
                self.translate_function_generic(name, constructor.parameters(), constructor.body());

            // At the moment `translate_function_generic` doesn't allow to easily get the last
            // function. Here we rely on an implementation detail, ie. that the last function is
            // inserted to the end of the functions vector.
            // TODO: fix it, cause it will break when how functions are persisted changes
            //
            // TODO: implement error when using constructor without new()
            // let constructor = self.module.functions.last().unwrap();
            // let instructions = vec![
            //     W::local_get("$this"),
            //     W::local_get("$scope"),
            //     W::i32_const(self.insert_data_string()),
            //     W::call,
            // ];

            (
                function_instructions,
                self.module.functions().last().unwrap().name.clone(),
            )
        } else {
            let function_instructions = self.translate_function_generic(
                name,
                &FormalParameterList::default(),
                &FunctionBody::default(),
            );

            (
                function_instructions,
                self.module
                    .functions()
                    .iter_mut()
                    .last()
                    .unwrap()
                    .name
                    .clone(),
            )
        };

        use boa_ast::function::ClassElement;

        let mut additional_constructor_instructions = vec![];

        let constructor_local = self.current_function().add_local(
            "$constructor",
            WasmType::Ref("$Function".to_string(), Nullable::True),
        );
        let prototype_local = self
            .current_function()
            .add_local("$prototype", WasmType::Anyref);

        constructor_instructions.push(W::local_set(&constructor_local));

        if let Some(name) = name {
            let offset = self.add_identifier(&name);
            // we have a name, which means it's a declaration
            constructor_instructions.append(&mut vec![
                W::global_get("$global_scope"),
                W::ref_cast(WasmType::Ref("$Scope".to_string(), Nullable::False)),
                W::i32_const(offset),
                W::local_get(&constructor_local),
                W::ref_cast(WasmType::r#ref("$Function")),
                W::i32_const(VarType::Const.to_i32()),
                W::call("$declare_variable"),
            ]);
        }

        constructor_instructions.append(&mut vec![
            W::local_get(&constructor_local),
            W::ref_cast(WasmType::r#ref("$Function")),
            W::i32_const(self.insert_data_string("prototype").0),
            W::call("$get_property_value"),
            W::local_set(&prototype_local),
        ]);

        for element in elements {
            match element {
                ClassElement::MethodDefinition(class_method_definition) => {
                    let function_instructions = self.translate_function_generic(
                        None,
                        class_method_definition.parameters(),
                        class_method_definition.body(),
                    );

                    let target_instruction = if class_method_definition.is_static() {
                        W::local_get(&constructor_local)
                    } else {
                        W::local_get(&prototype_local)
                    };

                    use boa_ast::function::ClassElementName;
                    let mut instructions = match class_method_definition.name() {
                        ClassElementName::PropertyName(property_name) => match property_name {
                            PropertyName::Literal(sym) => {
                                let offset = self.add_symbol(sym.to_owned());
                                vec![
                                    target_instruction,
                                    W::ref_cast(WasmType::r#ref("$Function")),
                                    W::i32_const(offset),
                                    ..function_instructions,
                                    W::call("$set_property_value"),
                                ]
                            }
                            PropertyName::Computed(expression) => {
                                vec![
                                    target_instruction,
                                    W::ref_cast(WasmType::r#ref("$Function")),
                                    ..self.translate_expression(expression, true),
                                    W::call("$to_string"),
                                    ..function_instructions,
                                    W::call("$set_property_value_str"),
                                ]
                            }
                        },
                        ClassElementName::PrivateName(private_name) => {
                            let offset = self.add_symbol(private_name.description());
                            vec![
                                target_instruction,
                                W::ref_cast(WasmType::r#ref("$Function")),
                                W::i32_const(self.private_field_offset(offset)),
                                ..function_instructions,
                                W::call("$set_property_value"),
                            ]
                        }
                    };

                    constructor_instructions.append(&mut instructions);
                }
                ClassElement::FieldDefinition(class_field_definition) => {
                    let assign = if let Some(expr) = class_field_definition.field() {
                        self.translate_expression(expr, true)
                    } else {
                        vec![W::ref_null_any()]
                    };

                    match class_field_definition.name() {
                        PropertyName::Literal(sym) => {
                            let offset = self.add_symbol(sym.to_owned());
                            let mut instructions = vec![
                                W::local_get("$this"),
                                W::i32_const(offset),
                                ..assign,
                                W::call("$set_property_value"),
                            ];

                            additional_constructor_instructions.append(&mut instructions);
                        }
                        PropertyName::Computed(expr) => {
                            let mut instructions = vec![
                                W::local_get("$this"),
                                ..self.translate_expression(expr, true),
                                W::call("$to_string"),
                                ..assign,
                                W::call("$set_property_value_str"),
                            ];

                            additional_constructor_instructions.append(&mut instructions);
                        }
                    }
                }
                ClassElement::PrivateFieldDefinition(private_field_definition) => {
                    let assign = if let Some(expr) = private_field_definition.field() {
                        self.translate_expression(expr, true)
                    } else {
                        vec![W::ref_null_any()]
                    };

                    let offset = self.add_symbol(private_field_definition.name().description());
                    let mut instructions = vec![
                        W::local_get("$this"),
                        W::i32_const(self.private_field_offset(offset)),
                        ..assign,
                        W::call("$set_property_value"),
                    ];

                    additional_constructor_instructions.append(&mut instructions);
                }
                ClassElement::StaticFieldDefinition(class_field_definition) => {
                    let assign = if let Some(expr) = class_field_definition.field() {
                        self.translate_expression(expr, true)
                    } else {
                        vec![W::ref_null_any()]
                    };

                    let mut instructions = match class_field_definition.name() {
                        PropertyName::Literal(sym) => {
                            let offset = self.add_symbol(sym.to_owned());
                            vec![
                                W::local_get(&constructor_local),
                                W::ref_cast(WasmType::r#ref("$Function")),
                                W::i32_const(offset),
                                ..assign,
                                W::call("$set_property_value"),
                            ]
                        }
                        PropertyName::Computed(expr) => {
                            vec![
                                W::local_get(&constructor_local),
                                W::ref_cast(WasmType::r#ref("$Function")),
                                ..self.translate_expression(expr, true),
                                W::call("$to_string"),
                                ..assign,
                                W::call("$set_property_value_str"),
                            ]
                        }
                    };

                    constructor_instructions.append(&mut instructions);
                }
                ClassElement::PrivateStaticFieldDefinition(private_name, expression) => {
                    let assign = if let Some(expr) = expression {
                        self.translate_expression(expr, true)
                    } else {
                        vec![W::ref_null_any()]
                    };

                    let offset = self.add_symbol(private_name.description());
                    let mut instructions = vec![
                        W::local_get(&constructor_local),
                        W::ref_cast(WasmType::r#ref("$Function")),
                        W::i32_const(self.private_field_offset(offset)),
                        ..assign,
                        W::call("$set_property_value"),
                    ];

                    constructor_instructions.append(&mut instructions);
                }
                ClassElement::StaticBlock(static_block_body) => todo!(),
            }
        }

        self.module
            .get_function_mut(&constructor_function_name)
            .unwrap()
            .prepend_instructions(additional_constructor_instructions);

        constructor_instructions
    }

    fn translate_template_literal(&mut self, lit: &TemplateLiteral) -> InstructionsList {
        let mut result = vec![W::call("$create_empty_string")];

        for element in lit.elements() {
            match element {
                TemplateElement::String(s) => {
                    let s = self.interner.resolve(*s).unwrap().to_string();
                    let (offset, length) = self.insert_data_string(&s);

                    result.append(&mut vec![
                        W::I32Const(offset),
                        W::I32Const(length),
                        W::call("$add_static_string_to_string"),
                    ]);
                }
                TemplateElement::Expr(expr) => {
                    result.append(&mut vec![
                        ..self.translate_expression(expr, true),
                        W::call("$to_string"),
                        W::call("$add_strings"),
                    ]);
                }
            }
        }

        result
    }

    fn translate_conditional(&mut self, conditional: &Conditional) -> InstructionsList {
        let result_local = self
            .current_function()
            .add_local("$result", WasmType::Anyref);
        let mut instructions = self.translate_expression(conditional.condition(), true);
        instructions.push(W::I31GetS);

        let mut then_instructions = self.translate_expression(conditional.if_true(), true);
        then_instructions.push(W::local_set(&result_local));
        let mut else_instructions = self.translate_expression(conditional.if_false(), true);
        else_instructions.push(W::local_set(&result_local));

        instructions.push(W::r#if(then_instructions, Some(else_instructions)));
        instructions.push(W::local_get(&result_local));

        instructions
    }

    fn translate_await_expression(
        &mut self,
        await_expression: &Await,
        will_use_return: bool,
    ) -> InstructionsList {
        // we need to set will_use_return for the awaited expression, cause we always want to use
        // the promise
        let await_instructions = self.translate_expression(await_expression.target(), true);

        let await_instr = if will_use_return {
            W::call("$__await__")
        } else {
            W::call("$__await_drop__")
        };
        vec![..await_instructions, await_instr]
    }

    fn translate_async_function(
        &mut self,
        name: Option<Identifier>,
        params: &FormalParameterList,
        body: &FunctionBody,
    ) -> InstructionsList {
        let result = self.translate_function_generic(name, params, body);
        let added_function_name = self.module.functions().last().unwrap().name.clone();
        self.async_functions.push(added_function_name);
        result
    }

    fn translate_array_literal(
        &mut self,
        array_literal: &ArrayLiteral,
        will_use_return: bool,
    ) -> InstructionsList {
        // println!("array literal: {:#?}", array_literal);
        let var = self
            .current_function()
            .add_local("$array_elem", WasmType::Anyref);
        let array_var = self.current_function().add_local(
            "$array_var",
            WasmType::Ref("$Array".into(), Nullable::False),
        );
        let array_data = self.current_function().add_local(
            "$array_data",
            WasmType::Ref("$AnyrefArray".into(), Nullable::False),
        );
        let array = array_literal.as_ref();
        let mut instructions = vec![
            W::i32_const(array.len() as i32),
            W::call("$create_array"),
            W::local_set(&array_var),
            W::local_get(&array_var),
            W::struct_get("$Array", "$array"),
            W::local_set(&array_data),
        ];

        for (i, item) in array.iter().enumerate() {
            let mut value = if let Some(expression) = item {
                self.translate_expression(expression, true)
            } else {
                vec![W::ref_null_any()]
            };

            instructions.append(&mut value);
            instructions.push(W::local_set(&var));
            instructions.append(&mut vec![
                W::local_get(&array_data),
                W::i32_const(i as i32),
                W::local_get(&var),
                W::array_set("$AnyrefArray"),
            ])
        }

        if will_use_return {
            instructions.push(W::local_get(&array_var));
        }

        instructions
    }

    fn translate_parenthesized(&mut self, parenthesized: &Parenthesized) -> InstructionsList {
        // println!("parenthesized: {parenthesized:#?}");

        self.translate_expression(parenthesized.expression(), true)
    }

    fn translate_object_literal(
        &mut self,
        object_literal: &ObjectLiteral,
        will_use_return: bool,
    ) -> InstructionsList {
        let mut instructions = Vec::new();
        let new_instance = self.current_function().add_local(
            "$new_instance",
            WasmType::Ref("$Object".into(), Nullable::False),
        );
        let temp = self.current_function().add_local("$temp", WasmType::Anyref);

        instructions.push(W::call("$create_object"));
        instructions.push(W::local_set(&new_instance));

        for property in object_literal.properties() {
            let mut instr = match property {
                PropertyDefinition::IdentifierReference(identifier) => {
                    let offset = self.add_identifier(identifier);
                    vec![
                        ..self.translate_identifier(identifier),
                        W::local_set(&temp),
                        W::local_get(&new_instance),
                        W::i32_const(offset),
                        W::local_get(&temp),
                        W::call("$set_property_value"),
                    ]
                }
                PropertyDefinition::Property(property_name, expression) => match property_name {
                    PropertyName::Literal(sym) => {
                        let offset = self.add_symbol(*sym);
                        let mut result = self.translate_expression(expression, true);
                        result.append(&mut vec![
                            W::local_set(&temp),
                            W::local_get(&new_instance),
                            W::i32_const(offset),
                            W::local_get(&temp),
                            W::call("$set_property_value"),
                        ]);
                        result
                    }
                    PropertyName::Computed(computed) => {
                        vec![
                            W::local_get(&new_instance),
                            ..self.translate_expression(computed, true),
                            W::call("$to_string"),
                            ..self.translate_expression(expression, true),
                            W::call("$set_property_value_str"),
                        ]
                    }
                },
                PropertyDefinition::MethodDefinition(method_definition) => {
                    match method_definition.kind() {
                        boa_ast::property::MethodDefinitionKind::Get => {
                            match method_definition.name() {
                                PropertyName::Literal(sym) => {
                                    let offset = self.add_symbol(sym.to_owned());
                                    vec![
                                        W::local_get(&new_instance),
                                        W::i32_const(offset),
                                        ..self.translate_get_function(
                                            Some(Identifier::new(sym.to_owned())),
                                            method_definition.parameters(),
                                            method_definition.body(),
                                        ),
                                        W::local_get(&new_instance),
                                        W::i32_const(offset),
                                        W::call("$create_get_property"),
                                        W::call("$set_property"),
                                    ]
                                }
                                PropertyName::Computed(expr) => {
                                    let prop_local = self.current_function().add_local(
                                        "$property_name",
                                        WasmType::Ref("$String".to_string(), Nullable::False),
                                    );
                                    vec![
                                        W::local_get(&new_instance),
                                        ..self.translate_expression(expr, true),
                                        W::call("$to_string"),
                                        W::local_tee(&prop_local),
                                        ..self.translate_get_function(
                                            None,
                                            method_definition.parameters(),
                                            method_definition.body(),
                                        ),
                                        W::local_get(&new_instance),
                                        W::local_get(&prop_local),
                                        W::call("$create_get_property_str"),
                                        W::call("$set_property_str"),
                                    ]
                                }
                            }
                        }
                        boa_ast::property::MethodDefinitionKind::Set => {
                            match method_definition.name() {
                                PropertyName::Literal(sym) => {
                                    let offset = self.add_symbol(sym.to_owned());
                                    vec![
                                        W::local_get(&new_instance),
                                        W::i32_const(offset),
                                        ..self.translate_set_function(
                                            Some(Identifier::new(sym.to_owned())),
                                            method_definition.parameters(),
                                            method_definition.body(),
                                        ),
                                        W::local_get(&new_instance),
                                        W::i32_const(offset),
                                        W::call("$create_set_property"),
                                        W::call("$set_property"),
                                    ]
                                }
                                PropertyName::Computed(expr) => {
                                    let prop_local = self.current_function().add_local(
                                        "$property_name",
                                        WasmType::Ref("$String".to_string(), Nullable::False),
                                    );
                                    vec![
                                        W::local_get(&new_instance),
                                        ..self.translate_expression(expr, true),
                                        W::call("$to_string"),
                                        W::local_tee(&prop_local),
                                        ..self.translate_set_function(
                                            None,
                                            method_definition.parameters(),
                                            method_definition.body(),
                                        ),
                                        W::local_get(&new_instance),
                                        W::local_get(&prop_local),
                                        W::call("$create_set_property_str"),
                                        W::call("$set_property_str"),
                                    ]
                                }
                            }
                        }
                        boa_ast::property::MethodDefinitionKind::Ordinary => {
                            match method_definition.name() {
                                PropertyName::Literal(sym) => {
                                    let offset = self.add_symbol(sym.to_owned());
                                    vec![
                                        W::local_get(&new_instance),
                                        W::i32_const(offset),
                                        ..self.translate_function_generic(
                                            Some(Identifier::new(sym.to_owned())),
                                            method_definition.parameters(),
                                            method_definition.body(),
                                        ),
                                        W::call("$set_property_value"),
                                    ]
                                }
                                PropertyName::Computed(expr) => {
                                    vec![
                                        W::local_get(&new_instance),
                                        ..self.translate_expression(expr, true),
                                        W::call("$to_string"),
                                        ..self.translate_function_generic(
                                            None,
                                            method_definition.parameters(),
                                            method_definition.body(),
                                        ),
                                        W::call("$set_property_value_str"),
                                    ]
                                }
                            }
                        }
                        boa_ast::property::MethodDefinitionKind::Generator => todo!(),
                        boa_ast::property::MethodDefinitionKind::AsyncGenerator => {
                            todo!()
                        }
                        boa_ast::property::MethodDefinitionKind::Async => todo!(),
                    }
                }
                PropertyDefinition::SpreadObject(_) => todo!(),
                PropertyDefinition::CoverInitializedName(_, _) => todo!(),
            };
            instructions.append(&mut instr);
        }

        if will_use_return {
            instructions.push(W::local_get(&new_instance));
        }

        instructions
    }

    fn translate_new(&mut self, new: &New) -> InstructionsList {
        let new_instance = self.current_function().add_local(
            "$new_instance",
            WasmType::Ref("$Object".into(), Nullable::False),
        );
        let prototype_local = self
            .current_function()
            .add_local("$prototype", WasmType::Anyref);
        let constructor = self.current_function().add_local(
            "$constructor",
            WasmType::Ref("$Function".to_string(), Nullable::True),
        );

        let prototype_instructions = vec![
            W::ref_cast(WasmType::Ref("$Function".to_string(), Nullable::False)),
            W::local_tee(&constructor),
            W::ref_cast(WasmType::r#ref("$Function")),
            W::I32Const(self.insert_data_string("prototype").0),
            W::call("$get_property"),
            W::local_get(&constructor),
            W::ref_cast(WasmType::r#ref("$Function")),
            W::call("$get_value_of_property"),
            W::local_set(&prototype_local),
        ];

        vec![
            W::call("$create_object"),
            W::local_set(&new_instance),
            ..self.translate_call(
                new.call(),
                W::local_get(&new_instance),
                Some(prototype_instructions),
                true,
            ),
            W::local_get(&new_instance),
            W::local_get(&prototype_local),
            W::local_get(&constructor),
            W::ref_cast(WasmType::r#ref("$Function")),
            W::call("$return_new_instance_result"),
            // W::local_set(&prototype_local),
            // W::local_get(&new_instance),
            // W::local_get(&prototype_local),
            // W::struct_set("$Object", "$prototype"),
            //
        ]
    }

    fn translate_arrow_function(&mut self, function: &ArrowFunction) -> InstructionsList {
        let bind_instructions = if self
            .current_function()
            .params
            .iter()
            .any(|(maybe_name, _)| maybe_name.as_ref().map(|n| n == "$this").unwrap_or(false))
        {
            vec![W::local_get("$this"), W::call("$bind_this")]
        } else {
            vec![W::global_get("$global_this"), W::call("$bind_this")]
        };
        vec![
            ..self.translate_function_generic(
                function.name(),
                function.parameters(),
                function.body(),
            ),
            ..bind_instructions,
        ]
    }

    fn translate_update(&mut self, update: &Update) -> InstructionsList {
        use boa_ast::expression::operator::update::UpdateOp;

        let var = self.current_function().add_local("$var", WasmType::Anyref);

        let fetch_instructions = match update.target() {
            UpdateTarget::Identifier(identifier) => self.translate_identifier(identifier),
            UpdateTarget::PropertyAccess(property_access) => {
                self.translate_property_access(property_access, None)
            }
        };

        let save_instructions = match update.target() {
            UpdateTarget::Identifier(identifier) => {
                let offset = self.add_identifier(identifier);
                vec![
                    W::local_get("$scope".to_string()),
                    W::i32_const(offset),
                    W::local_get(&var),
                    W::call("$assign_variable"),
                ]
            }
            UpdateTarget::PropertyAccess(property_access) => {
                let assign = vec![W::local_get(&var)];
                self.translate_property_access(property_access, Some(assign))
            }
        };

        // TODO: figure out pre vs post behaviour
        let instruction = match update.op() {
            UpdateOp::IncrementPost => W::call("$increment_number"),
            UpdateOp::IncrementPre => W::call("$increment_number"),
            UpdateOp::DecrementPost => W::call("$decrement_number"),
            UpdateOp::DecrementPre => W::call("$decrement_number"),
        };

        vec![
            ..fetch_instructions,
            instruction,
            W::local_set(&var),
            ..save_instructions,
        ]
    }

    fn translate_assign(&mut self, assign: &Assign) -> InstructionsList {
        use boa_ast::expression::operator::assign::AssignOp;
        use boa_ast::expression::operator::assign::AssignTarget;

        if let AssignOp::Assign = assign.op() {
            let mut rhs = self.translate_expression(assign.rhs(), true);
            return match assign.lhs() {
                AssignTarget::Identifier(identifier) => {
                    let offset = self.add_identifier(identifier);
                    // identifier.sym().get(),
                    let rhs_var = self.current_function().add_local("$rhs", WasmType::Anyref);
                    rhs.append(&mut vec![
                        W::local_set(&rhs_var),
                        W::local_get("$scope".to_string()),
                        W::i32_const(offset),
                        W::local_get(&rhs_var),
                        W::call("$assign_variable".to_string()),
                    ]);
                    rhs
                }
                AssignTarget::Access(property_access) => {
                    self.translate_property_access(property_access, Some(rhs))
                }
                AssignTarget::Pattern(pattern) => {
                    self.translate_pattern(rhs, vec![W::ref_null_any()], pattern, None)
                }
            };
        }

        let (transform_instructions, op_instruction) = match assign.op() {
            AssignOp::Add
            | AssignOp::Sub
            | AssignOp::Mul
            | AssignOp::Div
            | AssignOp::Mod
            | AssignOp::Exp
            | AssignOp::And
            | AssignOp::Or => {
                let func_name = match assign.op() {
                    AssignOp::Add => "$add",
                    AssignOp::Sub => "$sub",
                    AssignOp::Mul => "$mul",
                    AssignOp::Div => "$div",
                    AssignOp::Mod => "$mod_op",
                    AssignOp::Exp => todo!(),
                    AssignOp::And => "$logical_and",
                    AssignOp::Or => "$logical_or",
                    _ => unreachable!(),
                };

                (vec![], W::call(func_name))
            }
            AssignOp::Xor
            | AssignOp::Shl
            | AssignOp::Shr
            | AssignOp::Ushr
            | AssignOp::BoolAnd
            | AssignOp::BoolOr => {
                let operation = match assign.op() {
                    AssignOp::BoolAnd => W::I32And,
                    AssignOp::BoolOr => W::I32Or,
                    AssignOp::Xor => W::I32Xor,
                    AssignOp::Shl => W::I32Shl,
                    AssignOp::Shr => W::I32ShrS,
                    AssignOp::Ushr => W::I32ShrU,
                    _ => unreachable!(),
                };
                // TODO: convert args to primitive
                let instructions = vec![
                    W::ref_cast(WasmType::Ref("$Number".to_string(), Nullable::False)),
                    W::struct_get("$Number", "$value"),
                    W::I32TruncF64S,
                ];

                (instructions, operation)
            }
            AssignOp::Coalesce => todo!(),
            AssignOp::Assign => unreachable!(),
        };

        let rhs = self.translate_expression(assign.rhs(), true);
        let rhs_var = self.current_function().add_local("$rhs", WasmType::Anyref);

        match assign.lhs() {
            AssignTarget::Identifier(identifier) => {
                let offset = self.add_identifier(identifier);
                // identifier.sym().get(),
                vec![
                    ..rhs,
                    W::local_set(&rhs_var),
                    W::local_get("$scope"),
                    W::i32_const(offset),
                    W::call("$get_variable"),
                    ..transform_instructions.clone(),
                    W::local_get(&rhs_var),
                    ..transform_instructions,
                    op_instruction,
                    W::local_set(&rhs_var),
                    W::local_get("$scope".to_string()),
                    W::i32_const(offset),
                    W::local_get(&rhs_var),
                    W::call("$assign_variable"),
                ]
            }
            AssignTarget::Access(property_access) => {
                vec![
                    ..rhs,
                    W::local_set(&rhs_var),
                    ..self.translate_property_access(property_access, None),
                    ..transform_instructions.clone(),
                    W::local_get(&rhs_var),
                    ..transform_instructions,
                    op_instruction,
                    W::local_set(&rhs_var),
                    ..self.translate_property_access(
                        property_access,
                        Some(vec![W::local_get(&rhs_var)]),
                    ),
                ]
            }
            // assignment pattern is only possible with simple assignment, not with assignment
            // operator
            AssignTarget::Pattern(_pattern) => unreachable!(),
        }
    }

    fn translate_unary(&mut self, unary: &Unary) -> InstructionsList {
        use boa_ast::expression::operator::unary::UnaryOp;

        if let UnaryOp::TypeOf = unary.op() {
            let result_local = self
                .current_function()
                .add_local("$result", WasmType::Anyref);
            let (offset, length) = self.insert_data_string("undefined");
            let mut target = self.translate_expression(unary.target(), true);
            if let Some(W::Call(name)) = target.last().clone() {
                if name == "$get_variable" {
                    target.pop();
                    target.push(W::call("$get_variable_for_typeof"));
                }
            }
            target.push(W::call("$type_of"));
            target
        } else if let UnaryOp::Delete = unary.op() {
            let mut instructions = self.translate_expression(unary.target(), true);
            match instructions.last() {
                Some(W::Call(name)) if name == "$get_value_of_property" => {
                    // I don't particularly like this, as this will fail as soon as we change the
                    // code generating these instructions to emit something different, but it
                    // should work for now
                    instructions.pop();
                    instructions.pop();
                    match instructions.last() {
                        Some(W::Call(name)) if name == "$get_property_str" => {
                            instructions.pop();
                            instructions.push(W::call("$delete_property_str"));
                        }
                        Some(W::Call(name)) if name == "$get_property" => {
                            instructions.pop();
                            instructions.push(W::call("$delete_property"));
                        }
                        _ => {
                            panic!("Couldn't find a property instruction for delete property. This is a bug in JAWSM, please report it")
                        }
                    }
                }
                _ => {
                    // JavaScript let's you use any expression with delete, but if
                    // it's not property access it just does nothing ¯\_(ツ)_/¯
                    // so we evaluate, but drop the value
                    instructions.push(W::Drop);
                }
            }
            instructions
        } else {
            let mut instructions = self.translate_expression(unary.target(), true);
            match unary.op() {
                UnaryOp::Minus => instructions.push(W::call("$op_minus")),
                UnaryOp::Plus => {}
                UnaryOp::Not => instructions.push(W::call("$logical_not")),
                UnaryOp::Tilde => todo!(),
                UnaryOp::TypeOf => unreachable!(),
                UnaryOp::Delete => unreachable!(),
                UnaryOp::Void => todo!(),
            }

            instructions
        }
    }

    fn translate_literal(&mut self, lit: &Literal) -> InstructionsList {
        // println!("translate_literal: {lit:#?}");
        match lit {
            Literal::Num(num) => vec![W::f64_const(*num), W::call("$new_number")],
            Literal::String(s) => {
                let s = self.interner.resolve(*s).unwrap().to_string();
                let (offset, length) = self.insert_data_string(&s);

                vec![
                    W::i32_const(offset),
                    W::i32_const(length),
                    W::call("$new_static_string"),
                ]
            }
            Literal::Int(i) => vec![W::f64_const(*i as f64), W::call("$new_number")],
            Literal::BigInt(_big_int) => todo!(),
            Literal::Bool(b) => vec![
                W::i32_const(if *b { 1 } else { 0 }),
                W::call("$new_boolean"),
            ],
            Literal::Null => vec![W::i32_const(2), W::ref_i31()],
            Literal::Undefined => vec![W::ref_null_any()],
        }
    }

    fn translate_declaration(&mut self, declaration: &Declaration) -> InstructionsList {
        // println!(
        //     "translate_declaration {}",
        //     declaration.to_interned_string(&self.interner)
        // );
        match declaration {
            Declaration::FunctionDeclaration(decl) => {
                let mut declaration = self.translate_function_generic(
                    Some(decl.name()),
                    decl.parameters(),
                    decl.body(),
                );
                // function declaration still needs to be added to the scope if function has a name
                // TODO: declared functions need to be hoisted
                let offset = self.add_identifier(&decl.name());
                let mut result = vec![W::local_get("$scope"), W::i32_const(offset)];
                result.append(&mut declaration);
                result.append(&mut vec![
                    W::i32_const(VarType::Function.to_i32()),
                    W::call("$declare_variable".to_string()),
                ]);
                result
            }
            Declaration::Lexical(v) => self.translate_lexical(v),
            Declaration::GeneratorDeclaration(_generator) => todo!(),
            Declaration::AsyncFunctionDeclaration(decl) => {
                let mut declaration = self.translate_async_function(
                    Some(decl.name()),
                    decl.parameters(),
                    decl.body(),
                );
                let offset = self.add_identifier(&decl.name());
                let mut result = vec![W::local_get("$scope"), W::i32_const(offset)];
                result.append(&mut declaration);
                result.append(&mut vec![
                    W::i32_const(VarType::Var.to_i32()),
                    W::call("$declare_variable".to_string()),
                ]);
                result
            }
            Declaration::AsyncGeneratorDeclaration(_async_generator) => todo!(),
            Declaration::ClassDeclaration(class) => self.translate_class(
                Some(class.name()),
                class.super_ref(),
                class.constructor(),
                class.elements(),
                false,
            ),
        }
    }

    fn additional_functions(&self) -> String {
        "".into()
    }

    pub fn insert_data_string(&mut self, s: &str) -> (i32, i32) {
        let (offset, length) = self.module.add_data(s.to_string());
        (offset as i32, length as i32)
    }

    fn translate_for_of_loop(&mut self, for_of_loop: &ForOfLoop) -> InstructionsList {
        self.enter_block();

        let current_loop_break_name = self.current_loop_break_name();
        let current_continue_block_name = self.current_continue_block_name();
        let current_loop_name = self.current_loop_name();

        use boa_ast::declaration::Binding;
        use boa_ast::statement::iteration::IterableLoopInitializer;
        let target = self.translate_expression(for_of_loop.iterable(), true);

        let current = self
            .current_function()
            .add_local("$current", WasmType::Anyref);
        let iterator = self
            .current_function()
            .add_local("$iterator", WasmType::Anyref);
        let iterator_result = self
            .current_function()
            .add_local("$iterator-result", WasmType::Anyref);

        let initializer = match for_of_loop.initializer() {
            IterableLoopInitializer::Identifier(identifier) => {
                vec![
                    W::local_get("$scope"),
                    W::I32Const(identifier.sym().get() as i32),
                    W::local_get(&current),
                    W::call("$assign_variable"),
                ]
            }
            IterableLoopInitializer::Access(property_access) => {
                self.translate_property_access(property_access, Some(vec![W::local_get(&current)]))
            }
            IterableLoopInitializer::Var(var) => match var.binding() {
                Binding::Identifier(identifier) => {
                    let s = self.interner.resolve(identifier.sym()).unwrap().to_string();
                    let (offset, _) = self.insert_data_string(&s);
                    vec![
                        W::local_get("$scope"),
                        W::I32Const(offset),
                        W::local_get(&current),
                        W::i32_const(VarType::Var.to_i32()),
                        W::call("$declare_variable"),
                    ]
                }
                Binding::Pattern(pattern) => self.translate_pattern(
                    vec![W::local_get(&current)],
                    vec![W::ref_null_any()],
                    pattern,
                    Some(VarType::Var),
                ),
            },
            IterableLoopInitializer::Let(binding) => match binding {
                Binding::Identifier(identifier) => {
                    let s = self.interner.resolve(identifier.sym()).unwrap().to_string();
                    let (offset, _) = self.insert_data_string(&s);
                    vec![
                        W::local_get("$scope"),
                        W::I32Const(offset),
                        W::local_get(&current),
                        W::i32_const(VarType::Let.to_i32()),
                        W::call("$declare_variable"),
                    ]
                }
                Binding::Pattern(pattern) => self.translate_pattern(
                    vec![W::local_get(&current)],
                    vec![W::ref_null_any()],
                    pattern,
                    Some(VarType::Let),
                ),
            },
            IterableLoopInitializer::Const(binding) => match binding {
                Binding::Identifier(identifier) => {
                    let s = self.interner.resolve(identifier.sym()).unwrap().to_string();
                    let (offset, _) = self.insert_data_string(&s);
                    vec![
                        W::local_get("$scope"),
                        W::I32Const(offset),
                        W::local_get(&current),
                        W::i32_const(VarType::Const.to_i32()),
                        W::call("$declare_variable"),
                    ]
                }
                Binding::Pattern(pattern) => self.translate_pattern(
                    vec![W::local_get(&current)],
                    vec![W::ref_null_any()],
                    pattern,
                    Some(VarType::Const),
                ),
            },
            IterableLoopInitializer::Pattern(pattern) => self.translate_pattern(
                vec![W::local_get(&current)],
                vec![W::ref_null_any()],
                pattern,
                None,
            ),
        };

        let block_instructions = vec![
            // set up new scope
            W::local_get("$scope"),
            W::call("$new_scope"),
            W::local_set("$scope"),
            // set up the current element
            W::local_get(&iterator),
            W::call("$get_iterator_next"),
            W::local_tee(&iterator_result),
            W::call("$get_iterator_result_value"),
            W::local_set(&current),
            // execute variable initializer
            ..initializer,
            // when using continue, we can't skip all the initialization and
            // scope manipulation parts
            W::block(
                &current_continue_block_name,
                Signature::default(),
                self.translate_statement(for_of_loop.body()),
            ),
            // check if we're done
            W::local_get(&iterator_result),
            W::call("$is_iterator_done"),
            W::br_if(&current_loop_break_name),
            // scope cleanup
            W::local_get("$scope"),
            W::call("$extract_parent_scope"),
            W::local_set("$scope"),
            W::br(&current_loop_name),
        ];

        let result = vec![
            ..target,
            W::call("$get_iterator"),
            W::local_set(&iterator),
            W::r#loop(
                &current_loop_name,
                vec![W::block(
                    &current_loop_break_name,
                    Signature::default(),
                    block_instructions,
                )],
            ),
        ];

        self.exit_block();

        result
    }
    fn translate_for_in_loop(&mut self, for_in_loop: &ForInLoop) -> InstructionsList {
        // TODO: current for..in implementation ignores array indexes at the moment
        self.enter_block();

        let current_loop_break_name = self.current_loop_break_name();
        let current_continue_block_name = self.current_continue_block_name();
        let current_loop_name = self.current_loop_name();

        use boa_ast::declaration::Binding;
        use boa_ast::statement::iteration::IterableLoopInitializer;
        let target = self.translate_expression(for_in_loop.target(), true);

        let current = self
            .current_function()
            .add_local("$current", WasmType::Anyref);
        let i = self.current_function().add_local("$i", WasmType::I32);
        let length = self.current_function().add_local("$length", WasmType::I32);
        let properties = self.current_function().add_local(
            "$properties",
            WasmType::Ref("$StringArray".to_string(), Nullable::False),
        );

        let initializer = match for_in_loop.initializer() {
            IterableLoopInitializer::Identifier(identifier) => {
                vec![
                    W::local_get("$scope"),
                    W::I32Const(identifier.sym().get() as i32),
                    W::local_get(&current),
                    W::call("$assign_variable"),
                ]
            }
            IterableLoopInitializer::Access(property_access) => {
                self.translate_property_access(property_access, Some(vec![W::local_get(&current)]))
            }
            IterableLoopInitializer::Var(var) => match var.binding() {
                Binding::Identifier(identifier) => {
                    let s = self.interner.resolve(identifier.sym()).unwrap().to_string();
                    let (offset, _) = self.insert_data_string(&s);
                    vec![
                        W::local_get("$scope"),
                        W::I32Const(offset),
                        W::local_get(&current),
                        W::i32_const(VarType::Var.to_i32()),
                        W::call("$declare_variable"),
                    ]
                }
                Binding::Pattern(pattern) => self.translate_pattern(
                    vec![W::local_get(&current)],
                    vec![W::ref_null_any()],
                    pattern,
                    Some(VarType::Var),
                ),
            },
            IterableLoopInitializer::Let(binding) => match binding {
                Binding::Identifier(identifier) => {
                    let s = self.interner.resolve(identifier.sym()).unwrap().to_string();
                    let (offset, _) = self.insert_data_string(&s);
                    vec![
                        W::local_get("$scope"),
                        W::I32Const(offset),
                        W::local_get(&current),
                        W::i32_const(VarType::Let.to_i32()),
                        W::call("$declare_variable"),
                    ]
                }
                Binding::Pattern(pattern) => self.translate_pattern(
                    vec![W::local_get(&current)],
                    vec![W::ref_null_any()],
                    pattern,
                    Some(VarType::Let),
                ),
            },
            IterableLoopInitializer::Const(binding) => match binding {
                Binding::Identifier(identifier) => {
                    let s = self.interner.resolve(identifier.sym()).unwrap().to_string();
                    let (offset, _) = self.insert_data_string(&s);
                    vec![
                        W::local_get("$scope"),
                        W::I32Const(offset),
                        W::local_get(&current),
                        W::i32_const(VarType::Const.to_i32()),
                        W::call("$declare_variable"),
                    ]
                }
                Binding::Pattern(pattern) => self.translate_pattern(
                    vec![W::local_get(&current)],
                    vec![W::ref_null_any()],
                    pattern,
                    Some(VarType::Const),
                ),
            },
            IterableLoopInitializer::Pattern(pattern) => self.translate_pattern(
                vec![W::local_get(&current)],
                vec![W::ref_null_any()],
                pattern,
                None,
            ),
        };

        let block_instructions = vec![
            // set up new scope
            W::local_get("$scope"),
            W::call("$new_scope"),
            W::local_set("$scope"),
            // check if we're done
            W::local_get(&i),
            W::local_get(&length),
            W::I32GeS,
            W::br_if(&current_loop_break_name),
            // set up the current element
            W::local_get(&properties),
            W::local_get(&i),
            W::array_get("$StringArray"),
            W::local_set(&current),
            // execute variable initializer
            ..initializer,
            // when using continue, we can't skip all the initialization and
            // scope manipulation parts
            W::block(
                &current_continue_block_name,
                Signature::default(),
                self.translate_statement(for_in_loop.body()),
            ),
            W::local_get(&i),
            W::I32Const(1),
            W::I32Add,
            W::local_set(&i),
            // scope cleanup
            W::local_get("$scope"),
            W::call("$extract_parent_scope"),
            W::local_set("$scope"),
            W::br(&current_loop_name),
        ];

        let result = vec![
            ..target,
            W::call("$get_enumerable_property_names"),
            W::local_tee(&properties),
            W::ArrayLen,
            W::local_set(&length),
            W::I32Const(0),
            W::local_set(&i),
            W::r#loop(
                &current_loop_name,
                vec![W::block(
                    &current_loop_break_name,
                    Signature::default(),
                    block_instructions,
                )],
            ),
        ];

        self.exit_block();

        result
    }

    fn translate_for_loop(&mut self, for_loop: &ForLoop) -> InstructionsList {
        self.enter_block();

        let current_loop_break_name = self.current_loop_break_name();
        let current_continue_block_name = self.current_continue_block_name();
        let current_loop_name = self.current_loop_name();

        use boa_ast::statement::iteration::ForLoopInitializer;
        let initializer_expr: InstructionsList = match for_loop.init() {
            Some(init) => match init {
                ForLoopInitializer::Expression(expr) => self.translate_expression(expr, true),
                ForLoopInitializer::Var(decl) => self.translate_var(decl),
                ForLoopInitializer::Lexical(decl) => self.translate_lexical(decl.declaration()),
            },
            None => vec![],
        };

        let initializer = vec![
            W::local_get("$scope"),
            W::call("$new_scope"),
            W::local_set("$scope"),
            ..initializer_expr,
        ];

        let condition: InstructionsList = match for_loop.condition() {
            Some(expr) => self.translate_expression(expr, true),
            None => vec![],
        };
        let final_instr: InstructionsList = match for_loop.final_expr() {
            Some(expr) => self.translate_expression(expr, true),
            None => vec![],
        };

        let scope_cleanup: InstructionsList = vec![
            W::local_get("$scope"),
            W::call("$extract_parent_scope"),
            W::local_set("$scope"),
        ];

        let block_instructions = vec![
            ..condition,
            W::call("$cast_ref_to_i32_bool"),
            W::i32_eqz(),
            W::br_if(&current_loop_break_name),
            W::block(
                &current_continue_block_name,
                Signature::default(),
                self.translate_statement(for_loop.body()),
            ),
            ..final_instr,
            // TODO: we need unique naming to support loop in a loop. the same problem exists
            //       for a while loop
            W::br(&current_loop_name),
        ];

        let result = vec![
            ..initializer,
            W::r#loop(
                &current_loop_name,
                vec![W::block(
                    &current_loop_break_name,
                    Signature::default(),
                    block_instructions,
                )],
            ),
            ..scope_cleanup,
        ];

        self.exit_block();

        result
    }

    fn translate_statement(&mut self, statement: &Statement) -> InstructionsList {
        match statement {
            Statement::Block(block) => self.translate_block(block),
            Statement::Var(var_declaration) => self.translate_var(var_declaration),
            Statement::Empty => vec![],
            Statement::Expression(expression) => self.translate_expression(expression, false),
            Statement::If(if_statement) => self.translate_if_statement(if_statement),
            Statement::DoWhileLoop(do_while_loop) => self.translate_do_while_loop(do_while_loop),
            Statement::WhileLoop(while_loop) => self.translate_while_loop(while_loop),
            Statement::ForLoop(for_loop) => self.translate_for_loop(for_loop),
            Statement::ForInLoop(for_in_loop) => self.translate_for_in_loop(for_in_loop),
            Statement::ForOfLoop(for_of_loop) => self.translate_for_of_loop(for_of_loop),
            Statement::Switch(switch) => self.translate_switch_statement(switch),
            Statement::Continue(_) => vec![W::br(self.current_continue_block_name())],
            Statement::Break(_) => vec![W::br(self.current_loop_break_name())],
            Statement::Return(ret) => self.translate_return(ret),
            Statement::Labelled(_labelled) => todo!(),
            Statement::Throw(throw) => self.translate_throw(throw),
            Statement::Try(r#try) => self.translate_try(r#try),
            Statement::With(_with) => todo!(),
        }
    }

    fn translate_catch(
        &mut self,
        catch: Option<&Catch>,
        finally: Option<&Finally>,
    ) -> InstructionsList {
        use boa_ast::declaration::Binding;
        let mut catch_instr = if let Some(catch) = catch {
            let mut binding_instr = if let Some(binding) = catch.parameter() {
                let temp = self.current_function().add_local("$temp", WasmType::Anyref);
                match binding {
                    Binding::Identifier(identifier) => {
                        let offset = self.add_identifier(identifier);
                        vec![
                            W::local_set(&temp),
                            W::local_get("$scope"),
                            W::i32_const(offset),
                            W::local_get(&temp),
                            W::i32_const(VarType::Param.to_i32()),
                            W::call("$declare_variable"),
                        ]
                    }
                    Binding::Pattern(pattern) => vec![
                        W::local_set(&temp),
                        ..self.translate_pattern(
                            vec![W::local_get(&temp)],
                            vec![W::ref_null_any()],
                            pattern,
                            Some(VarType::Param),
                        ),
                    ],
                }
            } else {
                vec![W::drop()]
            };
            binding_instr.append(&mut self.translate_block(catch.block()));
            binding_instr
        } else {
            vec![]
        };
        let mut finally_instr = if let Some(finally) = finally {
            self.translate_block(finally.block())
        } else {
            vec![]
        };
        // TODO: if catch throws an error this will not behave as it should.
        // we need to add another try inside, catch anything that happens
        // there, run finally and then rethrow
        let mut result = vec![];
        result.append(&mut catch_instr);
        result.append(&mut finally_instr);
        result
    }

    fn translate_try(&mut self, r#try: &Try) -> InstructionsList {
        let block = r#try.block();
        let catch = r#try.catch();
        let finally = r#try.finally();
        let instr = self.translate_catch(catch, finally);

        vec![W::r#try(
            self.translate_block(block),
            vec![("$JSException".to_string(), instr)],
            None,
        )]
    }

    fn translate_throw(&mut self, throw: &Throw) -> InstructionsList {
        let mut instructions = self.translate_expression(throw.target(), true);
        instructions.push(W::throw("$JSException"));
        instructions
    }

    fn translate_if_statement(&mut self, if_statement: &If) -> InstructionsList {
        let mut result = vec![];
        result.append(&mut self.translate_expression(if_statement.cond(), true));
        result.append(&mut vec![
            W::call("$cast_ref_to_i32_bool"),
            W::r#if(
                self.translate_statement(if_statement.body()),
                if_statement
                    .else_node()
                    .map(|e| self.translate_statement(e)),
            ),
        ]);
        result
    }

    fn translate_switch_statement(&mut self, switch: &Switch) -> InstructionsList {
        self.enter_block();

        let value_local = self
            .current_function()
            .add_local("$value", WasmType::Anyref);
        let fall_through_local = self
            .current_function()
            .add_local("$fall_through", WasmType::I32);
        let matches_local = self.current_function().add_local("$matches", WasmType::I32);
        let mut instructions = self.translate_expression(switch.val(), true);

        instructions.push(W::local_set(&value_local));
        instructions.push(W::I32Const(0));
        instructions.push(W::local_set(&fall_through_local));
        instructions.push(W::I32Const(0));
        instructions.push(W::local_set(&matches_local));

        let mut cases_instructions = vec![];

        for case in switch.cases() {
            let mut if_instructions = vec![W::I32Const(1), W::local_set(&fall_through_local)];
            for statement in case.body().statements() {
                if_instructions.append(&mut self.translate_statement_list_item(statement));
            }

            // if we haven't fallen through, check the condition
            let mut condition_instructions = vec![];
            condition_instructions.push(W::local_get(&value_local));
            if let Some(expr) = case.condition() {
                condition_instructions.append(&mut self.translate_expression(expr, true));
            } else {
                condition_instructions.push(W::I32Const(1));
                condition_instructions.push(W::ref_i31());
            }
            condition_instructions.push(W::call("$strict_equal"));
            condition_instructions.push(W::call("$cast_ref_to_i32_bool"));
            // if the conditions match, set fall through to 1
            condition_instructions.push(W::r#if(
                vec![W::I32Const(1), W::local_set(&fall_through_local)],
                None,
            ));

            cases_instructions.push(W::local_get(&fall_through_local));
            cases_instructions.push(W::I32Eqz);
            cases_instructions.push(W::r#if(condition_instructions, None));

            cases_instructions.push(W::local_get(&fall_through_local));
            cases_instructions.push(W::r#if(if_instructions, None))
        }

        if let Some(default_statements) = switch.default() {
            for statement in default_statements.statements() {
                cases_instructions.append(&mut self.translate_statement_list_item(statement));
            }
        }

        // putting it in a block should allow to use break out of the box
        instructions.push(W::block(
            "$switch-block",
            Signature::default(),
            cases_instructions,
        ));

        self.exit_block();

        instructions
    }

    fn translate_do_while_loop(&mut self, do_while_loop: &DoWhileLoop) -> InstructionsList {
        self.enter_block();

        let current_loop_break_name = self.current_loop_break_name();
        let current_continue_block_name = self.current_continue_block_name();
        let current_loop_name = self.current_loop_name();

        let condition = self.translate_expression(do_while_loop.cond(), true);
        let block_instructions = vec![
            W::local_get("$scope"),
            W::call("$new_scope"),
            W::local_set("$scope"),
            W::block(
                &current_continue_block_name,
                Signature::default(),
                self.translate_statement(do_while_loop.body()),
            ),
            W::local_get("$scope"),
            W::call("$extract_parent_scope"),
            W::local_set("$scope"),
            ..condition,
            W::call("$cast_ref_to_i32_bool"),
            W::i32_eqz(),
            W::br_if(&current_loop_break_name),
            W::br(&current_loop_name),
        ];

        vec![W::r#loop(
            &current_loop_name,
            vec![W::block(
                &current_loop_break_name,
                Signature::default(),
                block_instructions,
            )],
        )]
    }

    fn translate_while_loop(&mut self, while_loop: &WhileLoop) -> InstructionsList {
        self.enter_block();

        let current_loop_name = self.current_loop_name();
        let current_continue_block_name = self.current_continue_block_name();
        let current_loop_break_name = self.current_loop_break_name();

        let condition = self.translate_expression(while_loop.condition(), true);
        let block_instructions = vec![
            ..condition,
            W::call("$cast_ref_to_i32_bool"),
            W::i32_eqz(),
            W::br_if(&current_loop_break_name),
            W::local_get("$scope"),
            W::call("$new_scope"),
            W::local_set("$scope"),
            W::block(
                &current_continue_block_name,
                Signature::default(),
                self.translate_statement(while_loop.body()),
            ),
            W::local_get("$scope"),
            W::call("$extract_parent_scope"),
            W::local_set("$scope"),
            W::br(&current_loop_name),
        ];

        vec![W::r#loop(
            current_loop_name,
            vec![W::block(
                current_loop_break_name,
                Signature::default(),
                block_instructions,
            )],
        )]
    }

    fn translate_block(&mut self, block: &Block) -> InstructionsList {
        //self.enter_block();

        let mut instructions = vec![];
        for statement in block.statement_list().statements() {
            instructions.append(&mut self.translate_statement_list_item(statement));
        }
        let block_instr = W::block(self.random_block_name(), Signature::default(), instructions);

        let result = vec![block_instr];

        //self.exit_block();

        result
    }

    fn translate_statement_list_item(&mut self, statement: &StatementListItem) -> InstructionsList {
        match statement {
            StatementListItem::Statement(statement) => self.translate_statement(statement),
            StatementListItem::Declaration(declaration) => self.translate_declaration(declaration),
        }
    }
}

impl<'a> Visitor<'a> for WasmTranslator {
    type BreakTy = ();

    fn visit_var_declaration(&mut self, node: &'a VarDeclaration) -> ControlFlow<Self::BreakTy> {
        // println!(
        //     "visit_var_declaration: {}",
        //     node.to_interned_string(&self.interner)
        // );
        let instructions = self.translate_var(node);
        self.current_function().add_instructions(instructions);
        ControlFlow::Continue(())
    }

    fn visit_declaration(&mut self, node: &Declaration) -> ControlFlow<Self::BreakTy> {
        // println!(
        //     "visit_declaration: {}",
        //     node.to_interned_string(&self.interner)
        // );
        let instructions = self.translate_declaration(node);
        self.current_function().add_instructions(instructions);
        ControlFlow::Continue(())
    }

    fn visit_expression(&mut self, node: &Expression) -> ControlFlow<Self::BreakTy> {
        // println!(
        //     "visit_expression: {}",
        //     node.to_interned_string(&self.interner)
        // );
        let instructions = self.translate_expression(node, false);
        self.current_function().add_instructions(instructions);
        ControlFlow::Continue(())
    }

    fn visit_statement(&mut self, node: &'a Statement) -> ControlFlow<Self::BreakTy> {
        // println!(
        //     "visit_statement: {}",
        //     node.to_interned_string(&self.interner)
        // );
        match node {
            Statement::Expression(Expression::Literal(Literal::String(sym)))
                if self.interner.resolve(*sym).unwrap().to_string() == "use strict" =>
            {
                self.current_function()
                    .add_instruction(W::call("$enable_global_strict_mode"));
            }
            stmt => {
                let instructions = self.translate_statement(stmt);
                self.current_function().add_instructions(instructions);
            }
        }
        ControlFlow::Continue(())
    }

    fn visit_call(&mut self, node: &'a Call) -> ControlFlow<Self::BreakTy> {
        let instructions = self.translate_call(node, W::ref_null_any(), None, false);
        self.current_function().add_instructions(instructions);
        ControlFlow::Continue(())
    }
}

pub fn generate_data_string(pairs: &[(usize, String)]) -> String {
    let mut result = String::new();

    // First 4 bytes - length of entries (little-endian)
    let len = pairs.len() as i32;
    result.push_str(&format!(
        "\\{:02x}\\{:02x}\\{:02x}\\{:02x}",
        len & 0xff, // Least significant byte first
        (len >> 8) & 0xff,
        (len >> 16) & 0xff,
        (len >> 24) & 0xff // Most significant byte last
    ));

    // For each pair, add offset (4 bytes) and length (4 bytes)
    for (offset, str) in pairs {
        let offset = *offset as i32;
        result.push_str(&format!(
            "\\{:02x}\\{:02x}\\{:02x}\\{:02x}",
            offset & 0xff, // Least significant byte first
            (offset >> 8) & 0xff,
            (offset >> 16) & 0xff,
            (offset >> 24) & 0xff // Most significant byte last
        ));

        let len = str.len() as i32;
        result.push_str(&format!(
            "\\{:02x}\\{:02x}\\{:02x}\\{:02x}",
            len & 0xff, // Least significant byte first
            (len >> 8) & 0xff,
            (len >> 16) & 0xff,
            (len >> 24) & 0xff // Most significant byte last
        ));
    }

    result
}
