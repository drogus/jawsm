use anyhow::anyhow;
use boa_ast::{
    declaration::{Declaration, LexicalDeclaration, VarDeclaration, VariableList},
    expression::{
        self,
        access::PropertyAccess,
        literal::{ArrayLiteral, Literal, ObjectLiteral},
        operator::{
            binary::{ArithmeticOp, BinaryOp, LogicalOp},
            update::UpdateTarget,
            Assign, Binary, Unary, Update,
        },
        Await, Call, Expression, Identifier, New, Parenthesized,
    },
    function::{
        ArrowFunction, AsyncFunction, FormalParameter, FormalParameterList, Function, FunctionBody,
    },
    statement::{
        Block, Case, Catch, DoWhileLoop, ErrorHandler, Finally, ForInLoop, ForLoop, ForOfLoop, If,
        Return, Statement, Switch, Throw, Try, WhileLoop, With,
    },
    visitor::{VisitWith, Visitor},
    Script, StatementList, StatementListItem,
};
use boa_interner::{Interner, JStrRef, Sym, ToInternedString};
use boa_parser::{Parser, Source};
use rand::{distributions::Alphanumeric, Rng};
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read, Write},
    ops::ControlFlow,
    path::Path,
};

use jawsm::tail_call_transformer::TailCallTransformer;
use tarnik_ast::{
    InstructionsList, Nullable, Signature, WasmType, WatFunction, WatInstruction as W, WatModule,
};

enum VarType {
    Const,
    Let,
    Var,
    Param,
}

impl VarType {
    fn to_i32(&self) -> i32 {
        match self {
            VarType::Const => 0,
            VarType::Let => 1,
            VarType::Var => 2,
            VarType::Param => 3,
        }
    }
}

fn gen_function_name(s: Option<String>) -> String {
    let r: String = rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(7)
        .map(char::from)
        .collect();

    if let Some(s) = s {
        format!("{s}-{r}")
    } else {
        format!("function-{r}")
    }
}

struct WasmTranslator {
    module: WatModule,
    function_stack: Vec<WatFunction>,
    interner: Interner,
    functions: HashMap<String, String>,
    init_code: Vec<String>,
    data_entries: HashMap<i32, String>,
    string_offsets: HashMap<String, i32>,
    data_offset: i32,
    identifiers_map: HashMap<i32, i32>,
    current_block_number: u32,
}

impl WasmTranslator {
    fn new(interner: Interner) -> Self {
        let module = WatModule::new();
        let function = WatFunction::new("init".to_string());
        Self {
            module,
            function_stack: vec![function],
            interner,
            functions: HashMap::new(),
            init_code: Vec::new(),
            data_entries: HashMap::new(),
            string_offsets: HashMap::new(),
            data_offset: 300,
            identifiers_map: HashMap::new(),
            current_block_number: 0,
        }
    }

    fn add_new_symbol(&mut self, sym: Sym, value: &str) -> i32 {
        if let Some(offset) = self.identifiers_map.get(&(sym.get() as i32)) {
            *offset
        } else {
            let (offset, _) = self.insert_data_string(value);
            self.identifiers_map.insert(sym.get() as i32, offset);
            offset
        }
    }

    fn add_symbol(&mut self, sym: Sym) -> i32 {
        self.add_new_symbol(sym, &self.interner.resolve(sym).unwrap().to_string())
    }

    fn add_string(&mut self, s: impl Into<String>) -> i32 {
        let s: String = s.into();
        let sym = self.interner.get_or_intern(JStrRef::Utf8(&s));
        self.add_new_symbol(sym, &s)
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

    fn exit_function(&mut self) {
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

    fn translate_return(&mut self, ret: &Return) -> InstructionsList {
        // println!("Return: {ret:#?}");
        let mut instructions = Vec::new();
        if let Some(target) = ret.target() {
            instructions.append(&mut self.translate_expression(target, true));
        } else {
            instructions.push(W::ref_null_any());
        }
        instructions.push(W::r#return());
        instructions
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
        self.current_function()
            .add_instruction(W::local_get("$parentScope"));
        self.current_function()
            .add_instruction(W::call("$new_scope"));
        self.current_function()
            .add_instruction(W::local_set("$scope"));

        // set parameters on the scope
        for (i, param) in params.as_ref().iter().enumerate() {
            match param.variable().binding() {
                boa_ast::declaration::Binding::Identifier(identifier) => {
                    let offset = self.add_identifier(identifier);
                    self.current_function().add_instructions(vec![
                        W::local_get("$scope"),
                        W::i32_const(offset),
                        W::local_get("$arguments"),
                        W::i32_const(i as i32),
                        W::array_get("$JSArgs"),
                        W::i32_const(VarType::Param.to_i32()),
                    ]);
                    self.current_function()
                        .add_instruction(W::call("$declare_variable"));
                }
                boa_ast::declaration::Binding::Pattern(_pattern) => todo!(),
            }
        }

        for statement in body.statements().statements() {
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

    fn translate_function(&mut self, fun: &Function) -> InstructionsList {
        self.translate_function_generic(fun.name(), fun.parameters(), fun.body())
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
        // println!("LET: {:#?}", decl.0);
        // TODO: variables behave a bit differently when it comes to hoisting
        // for now I just ignore it, but it should be fixed
        // https://developer.mozilla.org/en-US/docs/Glossary/Hoisting
        self.translate_let_vars(&decl.0, VarType::Var)
    }

    fn translate_call(
        &mut self,
        call: &Call,
        get_this: W,
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
                    W::call("$set-timeout"),
                ]);
            } else {
                // TODO: throw TypeError
            }
        } else {
            // Add a local for arguments to the current function
            let call_arguments = self.current_function().add_local(
                "$call_arguments",
                WasmType::Ref("$JSArgs".into(), Nullable::False),
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
                    W::i32_const(index as i32),
                    W::local_get(&temp_arg),
                    W::array_set("$JSArgs"),
                ]);
            }

            if function_name == "console.log" {
                instructions.append(&mut vec![
                    W::local_get(&call_arguments),
                    W::call("$log"),
                    W::i32_const(1),
                ]);
            } else {
                // Translate the function expression
                let function_local = self
                    .current_function()
                    .add_local("$function", WasmType::Anyref);
                instructions.append(&mut self.translate_expression(call.function(), true));
                instructions.push(W::local_set(&function_local));

                // Call the function
                instructions.append(&mut vec![
                    W::local_get(&function_local),
                    get_this,
                    W::local_get(&call_arguments),
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

        let var_name = self.current_function().add_local("$var", WasmType::Anyref);

        let mut instructions = Vec::new();
        // TODO: handle hoisting
        for var in variable_list.as_ref() {
            match var.binding() {
                Binding::Identifier(identifier) => {
                    let offset = self.add_identifier(identifier);
                    if let Some(expression) = var.init() {
                        instructions.append(&mut self.translate_expression(expression, true));
                    } else {
                        instructions.push(W::ref_null_any());
                    }
                    instructions.push(W::local_set(&var_name));

                    instructions.push(W::local_get("$scope"));
                    instructions.push(W::i32_const(offset));
                    instructions.push(W::local_get(&var_name));
                    instructions.push(W::i32_const(var_type.to_i32()));
                    instructions.push(W::call("$declare_variable"));
                }
                Binding::Pattern(_pattern) => todo!(),
            }
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
                    ArithmeticOp::Exp => "$exp",
                    ArithmeticOp::Mod => "$mod",
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
            BinaryOp::Bitwise(_bitwise_op) => todo!(),
            BinaryOp::Relational(relational_op) => {
                let func_name = match relational_op {
                    RelationalOp::Equal => "$loose_equal",
                    RelationalOp::NotEqual => todo!(),
                    RelationalOp::StrictEqual => "$strict_equal",
                    RelationalOp::StrictNotEqual => "$strict_not_equal",
                    RelationalOp::GreaterThan => todo!(),
                    RelationalOp::GreaterThanOrEqual => "$greater_than_or_equal",
                    RelationalOp::LessThan => "$less_than",
                    RelationalOp::LessThanOrEqual => todo!(),
                    RelationalOp::In => todo!(),
                    RelationalOp::InstanceOf => todo!(),
                };
                let rhs = self.current_function().add_local("$rhs", WasmType::Anyref);
                let lhs = self.current_function().add_local("$lhs", WasmType::Anyref);

                let mut result = vec![];
                result.append(&mut self.translate_expression(binary.lhs(), true));
                result.push(W::local_set(&lhs));
                result.append(&mut self.translate_expression(binary.rhs(), true));
                result.append(&mut vec![
                    W::local_set(&rhs),
                    W::local_get(&lhs),
                    W::local_get(&rhs),
                    W::call(func_name),
                ]);
                result
            }
            BinaryOp::Logical(logical_op) => {
                let func_name = match logical_op {
                    LogicalOp::And => "$logical_and",
                    LogicalOp::Or => "$logical_or",
                    LogicalOp::Coalesce => "$logical_coalesce",
                };
                let rhs = self.current_function().add_local("$rhs", WasmType::Anyref);
                let lhs = self.current_function().add_local("$lhs", WasmType::Anyref);

                let mut result = vec![];
                result.append(&mut self.translate_expression(binary.lhs(), true));
                result.push(W::local_set(&lhs));
                result.append(&mut self.translate_expression(binary.rhs(), true));
                result.append(&mut vec![
                    W::local_set(&rhs),
                    W::local_get(&lhs),
                    W::local_get(&rhs),
                    W::call(func_name),
                ]);
                result
            }
            BinaryOp::Comma => todo!(),
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

                        if let Some(mut assign_instructions) = assign {
                            let temp = self.current_function().add_local("$temp", WasmType::Anyref);
                            let mut result = vec![];
                            result.append(&mut assign_instructions);
                            result.push(W::local_set(&temp));
                            result.append(&mut target);
                            result.append(&mut vec![
                                W::i32_const(offset),
                                W::local_get(&temp),
                                W::call("$set_property"),
                            ]);
                            result
                        } else {
                            target
                                .append(&mut vec![W::i32_const(offset), W::call("$get_property")]);
                            target
                        }
                    }
                    PropertyAccessField::Expr(expression) => {
                        todo!()
                        // let expr_result_var =
                        //     self.current_function().add_local("$expr_result", WasmType::Anyref);
                        // let expr_result_instr = self.translate_expression(expression, true);
                        //
                        //  TODO:
                        //
                        //  we need to:
                        //  1. create a function to convert various types to string
                        //  2. create a way to put those strings into an array (on an object
                        // itself, most probably)
                        //  3. use the mappings to translate string access into i32 access
                    }
                }
            }
            PropertyAccess::Private(_private_property_access) => todo!(),
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
            Expression::Literal(literal) => self.translate_literal(literal),
            Expression::RegExpLiteral(_reg_exp_literal) => todo!(),
            Expression::ArrayLiteral(array_literal) => {
                self.translate_array_literal(array_literal, will_use_return)
            }
            Expression::ObjectLiteral(object_literal) => {
                self.translate_object_literal(object_literal, will_use_return)
            }
            Expression::Spread(_spread) => todo!(),
            Expression::Function(function) => self.translate_function(function),
            Expression::ArrowFunction(arrow_function) => {
                self.translate_arrow_function(arrow_function)
            }
            Expression::AsyncArrowFunction(_async_arrow_function) => todo!(),
            Expression::Generator(_generator) => todo!(),
            Expression::AsyncFunction(async_function) => {
                self.translate_async_function(async_function)
            }
            Expression::AsyncGenerator(_async_generator) => todo!(),
            Expression::Class(_class) => todo!(),
            Expression::TemplateLiteral(_template_literal) => todo!(),
            Expression::PropertyAccess(property_access) => {
                self.translate_property_access(property_access, None)
            }
            Expression::New(new) => self.translate_new(new),
            // TODO: the default this value is a global object
            Expression::Call(call) => self.translate_call(call, W::ref_null_any(), will_use_return),
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
            Expression::Conditional(_conditional) => todo!(),
            Expression::Await(await_expr) => self.translate_await_expression(await_expr),
            Expression::Yield(_) => todo!(),
            Expression::Parenthesized(parenthesized) => self.translate_parenthesized(parenthesized),
            _ => todo!(),
        }
    }

    fn translate_await_expression(&mut self, await_expression: &Await) -> InstructionsList {
        // println!("AWAIT: {await_expression:#?}");
        todo!();
        vec![]
    }

    fn convert_return_to_resolve(&mut self, statement: &Statement) -> Statement {
        match statement {
            Statement::Return(ret) => {
                // Create resolve() call with the return value
                let resolve_call = Expression::Call(Call::new(
                    Expression::Identifier(Identifier::new(
                        self.interner.get_or_intern(JStrRef::Utf8("resolve")),
                    )),
                    if let Some(target) = ret.target() {
                        Box::new([target.clone()])
                    } else {
                        Box::new([])
                    },
                ));

                // Create a block with resolve() + return
                Statement::Return(Return::new(Some(resolve_call)))
            }
            _ => statement.clone(),
        }
    }

    fn transform_return_in_statement_list(&mut self, statements: StatementList) -> StatementList {
        StatementList::new(
            statements
                .statements()
                .iter()
                .map(|s| match s {
                    StatementListItem::Statement(statement) => {
                        StatementListItem::Statement(self.transform_return_in_statement(statement))
                    }
                    sli => sli.clone(),
                })
                .collect::<Vec<StatementListItem>>(),
            true,
        )
    }

    fn transform_return_in_statement(&mut self, stmt: &Statement) -> Statement {
        match stmt {
            Statement::Return(_) => self.convert_return_to_resolve(stmt),
            Statement::Block(block) => Statement::Block(Block::from(
                self.transform_return_in_statement_list(block.statement_list().clone()),
            )),
            Statement::If(if_stmt) => {
                let cond = if_stmt.cond();
                let body = self.transform_return_in_statement(if_stmt.body());
                let else_node = if_stmt
                    .else_node()
                    .map(|stmt| self.transform_return_in_statement(stmt));
                Statement::If(If::new(cond.clone(), body, else_node))
            }
            Statement::DoWhileLoop(lp) => {
                let body = self.transform_return_in_statement(lp.body());
                Statement::DoWhileLoop(DoWhileLoop::new(body, lp.cond().clone()))
            }
            Statement::WhileLoop(lp) => {
                let body = self.transform_return_in_statement(lp.body());
                Statement::WhileLoop(WhileLoop::new(lp.condition().clone(), body))
            }
            Statement::ForLoop(lp) => {
                let body = self.transform_return_in_statement(lp.body());
                Statement::ForLoop(ForLoop::new(
                    lp.init().cloned(),
                    lp.condition().cloned(),
                    lp.final_expr().cloned(),
                    body,
                ))
            }
            Statement::ForInLoop(lp) => {
                let body = self.transform_return_in_statement(lp.body());
                Statement::ForInLoop(ForInLoop::new(
                    lp.initializer().clone(),
                    lp.target().clone(),
                    body,
                ))
            }
            Statement::ForOfLoop(lp) => {
                let body = self.transform_return_in_statement(lp.body());
                Statement::ForOfLoop(ForOfLoop::new(
                    lp.initializer().clone(),
                    lp.iterable().clone(),
                    body,
                    lp.r#await(),
                ))
            }
            Statement::Switch(sw) => {
                let cases = sw
                    .cases()
                    .iter()
                    .map(|case| {
                        let body = self.transform_return_in_statement_list(case.body().clone());
                        if case.is_default() {
                            Case::default(body)
                        } else {
                            Case::new(case.condition().unwrap().clone(), body)
                        }
                    })
                    .collect();
                Statement::Switch(Switch::new(sw.val().clone(), cases))
            }
            Statement::Try(tr) => {
                let block = Block::from(
                    self.transform_return_in_statement_list(tr.block().statement_list().clone()),
                );
                let handler = match (tr.catch(), tr.finally()) {
                    (Some(c), None) => ErrorHandler::Catch(c.clone()),
                    (None, Some(f)) => ErrorHandler::Finally(f.clone()),
                    (Some(c), Some(f)) => ErrorHandler::Full(c.clone(), f.clone()),
                    _ => unreachable!(),
                };
                Statement::Try(Try::new(block, handler))
            }
            Statement::With(with) => {
                let statement = self.transform_return_in_statement(with.statement());
                Statement::With(With::new(with.expression().clone(), statement))
            }
            _ => stmt.clone(),
        }
    }

    fn transform_function_body(&mut self, body: &Script) -> Script {
        let mut new_statements = Vec::new();

        for statement in body.statements().statements() {
            match statement {
                StatementListItem::Statement(stmt) => {
                    new_statements.push(StatementListItem::Statement(
                        self.transform_return_in_statement(stmt),
                    ));
                }
                StatementListItem::Declaration(decl) => {
                    new_statements.push(StatementListItem::Declaration(decl.clone()));
                }
            }
        }

        // Add final resolve(), just in case there was no return earlier
        new_statements.push(StatementListItem::Statement(Statement::Expression(
            Expression::Call(Call::new(
                Expression::Identifier(Identifier::new(
                    self.interner.get_or_intern(JStrRef::Utf8("resolve")),
                )),
                Box::new([]),
            )),
        )));

        Script::new(StatementList::new(new_statements, true))
    }

    fn translate_async_function(&mut self, async_function: &AsyncFunction) -> InstructionsList {
        use boa_ast::declaration::Variable;
        use boa_ast::expression::Identifier;

        // Create the promise callback function
        let callback_params = FormalParameterList::from_parameters(vec![
            FormalParameter::new(
                Variable::from_identifier(
                    Identifier::new(self.interner.get_or_intern(JStrRef::Utf8("resolve"))),
                    None,
                ),
                false,
            ),
            FormalParameter::new(
                Variable::from_identifier(
                    Identifier::new(self.interner.get_or_intern(JStrRef::Utf8("reject"))),
                    None,
                ),
                false,
            ),
        ]);

        // Transform the original function body
        let transformed_body = self.transform_function_body(async_function.body());

        // Create the callback function
        let callback_function = Function::new(None, callback_params, transformed_body);

        // Create the Promise constructor call
        let promise_function = Expression::Function(callback_function);
        let promise_new = Expression::New(
            Call::new(
                Expression::Identifier(Identifier::new(
                    self.interner.get_or_intern(JStrRef::Utf8("Promise")),
                )),
                Box::new([promise_function]),
            )
            .into(),
        );

        // Create the outer function that returns the promise
        let outer_function = Function::new(
            async_function.name(),
            async_function.parameters().clone(),
            Script::new(StatementList::new(
                vec![StatementListItem::Statement(Statement::Return(
                    Return::new(Some(promise_new)),
                ))],
                true,
            )),
        );

        // println!("{}", outer_function.to_interned_string(&self.interner));
        self.translate_function(&outer_function)
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
            W::call("$new_array"),
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
        use boa_ast::property::{PropertyDefinition, PropertyName};

        let mut instructions = Vec::new();
        let new_instance = self.current_function().add_local(
            "$new_instance",
            WasmType::Ref("$Object".into(), Nullable::False),
        );
        let temp = self.current_function().add_local("$temp", WasmType::Anyref);

        instructions.push(W::call("$new_object"));
        instructions.push(W::local_set(&new_instance));

        for property in object_literal.properties() {
            let mut instr = match property {
                PropertyDefinition::IdentifierReference(identifier) => {
                    let offset = self.add_identifier(identifier);
                    let mut result = self.translate_identifier(identifier);
                    result.append(&mut vec![
                        W::local_set(&temp),
                        W::local_get(&new_instance),
                        W::i32_const(offset),
                        W::local_get(&temp),
                        W::call("$set_property"),
                    ]);
                    result
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
                            W::call("$set_property"),
                        ]);
                        result
                    }
                    PropertyName::Computed(_) => todo!(),
                },
                PropertyDefinition::MethodDefinition(property_name, method_definition) => {
                    match property_name {
                        PropertyName::Literal(sym) => {
                            let offset = self.add_symbol(*sym);
                            let func_instr = match method_definition {
                                boa_ast::property::MethodDefinition::Get(_) => todo!(),
                                boa_ast::property::MethodDefinition::Set(_) => todo!(),
                                boa_ast::property::MethodDefinition::Ordinary(function) => {
                                    self.translate_function(function)
                                }
                                boa_ast::property::MethodDefinition::Generator(_) => todo!(),
                                boa_ast::property::MethodDefinition::AsyncGenerator(_) => todo!(),
                                boa_ast::property::MethodDefinition::Async(_) => todo!(),
                            };
                            let mut result = func_instr;
                            result.append(&mut vec![
                                W::local_set(&temp),
                                W::local_get(&new_instance),
                                W::i32_const(offset),
                                W::local_get(&temp),
                                W::call("$set_property"),
                            ]);
                            result
                        }
                        PropertyName::Computed(_) => todo!(),
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
        let mut result = vec![W::call("$new_object"), W::local_set(&new_instance)];
        result.append(&mut self.translate_call(new.call(), W::local_get(&new_instance), true));
        result.append(&mut vec![
            W::local_get(&new_instance),
            W::call("$return_object_or"),
        ]);
        result
    }

    fn translate_arrow_function(&mut self, function: &ArrowFunction) -> InstructionsList {
        self.translate_function_generic(function.name(), function.parameters(), function.body())
    }

    fn translate_update(&mut self, update: &Update) -> InstructionsList {
        use boa_ast::expression::operator::update::UpdateOp;
        let identifier = match update.target() {
            UpdateTarget::Identifier(identifier) => identifier,
            UpdateTarget::PropertyAccess(_property_access) => todo!(),
        };
        let var = self.current_function().add_local("$var", WasmType::Anyref);

        // TODO: figure out pre vs post behaviour
        let instruction = match update.op() {
            UpdateOp::IncrementPost => W::call("$increment_number"),
            UpdateOp::IncrementPre => W::call("$increment_number"),
            UpdateOp::DecrementPost => W::call("$decrement_number"),
            UpdateOp::DecrementPre => W::call("$decrement_number"),
        };
        let mut target = self.translate_identifier(identifier);
        let offset = self.add_identifier(identifier);
        let mut set_variable = vec![
            W::local_get("$scope".to_string()),
            W::i32_const(offset),
            W::local_get(&var),
            W::call("$set_variable"),
        ];

        let mut result = vec![];
        result.append(&mut target);
        result.append(&mut vec![instruction, W::local_set(&var)]);
        result.append(&mut set_variable);
        result
    }

    fn translate_assign(&mut self, assign: &Assign) -> InstructionsList {
        use boa_ast::expression::operator::assign::AssignOp;
        use boa_ast::expression::operator::assign::AssignTarget;

        match assign.op() {
            AssignOp::Assign => {
                let mut rhs = self.translate_expression(assign.rhs(), true);
                match assign.lhs() {
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
                    AssignTarget::Pattern(_pattern) => todo!(),
                }
            }
            AssignOp::Add => {
                let mut rhs = self.translate_expression(assign.rhs(), true);
                match assign.lhs() {
                    AssignTarget::Identifier(identifier) => {
                        let offset = self.add_identifier(identifier);
                        // identifier.sym().get(),
                        let rhs_var = self.current_function().add_local("$rhs", WasmType::Anyref);
                        let mut result = vec![];
                        result.append(&mut rhs);
                        result.append(&mut vec![
                            W::local_set(&rhs_var),
                            W::local_get("$scope"),
                            W::i32_const(offset),
                            W::call("$get_variable"),
                            W::local_get(&rhs_var),
                            W::call("$add"),
                            W::local_set(&rhs_var),
                            W::local_get("$scope".to_string()),
                            W::i32_const(offset),
                            W::local_get(&rhs_var),
                            W::call("$set_variable"),
                        ]);
                        result
                    }
                    AssignTarget::Access(property_access) => {
                        let rhs_var = self.current_function().add_local("$rhs", WasmType::Anyref);
                        let mut result = vec![];
                        result.append(&mut rhs);
                        result.push(W::local_set(&rhs_var));
                        result.append(&mut self.translate_property_access(property_access, None));
                        result.append(&mut vec![
                            W::local_get(&rhs_var),
                            W::call("$add"),
                            W::local_set(&rhs_var),
                        ]);
                        result.append(&mut self.translate_property_access(
                            property_access,
                            Some(vec![W::local_get(&rhs_var)]),
                        ));
                        result
                    }
                    AssignTarget::Pattern(_pattern) => todo!(),
                }
            }
            AssignOp::Sub => todo!(),
            AssignOp::Mul => todo!(),
            AssignOp::Div => todo!(),
            AssignOp::Mod => todo!(),
            AssignOp::Exp => todo!(),
            AssignOp::And => todo!(),
            AssignOp::Or => todo!(),
            AssignOp::Xor => todo!(),
            AssignOp::Shl => todo!(),
            AssignOp::Shr => todo!(),
            AssignOp::Ushr => todo!(),
            AssignOp::BoolAnd => todo!(),
            AssignOp::BoolOr => todo!(),
            AssignOp::Coalesce => todo!(),
        }
    }

    fn translate_unary(&mut self, unary: &Unary) -> InstructionsList {
        use boa_ast::expression::operator::unary::UnaryOp;

        let mut target = self.translate_expression(unary.target(), true);
        match unary.op() {
            UnaryOp::Minus => todo!(),
            UnaryOp::Plus => todo!(),
            UnaryOp::Not => target.push(W::call("$logical_not")),
            UnaryOp::Tilde => todo!(),
            UnaryOp::TypeOf => target.push(W::call("$type_of")),
            UnaryOp::Delete => todo!(),
            UnaryOp::Void => todo!(),
        }

        target
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
            Declaration::Function(decl) => {
                let mut declaration = self.translate_function(decl);
                // function declaration still needs to be added to the scope if function has a name
                // TODO: declared functions need to be hoisted
                if let Some(name) = decl.name() {
                    let offset = self.add_identifier(&name);
                    let mut result = vec![W::local_get("$scope"), W::i32_const(offset)];
                    result.append(&mut declaration);
                    result.append(&mut vec![
                        W::i32_const(VarType::Var.to_i32()),
                        W::call("$declare_variable".to_string()),
                    ]);
                    result
                } else {
                    // TODO: if it's empty and not called right away I guess we can just ignore it?
                    declaration
                }
            }
            Declaration::Lexical(v) => self.translate_lexical(v),
            Declaration::Generator(_generator) => todo!(),
            Declaration::AsyncFunction(decl) => {
                let mut declaration = self.translate_async_function(decl);
                if let Some(name) = decl.name() {
                    let offset = self.add_identifier(&name);
                    let mut result = vec![W::local_get("$scope"), W::i32_const(offset)];
                    result.append(&mut declaration);
                    result.append(&mut vec![
                        W::i32_const(VarType::Var.to_i32()),
                        W::call("$declare_variable".to_string()),
                    ]);
                    result
                } else {
                    // TODO: if it's empty and not called right away I guess we can just ignore it?
                    declaration
                }
            }
            Declaration::AsyncGenerator(_async_generator) => todo!(),
            Declaration::Class(_class) => todo!(),
        }
    }

    fn additional_functions(&self) -> String {
        "".into()
    }

    fn insert_data_string(&mut self, s: &str) -> (i32, i32) {
        let value = s.replace("\"", "\\\"");
        let len = value.len() as i32;
        let offset = self.data_offset;
        if let Some(offset) = self.string_offsets.get(&value) {
            (*offset, len)
        } else {
            self.data_entries.insert(offset, value.clone());
            self.string_offsets.insert(value, offset);
            self.data_offset += if len % 4 == 0 {
                len
            } else {
                // some runtimes expect all data aligned to 4 bytes
                len + (4 - len % 4)
            };

            (offset, len)
        }
    }

    fn translate_statement(&mut self, statement: &Statement) -> InstructionsList {
        match statement {
            Statement::Block(block) => self.translate_block(block),
            Statement::Var(var_declaration) => self.translate_var(var_declaration),
            Statement::Empty => vec![],
            Statement::Expression(expression) => self.translate_expression(expression, false),
            Statement::If(if_statement) => self.translate_if_statement(if_statement),
            Statement::DoWhileLoop(_do_while_loop) => todo!(),
            Statement::WhileLoop(while_loop) => self.translate_while_loop(while_loop),
            Statement::ForLoop(_for_loop) => todo!(),
            Statement::ForInLoop(_for_in_loop) => todo!(),
            Statement::ForOfLoop(_for_of_loop) => todo!(),
            Statement::Switch(_switch) => todo!(),
            Statement::Continue(_) => todo!(),
            Statement::Break(_) => todo!(),
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
                match binding {
                    Binding::Identifier(identifier) => {
                        let temp = self.current_function().add_local("$temp", WasmType::Anyref);
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
                    Binding::Pattern(_) => todo!(),
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

    fn translate_while_loop(&mut self, while_loop: &WhileLoop) -> InstructionsList {
        let mut condition = self.translate_expression(while_loop.condition(), true);
        let mut block_instructions = vec![];
        block_instructions.append(&mut condition);
        block_instructions.append(&mut vec![
            W::call("$cast_ref_to_i32_bool"),
            W::i32_eqz(),
            W::br_if("$break"),
        ]);
        block_instructions.append(&mut self.translate_statement(while_loop.body()));
        block_instructions.push(W::br("$while_loop"));
        vec![W::r#loop(
            "$while_loop".to_string(),
            vec![W::block("$break", Signature::default(), block_instructions)],
        )]
    }

    fn translate_block(&mut self, block: &Block) -> InstructionsList {
        self.enter_block();
        let mut instructions = vec![];
        for statement in block.statement_list().statements() {
            instructions.append(&mut self.translate_statement_list_item(statement));
        }
        let block_instr = W::block(
            self.current_block_name(),
            Signature::default(),
            instructions,
        );
        self.exit_block();

        vec![block_instr]
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
        let instructions = self.translate_statement(node);
        self.current_function().add_instructions(instructions);
        ControlFlow::Continue(())
    }

    fn visit_call(&mut self, node: &'a Call) -> ControlFlow<Self::BreakTy> {
        let instructions = self.translate_call(node, W::ref_null_any(), false);
        self.current_function().add_instructions(instructions);
        ControlFlow::Continue(())
    }
}

fn main() -> anyhow::Result<()> {
    let mut js_code = String::new();
    io::stdin().read_to_string(&mut js_code)?;

    let mut interner = Interner::default();

    let mut parser = Parser::new(Source::from_bytes(&js_code));
    let ast = parser
        .parse_script(&mut interner)
        .map_err(|e| anyhow!("JAWSM parsing error: {e}"))?;

    let mut translator = WasmTranslator::new(interner);
    // println!("{ast:#?}");
    ast.visit_with(&mut translator);
    // exit $init function
    translator.exit_function();

    let scope_type = tarnik_ast::WasmType::Ref("$Scope".into(), Nullable::False);
    let init = translator.module.get_function_mut("init").unwrap();
    init.add_local_exact("$scope", scope_type.clone());

    // TODO: I'm not a big fan of this, cause it's in reverse order
    init.body.push_front(W::local_set("$scope"));
    init.body.push_front(W::ref_cast(scope_type));
    init.body.push_front(W::global_get("$global_scope"));

    let module = translator.module.clone();
    let mut module = TailCallTransformer::new(module).transform();
    let mut module = jawsm::wasm::generate_module(&mut module);

    for (offset, value) in translator.data_entries {
        module.add_data_raw(offset as usize, value);
    }

    let jawsm_dir = std::env::var("JAWSM_DIR").unwrap_or(".".into());
    let mut f = File::create(Path::new(&jawsm_dir).join("wat/generated.wat")).unwrap();
    f.write_all(module.to_string().as_bytes()).unwrap();

    // println!("WAT modules generated successfully!");
    Ok(())
}
