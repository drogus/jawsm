use anyhow::{anyhow, Context, Result};
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
    ops::{ControlFlow, Deref},
    path::Path,
};

mod wat_ast;
mod wat_template;
use jawsm::remove_lists_transformer::RemoveListsTransformer;
use jawsm::tail_call_transformer::TailCallTransformer;
use jawsm::wat_ast::{WatFunction, WatInstruction as W, WatModule};

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

    fn translate_return(&mut self, ret: &Return) -> Box<W> {
        // println!("Return: {ret:#?}");
        let mut instructions = Vec::new();
        if let Some(target) = ret.target() {
            instructions.push(self.translate_expression(target, true));
        } else {
            instructions.push(W::ref_null("any"));
        }
        instructions.push(W::r#return());
        W::list(instructions)
    }

    fn translate_function_generic(
        &mut self,
        name: Option<Identifier>,
        params: &FormalParameterList,
        body: &FunctionBody,
    ) -> Box<W> {
        let function_name = gen_function_name(name.map(|i| i.to_interned_string(&self.interner)));
        let wat_function = WatFunction::new(function_name.clone());
        self.enter_function(wat_function);

        self.current_function()
            .add_param("$parentScope".to_string(), "(ref $Scope)".to_string());
        self.current_function()
            .add_param("$this".to_string(), "anyref".to_string());
        self.current_function()
            .add_param("$arguments".to_string(), "(ref $JSArgs)".to_string());
        self.current_function().add_result("anyref".to_string());

        self.current_function()
            .add_local_exact("$scope", "(ref $Scope)");
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
                    self.current_function().add_instruction(W::list(vec![
                        W::local_get("$scope"),
                        W::i32_const(offset),
                        W::instruction(
                            "array.get",
                            vec![
                                W::r#type("$JSArgs"),
                                W::local_get("$arguments"),
                                W::i32_const(i as i32),
                            ],
                        ),
                        W::i32_const(VarType::Param.to_i32()),
                    ]));
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
                    self.current_function().add_instruction(res);
                }
                boa_ast::StatementListItem::Declaration(declaration) => {
                    let declaration = self.translate_declaration(declaration);
                    self.current_function().add_instruction(declaration);
                }
            }
        }

        // This is a bit dumb, but it will work for now - every $JSFunc
        // has to return a value. If we already returned this will get ignored
        // If not, ie. there is no return statement, we will return undefined
        self.current_function()
            .add_instruction(W::list(vec![W::ref_null("any"), W::r#return()]));

        self.exit_function();

        W::list(vec![
            W::local_get("$scope"),
            W::ref_func(function_name),
            W::ref_null("any"),
            W::call("$new_function"),
        ])
    }

    fn translate_function(&mut self, fun: &Function) -> Box<W> {
        self.translate_function_generic(fun.name(), fun.parameters(), fun.body())
    }

    fn translate_lexical(&mut self, decl: &LexicalDeclaration) -> Box<W> {
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

    fn translate_var(&mut self, decl: &VarDeclaration) -> Box<W> {
        // println!("LET: {:#?}", decl.0);
        // TODO: variables behave a bit differently when it comes to hoisting
        // for now I just ignore it, but it should be fixed
        // https://developer.mozilla.org/en-US/docs/Glossary/Hoisting
        self.translate_let_vars(&decl.0, VarType::Var)
    }

    fn translate_call(&mut self, call: &Call, get_this: Box<W>, will_use_return: bool) -> Box<W> {
        // println!(
        //     "translate_call {}",
        //     call.function().to_interned_string(&self.interner)
        // );
        let function_name = call.function().to_interned_string(&self.interner);
        let mut instructions = Vec::new();

        if function_name == "setTimeout" {
            if let Some(callback) = call.args().get(0) {
                let callback_var = self.current_function().add_local("$callback", "anyref");
                let duration_var = self.current_function().add_local("$duration", "anyref");
                instructions.push(self.translate_expression(callback, true));
                instructions.push(W::local_set(&callback_var));

                let time = if let Some(time) = call.args().get(1) {
                    self.translate_expression(time, true)
                } else {
                    // pass undefined
                    W::ref_null("any")
                };
                instructions.push(time);
                instructions.push(W::local_set(&duration_var));

                // the rest of arguments doesn't matter
                instructions.push(W::list(vec![
                    W::local_get(&callback_var),
                    W::local_get(&duration_var),
                    W::call("$set-timeout"),
                ]));
            } else {
                // TODO: throw TypeError
            }
        } else {
            // Add a local for arguments to the current function
            let call_arguments = self
                .current_function()
                .add_local("$call_arguments", "(ref $JSArgs)");
            let temp_arg = self.current_function().add_local("$temp_arg", "anyref");

            // Create the arguments array
            let args_count = call.args().len() as i32;
            instructions.push(W::array_new(
                "$JSArgs",
                W::ref_null("any"),
                W::i32_const(args_count),
            ));
            instructions.push(W::local_set(&call_arguments));

            // Populate the arguments array
            for (index, arg) in call.args().iter().enumerate() {
                let arg_instruction = self.translate_expression(arg, true);
                instructions.push(W::list(vec![
                    arg_instruction,
                    W::local_set(&temp_arg),
                    W::instruction(
                        "array.set",
                        vec![
                            W::r#type("$JSArgs"),
                            W::local_get(&call_arguments),
                            W::i32_const(index as i32),
                            W::local_get(&temp_arg),
                        ],
                    ),
                ]));
            }

            if function_name == "console.log" {
                instructions.push(W::list(vec![
                    W::local_get(&call_arguments),
                    W::call("$log"),
                    W::i32_const(1),
                ]));
            } else {
                // Translate the function expression
                let function_local = self.current_function().add_local("$function", "anyref");
                instructions.push(self.translate_expression(call.function(), true));
                instructions.push(W::local_set(&function_local));

                // Call the function
                instructions.push(W::list(vec![
                    W::local_get(&function_local),
                    get_this,
                    W::local_get(&call_arguments),
                    W::call("$call_function"),
                ]));
            }
        }

        if !will_use_return {
            instructions.push(W::drop());
        }
        W::list(instructions)
    }

    fn translate_let_vars(&mut self, variable_list: &VariableList, var_type: VarType) -> Box<W> {
        use boa_ast::declaration::Binding;

        let var_name = self.current_function().add_local("$var", "anyref");

        let mut instructions = Vec::new();
        // TODO: handle hoisting
        for var in variable_list.as_ref() {
            match var.binding() {
                Binding::Identifier(identifier) => {
                    let offset = self.add_identifier(identifier);
                    if let Some(expression) = var.init() {
                        instructions.push(self.translate_expression(expression, true));
                    } else {
                        instructions.push(W::ref_null("any"));
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

        W::list(instructions)
    }

    fn translate_binary(&mut self, binary: &Binary) -> Box<W> {
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
                let lhs = self.translate_expression(binary.lhs(), true);
                let rhs = self.translate_expression(binary.rhs(), true);
                W::list(vec![lhs, rhs, W::call(func.to_string())])
            }
            BinaryOp::Bitwise(_bitwise_op) => todo!(),
            BinaryOp::Relational(relational_op) => {
                let func_name = match relational_op {
                    RelationalOp::Equal => todo!(),
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
                let rhs = self.current_function().add_local("$rhs", "anyref");
                let lhs = self.current_function().add_local("$lhs", "anyref");

                W::list(vec![
                    self.translate_expression(binary.lhs(), true),
                    W::local_set(&lhs),
                    self.translate_expression(binary.rhs(), true),
                    W::local_set(&rhs),
                    W::local_get(&lhs),
                    W::local_get(&rhs),
                    W::call(func_name),
                ])
            }
            BinaryOp::Logical(logical_op) => {
                let func_name = match logical_op {
                    LogicalOp::And => "$logical_and",
                    LogicalOp::Or => "$logical_or",
                    LogicalOp::Coalesce => "$logical_coalesce",
                };
                let rhs = self.current_function().add_local("$rhs", "anyref");
                let lhs = self.current_function().add_local("$lhs", "anyref");

                W::list(vec![
                    self.translate_expression(binary.lhs(), true),
                    W::local_set(&lhs),
                    self.translate_expression(binary.rhs(), true),
                    W::local_set(&rhs),
                    W::local_get(&lhs),
                    W::local_get(&rhs),
                    W::call(func_name),
                ])
            }
            BinaryOp::Comma => todo!(),
        }
    }

    fn translate_identifier(&mut self, identifier: &Identifier) -> Box<W> {
        let offset = self.add_identifier(identifier);

        if identifier.to_interned_string(&self.interner) == "undefined" {
            W::ref_null("any")
        } else {
            W::list(vec![
                W::local_get("$scope"),
                W::i32_const(offset),
                W::call("$get_variable".to_string()),
            ])
        }
    }

    fn translate_property_access(
        &mut self,
        property_access: &PropertyAccess,
        assign: Option<Box<W>>,
    ) -> Box<W> {
        use boa_ast::expression::access::PropertyAccessField;

        // println!("Property access: {:#?}", property_access);

        match property_access {
            PropertyAccess::Simple(simple_property_access) => {
                let target = self.translate_expression(simple_property_access.target(), true);
                // println!("TARGET: {target:#?}");
                match simple_property_access.field() {
                    PropertyAccessField::Const(sym) => {
                        let offset = self.add_symbol(*sym);

                        if let Some(assign_instruction) = assign {
                            let temp = self.current_function().add_local("$temp", "anyref");
                            W::list(vec![
                                assign_instruction,
                                W::local_set(&temp),
                                target,
                                W::i32_const(offset),
                                W::local_get(&temp),
                                W::call("$set_property"),
                            ])
                        } else {
                            W::list(vec![target, W::i32_const(offset), W::call("$get_property")])
                        }
                    }
                    PropertyAccessField::Expr(expression) => {
                        todo!()
                        // let expr_result_var =
                        //     self.current_function().add_local("$expr_result", "anyref");
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

    fn translate_expression(&mut self, expression: &Expression, will_use_return: bool) -> Box<W> {
        // println!(
        //     "translate expression ({will_use_return}) {} {expression:#?}",
        //     expression.to_interned_string(&self.interner)
        // );
        match expression {
            Expression::This => W::local_get("$this"),
            Expression::Identifier(identifier) => {
                let instr = self.translate_identifier(identifier);
                if !will_use_return {
                    W::list(vec![instr, W::drop()])
                } else {
                    instr
                }
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
            Expression::Call(call) => {
                self.translate_call(call, W::ref_null("any"), will_use_return)
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
            Expression::Conditional(_conditional) => todo!(),
            Expression::Await(await_expr) => self.translate_await_expression(await_expr),
            Expression::Yield(_) => todo!(),
            Expression::Parenthesized(parenthesized) => self.translate_parenthesized(parenthesized),
            _ => todo!(),
        }
    }

    fn translate_await_expression(&mut self, await_expression: &Await) -> Box<W> {
        // println!("AWAIT: {await_expression:#?}");
        todo!();
        W::empty()
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

    fn translate_async_function(&mut self, async_function: &AsyncFunction) -> Box<W> {
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
    ) -> Box<W> {
        // println!("array literal: {:#?}", array_literal);
        let var = self.current_function().add_local("$array_elem", "anyref");
        let array_var = self
            .current_function()
            .add_local("$array_var", "(ref $Array)");
        let array_data = self
            .current_function()
            .add_local("$array_data", "(ref $AnyrefArray)");
        let array = array_literal.as_ref();
        let mut instructions = vec![
            W::i32_const(array.len() as i32),
            W::call("$new_array"),
            W::local_set(&array_var),
            W::instruction(
                "struct.get",
                vec![
                    W::r#type("$Array"),
                    W::r#type("$array"),
                    W::local_get(&array_var),
                ],
            ),
            W::local_set(&array_data),
        ];

        for (i, item) in array.iter().enumerate() {
            let value = if let Some(expression) = item {
                self.translate_expression(expression, true)
            } else {
                W::ref_null("any")
            };

            instructions.push(value);
            instructions.push(W::local_set(&var));
            instructions.push(W::instruction(
                "array.set",
                vec![
                    W::r#type("$AnyrefArray"),
                    W::local_get(&array_data),
                    W::i32_const(i as i32),
                    W::local_get(&var),
                ],
            ))
        }

        if will_use_return {
            instructions.push(W::local_get(&array_var));
        }

        W::list(instructions)
    }

    fn translate_parenthesized(&mut self, parenthesized: &Parenthesized) -> Box<W> {
        // println!("parenthesized: {parenthesized:#?}");

        self.translate_expression(parenthesized.expression(), true)
    }

    fn translate_object_literal(
        &mut self,
        object_literal: &ObjectLiteral,
        will_use_return: bool,
    ) -> Box<W> {
        use boa_ast::property::{PropertyDefinition, PropertyName};

        let mut instructions = Vec::new();
        let new_instance = self
            .current_function()
            .add_local("$new_instance", "(ref $Object)");
        let temp = self.current_function().add_local("$temp", "anyref");

        instructions.push(W::call("$new_object"));
        instructions.push(W::local_set(&new_instance));

        for property in object_literal.properties() {
            let instr = match property {
                PropertyDefinition::IdentifierReference(identifier) => {
                    let offset = self.add_identifier(identifier);
                    W::list(vec![
                        self.translate_identifier(identifier),
                        W::local_set(&temp),
                        W::local_get(&new_instance),
                        W::i32_const(offset),
                        W::local_get(&temp),
                        W::call("$set_property"),
                    ])
                }
                PropertyDefinition::Property(property_name, expression) => match property_name {
                    PropertyName::Literal(sym) => {
                        let offset = self.add_symbol(*sym);
                        W::list(vec![
                            self.translate_expression(expression, true),
                            W::local_set(&temp),
                            W::local_get(&new_instance),
                            W::i32_const(offset),
                            W::local_get(&temp),
                            W::call("$set_property"),
                        ])
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
                            W::list(vec![
                                func_instr,
                                W::local_set(&temp),
                                W::local_get(&new_instance),
                                W::i32_const(offset),
                                W::local_get(&temp),
                                W::call("$set_property"),
                            ])
                        }
                        PropertyName::Computed(_) => todo!(),
                    }
                }
                PropertyDefinition::SpreadObject(_) => todo!(),
                PropertyDefinition::CoverInitializedName(_, _) => todo!(),
            };
            instructions.push(instr);
        }

        if will_use_return {
            instructions.push(W::local_get(&new_instance));
        }
        W::list(instructions)
    }

    fn translate_new(&mut self, new: &New) -> Box<W> {
        let new_instance = self
            .current_function()
            .add_local("$new_instance", "(ref $Object)");
        W::list(vec![
            W::call("$new_object"),
            W::local_set(&new_instance),
            self.translate_call(new.call(), W::local_get(&new_instance), true),
            W::local_get(&new_instance),
            W::call("$return_object_or"),
        ])
    }

    fn translate_arrow_function(&mut self, function: &ArrowFunction) -> Box<W> {
        self.translate_function_generic(function.name(), function.parameters(), function.body())
    }

    fn translate_update(&mut self, update: &Update) -> Box<W> {
        use boa_ast::expression::operator::update::UpdateOp;
        let identifier = match update.target() {
            UpdateTarget::Identifier(identifier) => identifier,
            UpdateTarget::PropertyAccess(_property_access) => todo!(),
        };
        let var = self.current_function().add_local("$var", "anyref");

        // TODO: figure out pre vs post behaviour
        let instruction = match update.op() {
            UpdateOp::IncrementPost => W::call("$increment_number"),
            UpdateOp::IncrementPre => W::call("$increment_number"),
            UpdateOp::DecrementPost => W::call("$decrement_number"),
            UpdateOp::DecrementPre => W::call("$decrement_number"),
        };
        let target = self.translate_identifier(identifier);
        let offset = self.add_identifier(identifier);
        let set_variable = W::list(vec![
            W::local_get("$scope".to_string()),
            W::i32_const(offset),
            W::local_get(&var),
            W::call("$set_variable"),
        ]);

        W::list(vec![target, instruction, W::local_set(&var), set_variable])
    }

    fn translate_assign(&mut self, assign: &Assign) -> Box<W> {
        use boa_ast::expression::operator::assign::AssignOp;
        use boa_ast::expression::operator::assign::AssignTarget;

        match assign.op() {
            AssignOp::Assign => {
                let rhs = self.translate_expression(assign.rhs(), true);
                match assign.lhs() {
                    AssignTarget::Identifier(identifier) => {
                        let offset = self.add_identifier(identifier);
                        // identifier.sym().get(),
                        let rhs_var = self.current_function().add_local("$rhs", "anyref");
                        W::list(vec![
                            rhs,
                            W::local_set(&rhs_var),
                            W::local_get("$scope".to_string()),
                            W::i32_const(offset),
                            W::local_get(&rhs_var),
                            W::call("$assign_variable".to_string()),
                        ])
                    }
                    AssignTarget::Access(property_access) => {
                        self.translate_property_access(property_access, Some(rhs))
                    }
                    AssignTarget::Pattern(_pattern) => todo!(),
                }
            }
            AssignOp::Add => {
                let rhs = self.translate_expression(assign.rhs(), true);
                match assign.lhs() {
                    AssignTarget::Identifier(identifier) => {
                        let offset = self.add_identifier(identifier);
                        // identifier.sym().get(),
                        let rhs_var = self.current_function().add_local("$rhs", "anyref");
                        W::list(vec![
                            rhs,
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
                        ])
                    }
                    AssignTarget::Access(property_access) => {
                        let rhs_var = self.current_function().add_local("$rhs", "anyref");
                        W::list(vec![
                            rhs,
                            W::local_set(&rhs_var),
                            self.translate_property_access(property_access, None),
                            W::local_get(&rhs_var),
                            W::call("$add"),
                            W::local_set(&rhs_var),
                            self.translate_property_access(
                                property_access,
                                Some(W::local_get(&rhs_var)),
                            ),
                        ])
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

    fn translate_unary(&mut self, unary: &Unary) -> Box<W> {
        use boa_ast::expression::operator::unary::UnaryOp;

        let target = self.translate_expression(unary.target(), true);
        match unary.op() {
            UnaryOp::Minus => todo!(),
            UnaryOp::Plus => todo!(),
            UnaryOp::Not => W::list(vec![target, W::call("$logical_not")]),
            UnaryOp::Tilde => todo!(),
            UnaryOp::TypeOf => W::list(vec![target, W::call("$type_of")]),
            UnaryOp::Delete => todo!(),
            UnaryOp::Void => todo!(),
        }
    }

    fn translate_literal(&mut self, lit: &Literal) -> Box<W> {
        // println!("translate_literal: {lit:#?}");
        match lit {
            Literal::Num(num) => W::list(vec![W::f64_const(*num), W::call("$new_number")]),
            Literal::String(s) => {
                let s = self.interner.resolve(*s).unwrap().to_string();
                let (offset, length) = self.insert_data_string(&s);

                W::list(vec![
                    W::i32_const(offset),
                    W::i32_const(length),
                    W::call("$new_static_string"),
                ])
            }
            Literal::Int(i) => W::list(vec![W::f64_const(*i as f64), W::call("$new_number")]),
            Literal::BigInt(_big_int) => todo!(),
            Literal::Bool(b) => W::list(vec![
                W::i32_const(if *b { 1 } else { 0 }),
                W::call("$new_boolean"),
            ]),
            Literal::Null => W::ref_i31(W::i32_const(2)),
            Literal::Undefined => W::ref_null("any"),
        }
    }

    fn translate_declaration(&mut self, declaration: &Declaration) -> Box<W> {
        // println!(
        //     "translate_declaration {}",
        //     declaration.to_interned_string(&self.interner)
        // );
        match declaration {
            Declaration::Function(decl) => {
                let declaration = self.translate_function(decl);
                // function declaration still needs to be added to the scope if function has a name
                // TODO: declared functions need to be hoisted
                if let Some(name) = decl.name() {
                    let offset = self.add_identifier(&name);
                    W::list(vec![
                        W::local_get("$scope"),
                        W::i32_const(offset),
                        declaration,
                        W::i32_const(VarType::Var.to_i32()),
                        W::call("$declare_variable".to_string()),
                    ])
                } else {
                    // TODO: if it's empty and not called right away I guess we can just ignore it?
                    declaration
                }
            }
            Declaration::Lexical(v) => self.translate_lexical(v),
            Declaration::Generator(_generator) => todo!(),
            Declaration::AsyncFunction(decl) => {
                let declaration = self.translate_async_function(decl);
                if let Some(name) = decl.name() {
                    let offset = self.add_identifier(&name);
                    W::list(vec![
                        W::local_get("$scope"),
                        W::i32_const(offset),
                        declaration,
                        W::i32_const(VarType::Var.to_i32()),
                        W::call("$declare_variable".to_string()),
                    ])
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

    fn translate_statement(&mut self, statement: &Statement) -> Box<W> {
        match statement {
            Statement::Block(block) => self.translate_block(block),
            Statement::Var(var_declaration) => self.translate_var(var_declaration),
            Statement::Empty => W::empty(),
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

    fn translate_catch(&mut self, catch: Option<&Catch>, finally: Option<&Finally>) -> Box<W> {
        use boa_ast::declaration::Binding;
        let catch_instr = if let Some(catch) = catch {
            let binding_instr = if let Some(binding) = catch.parameter() {
                match binding {
                    Binding::Identifier(identifier) => {
                        let temp = self.current_function().add_local("$temp", "anyref");
                        let offset = self.add_identifier(identifier);
                        W::list(vec![
                            W::local_set(&temp),
                            W::local_get("$scope"),
                            W::i32_const(offset),
                            W::local_get(&temp),
                            W::i32_const(VarType::Param.to_i32()),
                            W::call("$declare_variable"),
                        ])
                    }
                    Binding::Pattern(_) => todo!(),
                }
            } else {
                W::drop()
            };
            W::list(vec![binding_instr, self.translate_block(catch.block())])
        } else {
            W::empty()
        };
        let finally_instr = if let Some(finally) = finally {
            self.translate_block(finally.block())
        } else {
            W::empty()
        };
        // TODO: if catch throws an error this will not behave as it should.
        // we need to add another try inside, catch anything that happens
        // there, run finally and then rethrow
        W::catch("$JSException", W::list(vec![catch_instr, finally_instr]))
    }

    fn translate_try(&mut self, r#try: &Try) -> Box<W> {
        let block = r#try.block();
        let catch = r#try.catch();
        let finally = r#try.finally();
        let instr = self.translate_catch(catch, finally);

        W::r#try(self.translate_block(block), vec![instr], None)
    }

    fn translate_throw(&mut self, throw: &Throw) -> Box<W> {
        let target = self.translate_expression(throw.target(), true);
        W::list(vec![target, W::throw("$JSException")])
    }

    fn translate_if_statement(&mut self, if_statement: &If) -> Box<W> {
        W::list(vec![
            self.translate_expression(if_statement.cond(), true),
            W::call("$cast_ref_to_i32_bool"),
            W::r#if(
                None,
                vec![self.translate_statement(if_statement.body())],
                if_statement
                    .else_node()
                    .map(|e| vec![self.translate_statement(e)]),
            ),
        ])
    }

    fn translate_while_loop(&mut self, while_loop: &WhileLoop) -> Box<W> {
        let condition = self.translate_expression(while_loop.condition(), true);
        W::r#loop(
            "$while_loop".to_string(),
            vec![W::block(
                "$break",
                vec![
                    condition,
                    W::call("$cast_ref_to_i32_bool"),
                    W::i32_eqz(),
                    W::br_if("$break"),
                    self.translate_statement(while_loop.body()),
                    W::br("$while_loop"),
                ],
            )],
        )
    }

    fn translate_block(&mut self, block: &Block) -> Box<W> {
        self.enter_block();
        let block_instr = W::block(
            self.current_block_name(),
            block
                .statement_list()
                .statements()
                .iter()
                .map(|s| self.translate_statement_list_item(s))
                .collect(),
        );
        self.exit_block();

        block_instr
    }

    fn translate_statement_list_item(&mut self, statement: &StatementListItem) -> Box<W> {
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
        let instruction = self.translate_var(node);
        self.current_function().add_instruction(instruction);
        ControlFlow::Continue(())
    }

    fn visit_declaration(&mut self, node: &Declaration) -> ControlFlow<Self::BreakTy> {
        // println!(
        //     "visit_declaration: {}",
        //     node.to_interned_string(&self.interner)
        // );
        let instruction = self.translate_declaration(node);
        self.current_function().add_instruction(instruction);
        ControlFlow::Continue(())
    }

    fn visit_expression(&mut self, node: &Expression) -> ControlFlow<Self::BreakTy> {
        // println!(
        //     "visit_expression: {}",
        //     node.to_interned_string(&self.interner)
        // );
        let instruction = self.translate_expression(node, false);
        self.current_function().add_instruction(instruction);
        ControlFlow::Continue(())
    }

    fn visit_statement(&mut self, node: &'a Statement) -> ControlFlow<Self::BreakTy> {
        // println!(
        //     "visit_statement: {}",
        //     node.to_interned_string(&self.interner)
        // );
        let instruction = self.translate_statement(node);
        self.current_function().add_instruction(instruction);
        ControlFlow::Continue(())
    }

    fn visit_call(&mut self, node: &'a Call) -> ControlFlow<Self::BreakTy> {
        let instruction = self.translate_call(node, W::ref_null("any"), false);
        self.current_function().add_instruction(instruction);
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

    let init = translator.module.get_function_mut("init").unwrap();
    init.add_local_exact("$scope", "(ref $Scope)");

    init.body.push_front(W::list(vec![
        W::global_get("$scope"),
        W::instruction("ref.cast (ref $Scope)", vec![]),
        W::local_set("$scope"),
    ]));

    let module = translator.module.clone();
    let module = RemoveListsTransformer::new(module).transform();
    let module = TailCallTransformer::new(module).transform();

    // Generate the full WAT module
    let module = module.to_string();

    // Generate the full WAT module using the template
    let module = wat_template::generate_wat_template(
        translator.additional_functions(),
        module,
        &mut translator,
    );

    let jawsm_dir = std::env::var("JAWSM_DIR").unwrap_or(".".into());
    let mut f = File::create(Path::new(&jawsm_dir).join("wat/generated.wat")).unwrap();
    f.write_all(module.as_bytes()).unwrap();

    // println!("WAT modules generated successfully!");
    Ok(())
}
