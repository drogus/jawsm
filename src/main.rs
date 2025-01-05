use std::{io::Write, process::exit};
use tarnik_ast::{Global, Nullable, WasmType, WatInstruction as W};

use anyhow::anyhow;
use boa_ast::{scope::Scope, visitor::VisitWith};
use boa_interner::Interner;
use boa_parser::{Parser, Source};
use jawsm::{
    async_functions_transformer::AsyncFunctionsTransformer,
    hoisting_transformer::HoistingTransformer, tail_call_transformer::TailCallTransformer,
    WasmTranslator,
};

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        return Err(anyhow!("Please provide a JavaScript file path"));
    }

    let js_path = &args[1];
    let output_path = args.get(2);

    let js_code = std::fs::read_to_string(js_path)?;
    let js_include = include_str!("js/prepend.js");
    let full = format!("{js_include}\n{js_code}");

    let mut interner = Interner::default();

    let mut parser = Parser::new(Source::from_bytes(&full));
    let ast = match parser.parse_script(&Scope::default(), &mut interner) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("SyntaxError: {e}");
            exit(1);
        }
    };

    let module = jawsm::wasm::generate_module();
    let mut translator = WasmTranslator::new(interner, module);
    // println!("{ast:#?}");
    ast.visit_with(&mut translator);
    // exit $init function
    translator.exit_function();

    let scope_type = tarnik_ast::WasmType::Ref("$Scope".into(), Nullable::False);
    let init = translator.module.get_function_mut("init").unwrap();
    init.add_local_exact("$scope", scope_type.clone());

    init.prepend_instructions(vec![
        W::global_get("$global_scope"),
        W::ref_cast(scope_type),
        W::local_set("$scope"),
    ]);

    let mut module = translator.module.clone();
    // TODO: it's weird to clone the module and pass it as mutable alongisde the translator. We
    // can just pass the translator and access the module through the translator
    AsyncFunctionsTransformer::new(&mut module, &mut translator).transform();
    // TailCallTransformer::new(module).transform();
    HoistingTransformer::new(&mut module).transform();

    // add data entries from the translator to the generated module
    // let mut sorted_entries: Vec<_> = translator.data_entries.into_iter().collect();
    // sorted_entries.sort_by_key(|(offset, _)| *offset);
    //
    // for (offset, value) in sorted_entries {
    //     if !module.data.iter().any(|(o, _)| *o as i32 == offset) {
    //         module.add_data_raw(offset as usize, value);
    //     }
    // }

    let data_str = generate_data_string(&module.data);
    let (offset, _) = module.add_data(data_str);

    // at the moment we don't have a way to inject stuff into WASM macro, so we change the right
    // global afterwards
    module.globals.insert(
        "$data_offsets_offset".to_string(),
        Global {
            name: "$data_offsets_offset".to_string(),
            ty: WasmType::I32,
            init: vec![tarnik_ast::WatInstruction::I32Const(offset as i32)],
            mutable: false,
        },
    );

    // std::fs::write("wat/generated.wat", module.to_string().as_bytes())?;

    let binary = wat::parse_str(module.to_string())?;

    match output_path {
        Some(path) => {
            std::fs::write(path, &binary)?;
        }
        None => {
            std::io::stdout().write_all(&binary)?;
        }
    }

    Ok(())
}
