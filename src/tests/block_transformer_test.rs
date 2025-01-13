use crate::{WatFunction, WatInstruction as W, WatModule, WasmType};

#[test]
fn test_block_transformation() {
    // Create initial module and function
    let mut module = WatModule::new();
    let mut func = WatFunction::new("foo");
    
    // Add some params and locals to test state preservation
    func.add_param("$param1", WasmType::I32);
    func.add_local("$local1", WasmType::I32);
    
    // Create function body with a block that has br_if
    let instructions = vec![
        // Code before block
        W::I32Const(10),
        W::local_set("$local1"),
        
        // Block with br_if
        W::block("$test_block", Default::default(), vec![
            W::I32Const(5),
            W::I32Const(1),
            W::br_if("$test_block"),  // Should jump to block end if condition is true
            W::I32Const(20),          // Only executed if br_if condition was false
        ]),
        
        // Code after block
        W::local_get("$local1"),
        W::I32Add(),
    ];
    
    func.set_body(instructions);
    module.add_function(func);

    // Create transformer and transform
    let mut transformer = BlockTransformer::new(&mut module);
    transformer.transform();

    // Get the transformed functions
    let main_func = module.get_function("$foo").unwrap();
    let end_block_func = module.get_function("$block-test_block-end").unwrap();

    // Verify end block function signature
    assert_eq!(end_block_func.params.len(), 1);
    assert_eq!(
        end_block_func.params[0].1,
        WasmType::r#ref("$function-state-foo")
    );
    
    // Verify end block function has required locals
    assert!(end_block_func.locals.contains_key("$arguments"));
    assert!(end_block_func.locals.contains_key("$parentScope"));
    assert!(end_block_func.locals.contains_key("$this"));

    // Verify main function transformation
    let main_body = main_func.body.borrow();
    
    // Check if br_if was transformed to if with state save and end block call
    let contains_if = main_body.iter().any(|instr| {
        matches!(instr, W::If { .. })
    });
    assert!(contains_if, "br_if should be transformed to if");

    // Check if there's a call to end block function at the end
    let last_instructions: Vec<_> = main_body.iter().rev().take(2).collect();
    assert!(matches!(
        last_instructions[0],
        W::Call(name) if name == "$block-test_block-end"
    ));
}
