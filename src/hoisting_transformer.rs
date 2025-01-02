use tarnik_ast::{
    cursor::InstructionsCursor, InstructionsList, WasmType, WatFunction, WatInstruction, WatModule,
};

use crate::VarType;

// This is a WAT code transformer that will rewrite `$declare_variable` calls for `var` type
// variables and function declarations to handle hoisting.
//
// In JavaScript hoisting means a `var` variable will be available in the entire function scope.
// Function declaration hoisting means that function definitions will be moved to the beginning of
// the scope. An example for vars:
//
//     console.log(foo);
//     if (false) {
//       while (false) {
//          var foo = 1;
//       }
//     }
//
//  `console.log` call will output `undefined` rather than throwing a ReferenceError, cause any var
//  declaration will be moved to the beginning of a function (or a global) scope. The code above
//  essentially becomes an equivalent of:
//
//     var foo;
//     console.log(foo);
//     if (false) {
//         while (false) {
//             foo = 1;
//         }
//     }
//
// Function hoisting mostly means moving declarations to the beginning, for example this code will
// properly call the `foo()` function:
//
//      foo();
//
//      function foo() {
//          console.log("It works!");
//      }
//
//  The HoistingTransformer works with the following principle:
//
//  1. Inspect each of the functions in the module
//  2. Traverse the function body and all of the blocks (loops, ifs etc)
//  3. Find calls to `$declare_variable` that are preceeded by i32 value equal to
//     `VarType::Var.to_i32()` or `VarType::Function.to_i32()`
//  4. For declarations of type `Var` it will:
//     a. grab the arguments of the `$declare_variable` call
//     b. replace the `$declare_variable` call with an equivalent `$assign_variable` call (the only
//        thing that differs in the signature is that `$assign_variable doesn't need the 4th
//        argument, ie variable type)
//     c. replace the value in original `$declare_variable` arguments with `ref.null any`, which
//        means `undefined`
//     d. add the modified declaration call and arguments to the list of declarations to be hoisted
//  5. For declaration of type `Function` it will:
//     a. remove the function declaration and arguments from the code
//     b. add the declaration to the list of declarations to be hoisted
//  6. After gathering all of the declarations the the transforms finds the beginning of the
//     current's function scope and adds all of the gathered declarations there
pub struct HoistingTransformer {
    module: WatModule,
}

impl HoistingTransformer {
    pub fn new(module: WatModule) -> Self {
        Self { module }
    }

    pub fn transform(mut self) -> WatModule {
        self._transform();
        // consume self and return the modified module
        std::mem::take(&mut self.module)
    }

    fn _transform(&mut self) {
        for function in self.module.functions.iter() {
            let cursor = self.module.cursor_for_function(&function.name);
            if let Some(mut cursor) = cursor {
                let mut declarations = gather_declarations(&mut cursor);
                if declarations.len() > 0 {
                    cursor.reset();
                    let set_scope_instr = Some(WatInstruction::LocalSet("$scope".to_string()));
                    while cursor.current_instruction() != set_scope_instr && cursor.next() {}

                    if cursor.current_instruction() == set_scope_instr {
                        // we need to go in reverse order, it doesn't really matter for vars,
                        // cause all of the vars are assigned `undefined` at declaration anyway,
                        // but it matters for function declarations
                        declarations.reverse();
                        for decl in declarations {
                            cursor.insert_after_current(decl);
                        }
                    }
                }
            }
        }
    }
}

fn gather_declarations(mut cursor: &mut InstructionsCursor) -> Vec<InstructionsList> {
    let mut result = Vec::new();
    while let Some(instr) = cursor.current_instruction() {
        if instr.is_block_type() {
            for mut c in cursor.enter_block() {
                result.append(&mut gather_declarations(&mut c));
            }
        } else {
            match instr {
                WatInstruction::Call(name) if name == "$declare_variable" => {
                    let arguments = cursor.get_call_arguments();

                    if let Some(args) = arguments {
                        if let WatInstruction::I32Const(var_type) = args[3][0] {
                            if var_type == VarType::Var.to_i32() {
                                // it's a variable. First, let's change the instruction to assign
                                // variable
                                let new_instructions = [
                                    &args[0],
                                    &args[1],
                                    &args[2],
                                    vec![WatInstruction::call("$assign_variable")].as_slice(),
                                ]
                                .concat();

                                let _ =
                                    cursor.replace_current_call_with_arguments(new_instructions);

                                // then push the declaration to the list of declarations
                                result.push(
                                    [
                                        &args[0],
                                        &args[1],
                                        vec![WatInstruction::ref_null_any()].as_slice(),
                                        &args[3],
                                        vec![WatInstruction::call("$declare_variable")].as_slice(),
                                    ]
                                    .concat(),
                                );
                            } else if var_type == VarType::Function.to_i32() {
                                // it's a function declaration, we need to get it to the beginning
                                // of the current function, so just remove it from here and add to
                                // the rest of hoisted declarations
                                let old = cursor.replace_current_call_with_arguments(Vec::new());

                                result.push(old.unwrap());
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        if !cursor.next() {
            break;
        }
    }

    result
}
