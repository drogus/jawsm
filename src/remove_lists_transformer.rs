use crate::wat_ast::{WatFunction, WatInstruction, WatModule};

/// We have a `List` instruction in our WAT AST.
/// It dosn't correspond to any WASM instructions, it's just
/// used to make it easier to return multiple instruction as
/// a one instruction. This transformer rewrites the AST to get
/// rid of lists.
pub struct RemoveListsTransformer {
    module: WatModule,
}

impl RemoveListsTransformer {
    pub fn new(module: WatModule) -> Self {
        Self { module }
    }

    pub fn transform(mut self) -> WatModule {
        // This is quite inefficient - we check if there are any lists
        // in any of the functions and then we do a transform of those lists.
        // In case there were any lists nested in lists, we check and run
        // again if needed. A more optimal way would be to use recursion,
        // but at the moment I'm fine with slightly less efficient code
        while self.any_lists() {
            self._transform();
        }
        // consume self and return the modified module
        std::mem::take(&mut self.module)
    }

    fn _transform(&mut self) {
        for function in self.module.functions.iter_mut() {
            transform_function(function);
        }
    }

    fn any_lists(&self) -> bool {
        self.module
            .functions
            .iter()
            .any(|f| f.body.iter().any(|i| i.is_list()))
    }
}

fn transform_function(function: &mut WatFunction) {
    let mut i = 0;
    while i < function.body.len() {
        if function.body[i].is_list() {
            // replace list with nop
            let list = std::mem::replace(&mut function.body[i], Box::new(WatInstruction::Nop));

            if let WatInstruction::List { mut instructions } = *list {
                let n = instructions.len();
                while let Some(current) = instructions.pop() {
                    function.body.insert(i, current);
                }

                function.body.remove(i + n);
                // we added n instructions, but removed Nop
                i += n - 1;
            }
        } else {
            i += 1;
        }
    }
}
