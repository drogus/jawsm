use crate::wat_ast::{WatFunction, WatInstruction, WatModule};

pub struct TailCallTransformer {
    module: WatModule,
}

impl TailCallTransformer {
    pub fn new(module: WatModule) -> Self {
        Self { module }
    }

    pub fn transform(mut self) -> WatModule {
        self._transform();
        // consume self and return the modified module
        std::mem::take(&mut self.module)
    }

    fn _transform(&mut self) {
        for function in self.module.functions.iter_mut() {
            transform_function(function);
        }
    }
}

fn transform_function(function: &mut WatFunction) {
    if function.results.is_empty() {
        return;
    }

    let mut i = 0;
    while i < function.body.len() {
        if function.body[i].is_return() && i > 0 && function.body[i - 1].is_call() {
            // replace the call instruction with nop
            let call = std::mem::replace(&mut function.body[i - 1], Box::new(WatInstruction::Nop));
            if let WatInstruction::Call { name } = *call {
                // replace return with return_call
                let _ =
                    std::mem::replace(&mut function.body[i], WatInstruction::r#return_call(name));
            }
        }
        i += 1;
    }
}
