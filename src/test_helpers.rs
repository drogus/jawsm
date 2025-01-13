use similar::{ChangeTag, TextDiff};
use tarnik_ast::WatFunction;

pub fn assert_functions_eq(expected: &WatFunction, actual: &WatFunction) {
    if expected == actual {
        return;
    }

    // Convert both functions to their WAT string representation
    let expected_str = expected.to_string();
    let actual_str = actual.to_string();

    // Create a diff
    let diff = TextDiff::from_lines(&expected_str, &actual_str);

    // Build a detailed diff message
    let mut diff_msg = String::from("\nFunction comparison failed! Diff:\n");

    for change in diff.iter_all_changes() {
        let prefix = match change.tag() {
            ChangeTag::Delete => "- ",
            ChangeTag::Insert => "+ ",
            ChangeTag::Equal => "  ",
        };
        diff_msg.push_str(&format!("{}{}", prefix, change));
    }

    assert!(false, "{}", diff_msg);
}

#[cfg(test)]
mod tests {
    use super::*;
    use tarnik_ast::{WasmType, WatInstruction as W};

    #[test]
    fn test_identical_functions() {
        let mut f1 = WatFunction::new("test");
        f1.add_param("$p1", &WasmType::I32);
        f1.add_instruction(W::I32Const(42));

        let mut f2 = WatFunction::new("test");
        f2.add_param("$p1", &WasmType::I32);
        f2.add_instruction(W::I32Const(42));

        assert_functions_eq(&f1, &f2); // Should not panic
    }

    #[test]
    #[should_panic(expected = "Function comparison failed!")]
    fn test_different_functions() {
        let mut f1 = WatFunction::new("test");
        f1.add_param("$p1", &WasmType::I32);
        f1.add_instruction(W::I32Const(42));

        let mut f2 = WatFunction::new("test");
        f2.add_param("$p1", &WasmType::I32);
        f2.add_instruction(W::I32Const(43)); // Different constant

        assert_functions_eq(&f1, &f2); // Should panic with diff
    }
}
