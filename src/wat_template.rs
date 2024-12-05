use std::{
    collections::{BTreeMap, HashMap, HashSet},
    rc::Rc,
    sync::{
        atomic::{AtomicI32, Ordering},
        Arc, Mutex,
    },
};

use boa_interner::{Interner, JStrRef};
use tera::{from_value, to_value, Context, Function, Tera, Value};

use crate::WasmTranslator;

fn render_data_entries(data_entries: &HashMap<i32, String>) -> String {
    data_entries
        .iter()
        .map(|(offset, value)| {
            format!(
                "(data $d{offset} (i32.const {offset}) \"{value}\")\n",
                offset = offset,
                value = value
            )
        })
        .collect()
}

fn data(
    interner: Arc<Mutex<HashMap<String, i32>>>,
    last_data_entry_length: Arc<AtomicI32>,
) -> impl Function {
    Box::new(
        move |args: &HashMap<String, Value>| -> tera::Result<Value> {
            match args.get("str") {
                Some(v) => match from_value::<String>(v.clone()) {
                    Ok(v) => {
                        // if the value does not exist insert any i32, we will fix it on the second
                        // run
                        last_data_entry_length.store(v.len() as i32, Ordering::Relaxed);
                        let value = interner.lock().unwrap().entry(v).or_insert(0).to_string();
                        Ok(to_value(value).unwrap())
                    }
                    Err(_) => Err("str needs to be a string".into()),
                },
                None => Err("data function needs an str argument".into()),
            }
        },
    )
}

// this is another hack I'm using. I want to easily pass data length were needed,
// to I'm saving data length of last data operation and retrieving it here
fn data_length(last_data_entry_length: Arc<AtomicI32>) -> impl Function {
    Box::new(
        move |_args: &HashMap<String, Value>| -> tera::Result<Value> {
            let length = last_data_entry_length.load(Ordering::Relaxed).to_string();
            Ok(to_value(length).unwrap())
        },
    )
}

// TODO: using Tera's functions we could avoid defining data beforehand (like listing each string
// that will be needed for WAT code). If data definition rendering is split from the rest of the
// rendering there could be a function like data("am arbitrary string") that inserts the string
// into the interner and returns an index
pub fn generate_wat_template(
    additional_functions: impl Into<String>,
    init_code: impl Into<String>,
    translator: &mut WasmTranslator,
) -> String {
    // TODO: this is a hack, the new ast generates full module with `(module` start and end
    // so for now just remove first and last line
    let mut init_lines: Vec<String> = init_code
        .into()
        .split("\n")
        .skip(1)
        .map(|s| s.into())
        .collect();
    init_lines.pop();
    init_lines.pop();
    let init_code = init_lines.join("\n");

    let template = std::include_str!("wat/template.wat");
    let mut tera = Tera::default();
    tera.add_raw_template("module", template).unwrap();
    let mut context = Context::new();
    context.insert("init_code", &init_code);
    context.insert("data_entries", "");
    context.insert("free_memory_offset", "");
    context.insert("additional_functions", &additional_functions.into());
    let mapping = Arc::new(Mutex::new(HashMap::new()));

    let last_data_entry_length = Arc::new(AtomicI32::new(0));
    tera.register_function(
        "data",
        data(mapping.clone(), last_data_entry_length.clone()),
    );
    tera.register_function("data_length", data_length(last_data_entry_length));
    tera.render("module", &context).unwrap();
    // We're doing a hack here. For now I don't want to rewrite too much and
    // I can't pass interner to the `data` function, cause it's not Sync,
    // so I render the module once to gather all the strings from data() calls,
    // then I run them through the interner and run again to get actual proper
    // i32 values.
    let mut new_mapping = HashMap::new();
    let mut locked = mapping.lock().unwrap();
    for (s, _) in locked.iter() {
        let value = translator.add_string(s);
        new_mapping.insert(s.clone(), value);
    }
    *locked = new_mapping;
    context.insert(
        "data_entries",
        &render_data_entries(&translator.data_entries),
    );
    drop(locked);
    let offset = translator.data_offset + (4 - translator.data_offset % 4);
    context.insert("free_memory_offset", &offset.to_string());

    tera.render("module", &context).unwrap()
}
