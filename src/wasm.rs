use tarnik::wasm;
use tarnik_ast::WatModule;

pub fn generate_module() -> WatModule {
    wasm! {
        // Not sure where to put this info, but here is the mapping of WASM values
        // to JS values:
        //
        // null - undefined
        // 0i31 - false
        // 1i31 - true
        // 2i31 - null
        // 3i31 - empty array element
        #[import("wasi_snapshot_preview1", "fd_write")]
        fn write(fd: i32, iov_start: i32, iov_len: i32, nwritten: i32) -> i32;

        #[import("wasi_snapshot_preview1", "proc_exit")]
        fn proc_exit(result: i32);

        #[import("wasi:io/poll@0.2.1", "poll")]
        fn poll_many(a1: i32, a2: i32, a3: i32);

        #[import("wasi:clocks/monotonic-clock@0.2.1", "subscribe-duration")]
        fn subscribe_duration(a: i64) -> i32;

        #[export("memory")]
        memory!("memory", 1);

        type InternalExceptionType = fn(i32);
        type JSExceptionType = fn(anyref);
        type WriteType = fn(i32, i32, i32, i32) -> i32;

        tag!(InternalException, InternalExceptionType);
        tag!(JSException, JSExceptionType);

        static SAFE_INTEGER: f64 = 0x1FFFFFFFFFFFFF as f64;

        static TO_PRIMITIVE_NUMBER: i32 = 1;
        static TO_PRIMITIVE_STRING: i32 = 2;

        static PROPERTY_WRITABLE: i32     = 0b00000001;
        static PROPERTY_ENUMERABLE: i32   = 0b00000010;
        static PROPERTY_CONFIGURABLE: i32 = 0b00000100;
        static PROPERTY_IS_GETTER: i32    = 0b00001000;
        static PROPERTY_IS_SETTER: i32    = 0b00010000;
        // Private properties are checked before runtime, so I'm not even sure if
        // this is needed in the long run, but I decided to declare and use it for now,
        // and see how it goes
        static PROPERTY_IS_PRIVATE: i32   = 0b00100000;
        static PROPERTY_ANY: i32   = 0b11111111;

        static mut global_strict_mode: i32 = 0;


        type I32Array = [mut i32];
        struct String {
            data: mut I32Array,
            length: mut i32
        }
        type StringArray = [mut Nullable<String>];

        struct StaticString {
            offset: i32,
            length: i32
        }

        struct InternedString {
            value: String,
            offset: i32,
        }
        type InternedStringArray = [mut Nullable<InternedString>];

        struct Interner {
            entries: mut InternedStringArray,
            length: mut i32,
            current_offset: mut i32,
        }

        struct Symbol {
            description: Nullable<String>
        }

        struct SymbolsMapEntry {
            key: String,
            symbol: Symbol
        }
        type SymbolsMapEntries = [mut Nullable<SymbolsMapEntry>];
        struct SymbolsMap {
            entries: mut SymbolsMapEntries,
            length: mut i32
        }

        struct Property {
            value: mut anyref,
            flags: mut i32
        }
        type PropertiesArray = [Nullable<Property>];

        struct PropertyMapEntry {
            key: mut i32,
            value: mut Property
        }
        struct SymPropertyMapEntry {
            key: mut Symbol,
            value: mut Property
        }
        type PropertyEntriesArray = [mut Nullable<PropertyMapEntry>];
        type SymPropertyEntriesArray = [mut Nullable<SymPropertyMapEntry>];
        struct PropertyMap {
            entries: mut PropertyEntriesArray,
            sym_entries: mut SymPropertyEntriesArray,
            size: mut i32,
            sym_size: mut i32,
            interner: Interner,
        }

        static VARIABLE_CONST: i32     = 0b00000001;
        static VARIABLE_LET: i32       = 0b00000010;
        static VARIABLE_VAR: i32       = 0b00000100;
        static VARIABLE_PARAM: i32     = 0b00001000;
        static VARIABLE_FUNC: i32      = 0b00010000;

        struct Variable {
            value: mut anyref,
            flags: i32,
        }

        // VariableMap types
        struct VariableMapEntry {
            key: mut i32,
            value: mut Variable
        }
        type EntriesArray = [mut Nullable<VariableMapEntry>];
        struct VariableMap {
            entries: mut EntriesArray,
            size: mut i32
        }

        // Scope and variable management
        struct Scope {
            parent: mut Nullable<Scope>,
            variables: mut VariableMap,
        }

        struct FunctionMetadata {
            new_target: anyref,
        }

        // Function types
        type JSArgs = [mut anyref];
        // TODO: I would prefer meta to be of type Nullable<FunctionMetadata>, but passing null
        // of a certain type is not well supported in tarnik yet, so for now I'm going with anyref
        type JSFunc = fn(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref;

        struct Function {
            scope: mut Scope,
            func: mut JSFunc,
            this: mut anyref,
            properties: mut PropertyMap,
            own_prototype: mut anyref,
        }

        // struct Class {
        //     constructor: Nullable<JSFunc>,
        //
        // }

        struct AccessorMethod {
            get: mut Nullable<Function>,
            set: mut Nullable<Function>,
        }

        // Object types
        struct Object {
            properties: mut PropertyMap,
            own_prototype: mut anyref,
            value: mut anyref,
        }

        struct GlobalThis {
            properties: mut PropertyMap,
            own_prototype: Object,
        }

        struct Number {
            value: mut f64
        }

        struct BigInt {
            negative: i32,
            digits: I32Array,
        }

        type AnyrefArray = [mut anyref];

        struct Array {
            array: mut AnyrefArray,
            length: mut i32,
            properties: mut PropertyMap,
            own_prototype: mut anyref,
        }

        type PollableFunction = fn(scope: Scope, this: anyref) -> anyref;
        struct Pollable {
            id: i32,
            func: anyref
        }
        type PollablesArray = [mut Nullable<Pollable>];
        type FunctionArray = [mut Nullable<Function>];

        rec! {
            type PromisesArray = [mut Nullable<Promise>];
            struct Promise {
                properties: mut PropertyMap,
                success_result: mut anyref,
                error_result: mut anyref,
                then_callback: mut Nullable<Function>,
                catch_callback: mut Nullable<Function>,
                finally_callback: mut Nullable<Function>,
                resolved: mut i32,
                errored: mut i32,
                chained_promises: mut PromisesArray,
                own_prototype: mut anyref,
            }
        }

        struct AsyncGenerator {
            properties: mut PropertyMap,
            own_prototype: mut anyref,
            // TODO: not sure if this has to be a Function, maybe just a reference would be enough?
            next_callback: mut Function,
        }

        struct Generator {
            properties: mut PropertyMap,
            own_prototype: mut anyref,
            // TODO: not sure if this has to be a Function, maybe just a reference would be enough?
            next_callback: mut Function,
        }

        struct GeneratorResult {
            next_callback: Function,
            result: anyref,
            done: i32,
        }

        // Types for saving primitives on the stack
        type StackArray = [mut anyref];
        struct StackI32 { value: i32 }
        struct StackI64 { value: i64 }
        struct StackF32 { value: f32 }
        struct StackF64 { value: f64 }

        struct Thenable {
            // the object with .then method itself
            thenable: anyref,
            // callback function
            callback: Function,
            // reference to the function that was paused when running thenable
            // TODO: I don't think we need it with callbacks version, it was useful only for
            // asyncify version
            // caller: JSFunc,
            // TODO: again, I think it's not really needed here
            // result: anyref,
        }

        type Thenables = [mut Nullable<Thenable>];

        // TODO; allow to insert valus from outside the macro, for now it's fine to just use
        // something big enough to allow small scripts to run
        static mut free_memory_offset: i32 = 30000;
        static mut data_offsets_offset: i32 = 0;
        static mut global_scope: Nullable<Scope> = null;
        static mut promise_prototype: Nullable<Object> = null;
        static mut generator_prototype: Nullable<Object> = null;
        static mut async_generator_prototype: Nullable<Object> = null;
        static mut global_generator_constructor: Nullable<Function> = null;
        static mut global_async_generator_constructor: Nullable<Function> = null;
        static mut global_object_prototype: Nullable<Object> = null;
        static mut global_array_prototype: Nullable<Object> = null;
        static mut global_function_prototype: Nullable<Object> = null;
        static mut global_number_prototype: Nullable<Object> = null;
        static mut global_boolean_prototype: Nullable<Object> = null;
        static mut global_string_prototype: Nullable<Object> = null;
        static mut global_bigint_prototype: Nullable<Object> = null;
        static mut global_symbol_prototype: Nullable<Object> = null;
        static mut pollables: PollablesArray = [null; 2];
        static mut current_string_lookup: Nullable<String> = null;
        static mut global_this: Nullable<GlobalThis>  = null;
        static mut symbols: SymbolsMap = SymbolsMap {
            entries: [null; 10],
            length: 0
        };
        static mut symbol_iterator: Nullable<Symbol> = null;
        static mut symbol_async_iterator: Nullable<Symbol> = null;
        static mut symbol_to_primitive: Nullable<Symbol> = null;
        static mut symbol_has_instance: Nullable<Symbol> = null;
        static mut symbol_is_concat_spreadable: Nullable<Symbol> = null;
        static mut symbol_match: Nullable<Symbol> = null;
        static mut symbol_match_all: Nullable<Symbol> = null;
        static mut symbol_replace: Nullable<Symbol> = null;
        static mut symbol_search: Nullable<Symbol> = null;
        static mut symbol_species: Nullable<Symbol> = null;
        static mut symbol_split: Nullable<Symbol> = null;
        static mut symbol_to_string_tag: Nullable<Symbol> = null;
        static mut symbol_unscopables: Nullable<Symbol> = null;
        static mut thenables: Thenables = [null; 5];

        // Memory management functions required by the Component Model
        #[export("cabi_realloc")]
        fn cabi_realloc(old_ptr: i32, old_size: i32, align: i32, new_size: i32) -> i32 {
            // Simple bump allocator for this example
            let ptr: i32 = 1024;
            return ptr;
        }

        #[export("canonical_abi_free")]
        fn canonical_abi_free(ptr: i32, size: i32) {
            // TODO: implement something sensible here or find examples on how
            // it's usually done
        }

        fn create_function_metadata(new_target: anyref) -> FunctionMetadata {
            return FunctionMetadata {
                new_target: new_target
            };
        }

        fn enable_global_strict_mode() {
            global_strict_mode = 1;
        }

        fn push_thenable(callback: Function, thenable_obj: anyref) -> anyref {
            let len: i32 = len!(thenables);
            let mut i: i32 = 0;

            while i < len {
                if ref_test!(thenables[i], null) {
                    thenables[i] = Thenable {
                        thenable: thenable_obj,
                        callback: callback
                    };

                    return null;
                }
                i += 1;
            }

            // TODO: at the moment we don't extend the array if it's full. for now it's fine as
            // I'm creating the array with 100 elements, but eventually the initial array could be
            // smaller and then we extend it as needed
            return null;
        }

        fn process_thenables() {
            // log(create_arguments_1(create_string_from_array("process thenables")));
            let len: i32 = len!(thenables);
            let mut i: i32 = 0;

            while i < len {
                if !ref_test!(thenables[i], null) {
                    // log(create_arguments_1(create_string_from_array("found thenable")));
                    let thenable: Thenable = thenables[i] as Thenable;
                    thenables[i] = null;

                    set_then_callback(thenable.thenable, thenable.callback);
                    // the processed thenable could add more, so let's restart
                    // TODO: return_call should be used here, so we don't crash the stack
                    process_thenables();
                    return;
                }
                i += 1;
            }
        }

        fn new_pollable(id: i32, func: anyref) -> Pollable {
            return Pollable { id: id, func: func };
        }

        fn create_new_promise() -> Promise {
            return Promise {
                properties: create_propertymap(),
                success_result: null,
                error_result: null,
                then_callback: null,
                catch_callback: null,
                finally_callback: null,
                resolved: 0,
                errored: 0,
                chained_promises: [null; 1],
                own_prototype: null
            };
        }

        fn create_generator_prototype() -> Object {
            let object: Object = create_object();

            set_property(object, data!("next"),
                create_property_function(global_scope as Scope, Generator_next, null));

            set_property_sym(object, symbol_iterator as Symbol,
                create_property_function(global_scope as Scope, Generator_iterator, null));

            set_property(object, data!("constructor"),
                create_property_function(global_scope as Scope, Generator_constructor, null));

            return object;
        }

        fn create_async_generator_prototype() -> Object {
            let object: Object = create_object();

            set_property(object, data!("next"),
                create_property_function(global_scope as Scope, AsyncGenerator_next, null));

            set_property(object, data!("constructor"),
                create_property_function(global_scope as Scope, AsyncGenerator_constructor, null));

            return object;
        }

        fn create_promise_prototype() -> Object {
            let object: Object = create_object();

            set_property(object, data!("then"),
                create_property_function(global_scope as Scope, Promise_then, null));

            set_property(object, data!("catch"),
                create_property_function(global_scope as Scope, Promise_catch, null));

            set_property(object, data!("finally"),
                create_property_function(global_scope as Scope, Promise_finally, null));

            set_property(object, data!("toString"),
                create_property_function(global_scope as Scope, Promise_toString, null));

            set_property(object, data!("constructor"),
                create_property_function(global_scope as Scope, Promise_constructor, null));

            return object;
        }

        fn Promise_toString(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return new_static_string(data!("[object Promise]"), 16);
        }

        fn Promise_catch(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let new_promise: Promise = create_new_promise();
            new_promise.own_prototype = promise_prototype;
            let promise: Promise = this as Promise;
            let catch_args: JSArgs;

            // If no arguments, return empty promise
            if len!(arguments) == 0 {
                return new_promise;
            }

            // If we have an argument, pass it as second argument to then()
            if len!(arguments) > 0 {
                catch_args = create_arguments_2(null, arguments[0]);
                Promise_then(scope, new_promise, catch_args, null);
            }

            return new_promise;
        }

        fn create_global_this() -> GlobalThis {
            return GlobalThis {
                properties: create_propertymap(),
                own_prototype: global_object_prototype as Object
            };
        }

        fn create_object_prototype() -> Object {
            let object: Object = create_object();

            set_property(object, data!("toString"),
                create_property_function(global_scope as Scope, Object_toString, null));

            set_property(object, data!("constructor"),
                create_property_function(global_scope as Scope, Object_constructor, null));

            set_property(object, data!("hasOwnProperty"),
                create_property_function(global_scope as Scope, Object_hasOwnProperty, null));

            set_property(object, data!("valueOf"),
                create_property_function(global_scope as Scope, Object_valueOf, null));

            object.own_prototype = null;
            return object;
        }

        fn create_array_prototype() -> Object {
            let object: Object = create_object();

            set_property(object, data!("toString"),
                create_property_function(global_scope as Scope, Array_toString, null));

            set_property(object, data!("join"),
                create_property_function(global_scope as Scope, Array_join, null));

            set_property(object, data!("at"),
                create_property_function(global_scope as Scope, Array_at, null));

            set_property(object, data!("constructor"),
                create_property_function(global_scope as Scope, Array_constructor, null));

            // let length_getter: Function = new_function(global_scope as Scope, Array_length, null);
            // let length_setter: Function = new_function(global_scope as Scope, Array_set_length, null);
            // set_property(object, data!("length"),
            //     Property {
            //         value: AccessorMethod { get: length_getter, set: length_setter },
            //         flags: PROPERTY_IS_GETTER | PROPERTY_IS_SETTER | PROPERTY_WRITABLE
            //     });

            object.own_prototype = global_object_prototype as Object;
            return object;
        }

        fn create_known_symbols() {
            symbol_iterator = Symbol { description: create_string_from_array("Symbol.iterator") };
            symbol_async_iterator = Symbol { description: create_string_from_array("Symbol.asyncIterator") };
            symbol_to_primitive = Symbol { description: create_string_from_array("Symbol.toPrimitive") };
            symbol_has_instance = Symbol { description: create_string_from_array("Symbol.hasInstance") };
            symbol_is_concat_spreadable = Symbol { description: create_string_from_array("Symbol.isConcatSpreadable") };
            symbol_match = Symbol { description: create_string_from_array("Symbol.match") };
            symbol_match_all = Symbol { description: create_string_from_array("Symbol.matchAll") };
            symbol_replace = Symbol { description: create_string_from_array("Symbol.replace") };
            symbol_search = Symbol { description: create_string_from_array("Symbol.search") };
            symbol_species = Symbol { description: create_string_from_array("Symbol.species") };
            symbol_split = Symbol { description: create_string_from_array("Symbol.split") };
            symbol_to_string_tag = Symbol { description: create_string_from_array("Symbol.toStringTag") };
            symbol_unscopables = Symbol { description: create_string_from_array("Symbol.unscopables") };
        }

        fn create_symbol_prototype(constructor: Function) -> Object {
            let object: Object = create_object();

            set_property(object, data!("constructor"), create_bare_property(constructor));

            set_property(object, data!("toString"),
                create_property_function(global_scope as Scope, Symbol_toString, null));

            set_property(object, data!("valueOf"),
                create_property_function(global_scope as Scope, Symbol_valueOf, null));

            object.own_prototype = global_object_prototype as Object;
            return object;
        }

        fn setup_array_constructor(constructor: Function) {
            set_property(constructor, data!("prototype"), create_bare_property(global_array_prototype));

            set_property(constructor, data!("from"),
                create_property_function(global_scope as Scope, Array_from, null));

            set_property(constructor, data!("isArray"),
                create_property_function(global_scope as Scope, Array_isArray, null));
        }

        fn setup_symbol_constructor(symbol_constructor: Function) {
            create_known_symbols();
            set_property(symbol_constructor, data!("prototype"), create_bare_property(global_symbol_prototype));
            set_property(symbol_constructor, data!("iterator"), create_bare_property(symbol_iterator));
            set_property(symbol_constructor, data!("asyncIterator"), create_bare_property(symbol_async_iterator));
            set_property(symbol_constructor, data!("toPrimitive"), create_bare_property(symbol_to_primitive));
            set_property(symbol_constructor, data!("hasInstance"), create_bare_property(symbol_has_instance));
            set_property(symbol_constructor, data!("isConcatSpreadable"), create_bare_property(symbol_is_concat_spreadable));
            set_property(symbol_constructor, data!("matchAll"), create_bare_property(symbol_match_all));
            set_property(symbol_constructor, data!("replace"), create_bare_property(symbol_replace));
            set_property(symbol_constructor, data!("search"), create_bare_property(symbol_search));
            set_property(symbol_constructor, data!("species"), create_bare_property(symbol_species));
            set_property(symbol_constructor, data!("split"), create_bare_property(symbol_split));
            set_property(symbol_constructor, data!("toStringTag"), create_bare_property(symbol_to_string_tag));
            set_property(symbol_constructor, data!("unscopables"), create_bare_property(symbol_unscopables));
        }

        fn setup_object_constructor(constructor: Function) {
            set_property(constructor, data!("create"),
                create_property_function(global_scope as Scope, Object_create, null));

            set_property(constructor, data!("getPrototypeOf"),
                create_property_function(global_scope as Scope, Object_getPrototypeOf, null));

            set_property(constructor, data!("defineProperty"),
                create_property_function(global_scope as Scope, Object_defineProperty, null));

            set_property(constructor, data!("defineProperties"),
                create_property_function(global_scope as Scope, Object_defineProperties, null));

            set_property(constructor, data!("getOwnPropertyNames"),
                create_property_function(global_scope as Scope, Object_getOwnPropertyNames, null));

            set_property(constructor, data!("keys"),
                create_property_function(global_scope as Scope, Object_keys, null));

            set_property(constructor, data!("getOwnPropertyDescriptor"),
                create_property_function(global_scope as Scope, Object_getOwnPropertyDescriptor, null));
        }

        fn Object_getPrototypeOf(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return get_own_prototype(first_argument_or_null(arguments));
        }

        fn Object_toString(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return new_static_string(data!("[object Object]"), 15);
        }

        fn Array_isArray(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let result: i31ref;
            if ref_test!(first_argument_or_null(arguments), Array) {
                result = 1 as i31ref;
                return result;
            }

            result = 0 as i31ref;
            return result;
        }

        fn Array_from(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let array: anyref = first_argument_or_null(arguments);
            return array_from(array);
        }

        fn array_from(array: anyref) -> Array {
            if ref_test!(array, Array) {
                return clone_array(array as Array);
            } else if is_object(array) || ref_test!(array, String) || ref_test!(array, StaticString) {
                if ref_test!(array, StaticString) {
                    array = convert_static_string_to_string(array as StaticString);
                }

                let iterator_maybe: anyref = get_property_value_sym(array, symbol_iterator as Symbol);
                let mut new_array: AnyrefArray = [null; 1];
                let mut new_array_tmp: AnyrefArray;
                let mut i: i32 = 0;

                if ref_test!(iterator_maybe, Function) {
                    let mut iterator: anyref = call_function(iterator_maybe as Function, null, create_arguments_0(), null);
                    let mut result: anyref = null;
                    let mut value: anyref = null;
                    let mut done: anyref = null;

                    if !is_object(iterator) {
                        let error: anyref = create_error(data!("TypeError"), create_string_from_array("Result of the Symbol.iterator method is not an object"));
                        throw!(JSException, error);
                    }

                    let next_function: anyref = get_property_value(iterator, data!("next"));

                    if !ref_test!(next_function, Function) {
                        let error: anyref = create_error(data!("TypeError"), add_strings(ToString(next_function), create_string_from_array(" is not a function")));
                        throw!(JSException, error);
                    }

                    while 1 {
                        result = call_function(next_function as Function, null, create_arguments_0(), null);
                        if !is_object(result) {
                            let error: anyref = create_error(data!("TypeError"), create_string_from_array("Iterator result is not an object"));
                            throw!(JSException, error);
                        }
                        done = get_property_value(result, data!("done"));
                        if is_true(ToBoolean(done)) {
                            break;
                        }
                        value = get_property_value(result, data!("value"));

                        if i > len!(new_array) - 1 {
                            // expand array
                            new_array_tmp = [null; len!(new_array) * 2];
                            array_copy(AnyrefArray, AnyrefArray, new_array_tmp, 0, new_array, 0, len!(new_array));
                            new_array = new_array_tmp;
                        }
                        new_array[i] = value;
                        i += 1;
                    }

                    new_array_tmp = [null; i];
                    array_copy(AnyrefArray, AnyrefArray, new_array_tmp, 0, new_array, 0, len!(new_array_tmp));
                    return Array {
                        array: new_array_tmp,
                        length: len!(new_array_tmp),
                        properties: create_propertymap(),
                        own_prototype: global_array_prototype
                    };
                } else {
                    let length_maybe: anyref = get_property_value(array, data!("length"));
                    let length_num: Number = ToNumber(length_maybe);
                    let length: i32 = 0;
                    if is_nan(length_num) {
                        return create_empty_array();
                    }

                    if is_infinity(length_num) {
                        let error: anyref = create_error(data!("RangeError"), create_string_from_array("Invalid array length"));
                        throw!(JSException, error);
                    }

                    length = length_num.value as i32;
                    new_array = [null; length];

                    while i < length {
                        new_array[i] = get_property_value_str(array, NumberToString(new_number(i as f64), 10));
                        i+= 1;
                    }

                    return Array {
                        array: new_array,
                        length: len!(new_array),
                        properties: create_propertymap(),
                        own_prototype: global_array_prototype
                    };
                }
            }

            return create_empty_array();
        }

        fn clone_array(array: Array) -> Array {
            let new_array: AnyrefArray = [null; array.length];

            array_copy(AnyrefArray, AnyrefArray, new_array, 0, array.array, 0, array.length);

            return Array {
                array: new_array,
                length: array.length,
                properties: create_propertymap(),
                own_prototype: global_array_prototype
            };
        }

        fn create_empty_array() -> Array {
            return Array {
                array: [null; 0],
                length: 0,
                properties: create_propertymap(),
                own_prototype: global_array_prototype
            };

        }

        fn Array_at(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let array: Array = array_from(this);
            let index_num: Number = ToInteger(first_argument_or_null(arguments));
            if is_infinity(index_num) {
                return null;
            }

            let index: i32 = index_num.value as i32;
            if index < 0 {
                index = array.length + index;
                if index < 0 {
                    return null;
                }
            }

            if index > array.length - 1 {
                return null;
            }

            let array_data: AnyrefArray = array.array;
            return array_data[index];
        }

        fn Array_join(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let array: Array = create_empty_array();
            if ref_test!(this, Array) {
                array = this as Array;
            } else {
                array = array_from(this);
            }

            let maybe_separator: anyref = first_argument_or_null(arguments);
            let separator: String = create_empty_string();
            if ref_test!(maybe_separator, null) {
                separator = create_string_from_array(",");
            } else {
                separator = ToString(maybe_separator);
            }
            let separator_length: i32 = separator.length;
            let separator_data: I32Array = separator.data;
            let length: i32 = array.length;
            let elements: AnyrefArray = array.array;
            let strings: StringArray = [null; array.length];
            let new_data: I32Array;
            let mut i: i32 = 0;
            let mut j: i32 = 0;
            let mut k: i32 = 0;
            let mut str: anyref = create_empty_string();
            let mut element: anyref;
            let mut result_length: i32 = 0;
            let mut str: String;
            let mut str_length: i32;
            let mut str_data: I32Array;

            // TODO: this should delegate to join() in the future
            while i < length {
                element = elements[i];
                if is_empty(element) {
                    str = create_empty_string();
                } else {
                    str = ToString(element);
                }
                strings[i] = str;
                result_length += str.length;
                i += 1;
            }

            // separators
            if length > 0 {
                result_length += (length - 1) * separator_length;
            }
            new_data = [0; result_length];

            i = 0;
            // i - strings index
            // j - str_data index
            // k - result string index
            while i < length {
                j = 0;
                str = strings[i] as String;
                str_length = str.length;
                str_data = str.data;
                while j < str_length {
                    new_data[k] = str_data[j];
                    j += 1;
                    k += 1;
                }

                j = 0;
                if i != length - 1 {
                    while j < separator_length {
                        new_data[k] = separator_data[j];
                        k += 1;
                        j += 1;
                    }
                }

                i += 1;
            }

            return String {
                data: new_data,
                length: result_length,
            };
        }

        fn Array_toString(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return Array_join(scope, this, create_arguments_1(create_string_from_array(",")), meta);
        }

        fn JAWSM_ToString(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return ToString(first_argument_or_null(arguments));
        }

        fn ToString(target: anyref) -> String {
            if ref_test!(target, String) {
                return target as String;
            } else if ref_test!(target, StaticString) {
                return convert_static_string_to_string(target as StaticString);
            } else if ref_test!(target, null) {
                return create_string_from_array("undefined");
            } else if is_primitive(target) {
                if ref_test!(target, BigInt) {
                    return BigIntToString(target as BigInt, 10);
                } else if ref_test!(target, Number) {
                    return NumberToString(target as Number, 10);
                } else if ref_test!(target, i31ref) {
                    let v: i32 = target as i31ref as i32;

                    if v == 0 {
                        return create_string_from_array("false");
                    } else if v == 1 {
                        return create_string_from_array("true");
                    } else if v == 2 {
                        return create_string_from_array("null");
                    } else if v == 3 {
                        return create_string_from_array("empty");
                    }
                }
            } else {
                let result: anyref = ToPrimitive(target, TO_PRIMITIVE_STRING);

                if !is_object(result) {
                    return ToString(result);
                }

                let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot convert object to primitive value"));
                throw!(JSException, error);
            }

            let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot convert object to primitive value"));
            throw!(JSException, error);
        }

        fn NumberToString(number: Number, radix: i32) -> String {
            let written_length: i32 = number_to_string_memory(number.value, radix, free_memory_offset);
            return memory_to_string(free_memory_offset, written_length);
        }

        fn ToBigInt(target: anyref) -> BigInt {
            if ref_test!(target, BigInt) {
                return target as BigInt;
            }

            let str: String = string_trim(ToString(target));
            let str_data: I32Array = str.data;
            let str_len: i32 = len!(str_data);
            let mut valid: i32 = 1;
            let mut i: i32 = 0;
            let mut negative: i32 = 0;
            let mut base: i32 = 10;
            let mut c: i32;
            let digits_start: i32 = 0;
            let result: I32Array;

            if str_len == 0 {
                return BigInt {
                    negative: 0,
                    digits: [0; 0],
                };
            }

            if str_data[i] == '-' {
                i = 1;
                negative = 1;
            }

            if str_len - i >= 3 && str_data[i] == '0' {
                if str_data[i+1] == 'x' || str_data[i+1] == 'X' {
                    base = 16;
                    i += 2;
                }

                if str_data[i+1] == 'o' || str_data[i+1] == 'O' {
                    base = 8;
                    i += 2;
                }

                if str_data[i+1] == 'b' || str_data[i+1] == 'B' {
                    base = 2;
                    i += 2;
                }
            }

            digits_start = i;
            if str_len - i > 0 {
                while i < str_len {
                    c = str_data[i];
                    if c < '0' || c > '9' {
                        valid = 0;
                        break;
                    }

                    i += 1;
                }
            } else {
                valid = 0;
            }

            if !valid {
                let error: anyref = create_error(data!("SyntaxError"), add_strings(add_strings(create_string_from_array("Cannot convert "), str), create_string_from_array(" to a BigInt")));
                throw!(JSException, error);
            }

            i = digits_start;
            result = [0; str_len - i];
            while i < str_len {
                result[i - digits_start] = str_data[i];
                i += 1;
            }

            return bigint_from_digits(result, base, negative);
        }

        fn bigint_from_digits(digits: I32Array, base_from: i32, negative: i32) -> BigInt {
            let bigint_digits: I32Array = digits_to_bigint_array(digits, base_from);
            return BigInt {
                negative: negative,
                digits: bigint_digits,
            };
        }

        // This is a simplified version of an algorithm that V8 uses
        fn digits_to_bigint_array(digits: I32Array, base_from: i32) -> I32Array {
            let bigint: I32Array = [0; len!(digits)];
            let bigint_length: i32 = 0;
            // it looks like if we use anything bigger than 2^24 we won't fit in the available
            // types and overflow. I could use i64 for calculations before filling in I32Array,
            // but I alo would like to leave some buffer for computation for addition,
            // multiplication etc.
            let BASE: i32 = 0x1000000; // 2^24
            let mut carry: i32;
            let mut i: i32;
            let mut temp: i32;
            let mut add_carry: i32;
            let mut total: i32;
            let result: I32Array;
            let mut d: i32;
            let mut di: i32 = 0;
            let digits_length: i32 = len!(digits);

            while di < digits_length {
                d = digits[di];
                carry = 0;
                i = 0;
                while i < bigint_length {
                    temp = bigint[i] * base_from + carry;
                    bigint[i] = temp % BASE;
                    carry = temp/BASE;
                    i += 1;
                }
                if carry > 0 {
                    bigint[bigint_length] = carry;
                    bigint_length += 1;
                }

                add_carry = d;
                i = 0;
                while add_carry > 0 && i < bigint_length {
                    total = bigint[i] + add_carry;
                    bigint[i] = total % BASE;
                    add_carry = total / BASE;
                    i += 1;
                }
                if add_carry > 0 {
                    bigint[bigint_length] = add_carry;
                    bigint_length += 1;
                }

                di += 1;
            }

            result = [0; bigint_length];

            i = 0;
            while i < bigint_length {
                result[i] = bigint[i];
                i += 1;
            }

            return result;
        }

        fn reverse_i32_array(array: I32Array) -> I32Array {
            let len: i32 = len!(array);
            let reversed: I32Array = [0; len];
            let mut i: i32 = 0;
            let mut j: i32 = len - 1;
            while i < len {
                reversed[i] = array[j];

                i += 1;
                j -= 1;
            }

            return reversed;
        }

        fn remove_trailing_zeroes_i32_array(array: I32Array) -> I32Array {
            let len: i32 = len!(array);
            let mut i: i32 = len - 1;
            let mut j: i32 = 0;
            let result: I32Array;

            while array[i] == 0 {
                i -= 1;
                if i < 0 {
                    break;
                }
            }

            if i < len - 1 {
                // at least one trailing zero
                result = [0; i + 1];
                while j < i + 1 {
                    result[j] = array[j];
                    j += 1;
                }
                return result;
            }

            return array;
        }

        fn expand_i32_array(array: I32Array, new_capacity: i32) -> I32Array {
            let result: I32Array = [0; new_capacity];
            let len: i32 = len!(array);
            let i: i32 = 0;
            while i < len {
                result[i] = array[i];
                i += 1;
            }

            return result;
        }

        fn BigIntToString(bigint: BigInt, base: i32) -> String {
            let digits: I32Array = from_bigint_to_digits(bigint.digits, base);
            let len: i32 = len!(digits);
            let chars: I32Array = [0; len];
            let mut i: i32 = 0;

            while i < len {
                chars[i] = digits[i] + 48;
                i += 1;
            }

            let result: String = String {
                data: chars,
                length: len
            };

            if bigint.negative {
                result = add_strings(create_string_from_array("-"), result);
            }

            return result;
        }

        fn from_bigint_to_digits(bigint: I32Array, base: i32) -> I32Array {
            let mut digits: I32Array = [0; len!(bigint)];
            let mut di: i32 = 0;
            let mut quotient_digits: I32Array;
            let mut carry: i32 = 0;
            let mut i: i32;
            let mut j: i32;
            let mut current: i32;
            let mut new_digit: i32;

            while len!(bigint) > 0 {
                carry = 0;
                quotient_digits = [0; len!(bigint)];
                i = len!(bigint) - 1;
                j = 0;
                while i >= 0 {
                    current = bigint[i] + carry * 0x1000000;
                    new_digit = current / base;
                    carry = current % base;
                    quotient_digits[j] = new_digit;
                    i -= 1;
                    j += 1;
                }
                quotient_digits = reverse_i32_array(quotient_digits);
                quotient_digits = remove_trailing_zeroes_i32_array(quotient_digits);
                bigint = quotient_digits;
                if di == len!(digits) {
                    digits = expand_i32_array(digits, len!(digits) * 2);
                }
                digits[di] = carry;
                di += 1;
            }

            return reverse_i32_array(remove_trailing_zeroes_i32_array(digits));
        }

        fn number_to_string_memory(value: f64, radix: i32, offset: i32) -> i32 {
            let mut current_offset: i32 = offset;

            // Handle NaN
            if value != value {
                memory::<i8>[current_offset] = 'N';
                memory::<i8>[current_offset + 1] = 'a';
                memory::<i8>[current_offset + 2] = 'N';
                return 3;
            }

            // Handle Infinity
            if value == f64::INFINITY {
                memory::<i8>[current_offset] = 'I';
                memory::<i8>[current_offset + 1] = 'n';
                memory::<i8>[current_offset + 2] = 'f';
                memory::<i8>[current_offset + 3] = 'i';
                memory::<i8>[current_offset + 4] = 'n';
                memory::<i8>[current_offset + 5] = 'i';
                memory::<i8>[current_offset + 6] = 't';
                memory::<i8>[current_offset + 7] = 'y';
                return 8;
            } else if value == f64::NEG_INFINITY {
                memory::<i8>[current_offset] = '-';
                current_offset += 1;
                memory::<i8>[current_offset] = 'I';
                memory::<i8>[current_offset + 1] = 'n';
                memory::<i8>[current_offset + 2] = 'f';
                memory::<i8>[current_offset + 3] = 'i';
                memory::<i8>[current_offset + 4] = 'n';
                memory::<i8>[current_offset + 5] = 'i';
                memory::<i8>[current_offset + 6] = 't';
                memory::<i8>[current_offset + 7] = 'y';
                return 9;
            }

            if value == 0.0 || value == -0.0 {
                memory::<i8>[current_offset] = '0';
                return 1;
            }

            // Validate radix
            if radix < 2 || radix > 36 {
                // TODO: throw RangeError
                return 0;
            }

            // Use specal function for 10 base, that support scientific notation
            if radix == 10 {
                return writeF64AsAscii(value, offset);
            }

            // Determine sign and absolute value
            let is_negative: i32;
            if value < 0.0 {
                is_negative = 1;
            } else {
                is_negative = 0;
            }
            let mut abs_value: f64;
            if is_negative != 0 {
                abs_value = -value;
            } else {
                abs_value = value;
            }

            // Split into integer and fractional parts
            let integer_part: f64 = trunc!(abs_value);
            let fractional_part: f64 = abs_value - integer_part;
            let mut digit: i32;
            let mut divisor: f64;
            let mut divided: f64;
            let mut truncated: f64;
            let mut remainder: f64;

            // Convert integer part to digits in reverse order
            let mut int_digits: I32Array = [0; 64];
            let mut int_count: i32 = 0;
            let mut int_val: f64 = integer_part;

            if int_val == 0.0 {
                int_digits[0] = 0;
                int_count = 1;
            } else {
                while int_val > 0.0 && int_count < 64 {
                    divisor = radix as f64;
                    divided = int_val / divisor;
                    truncated = trunc!(divided);
                    remainder = int_val - truncated * divisor;
                    digit = remainder as i32;
                    int_digits[int_count] = digit;
                    int_count += 1;
                    int_val = truncated;
                }
            }

            // Convert fractional part to digits
            let mut frac_digits: I32Array = [0; 20];
            let mut frac_count: i32 = 0;
            let mut frac_val: f64 = fractional_part;

            while frac_val > 0.0 && frac_count < 20 {
                frac_val = frac_val * radix as f64;
                digit = trunc!(frac_val) as i32;
                frac_digits[frac_count] = digit;
                frac_count += 1;
                frac_val -= digit as f64;
            }

            // Write sign if negative
            if is_negative != 0 {
                memory::<i8>[current_offset] = '-';
                current_offset += 1;
            }

            let mut c: i32;

            // Write integer part in reverse order
            let mut i: i32 = int_count - 1;
            while i >= 0 {
                digit = int_digits[i];
                if digit < 10 {
                    c = '0' + digit;
                } else {
                    c = 'a' + (digit - 10);
                }
                memory::<i8>[current_offset] = c;
                current_offset += 1;
                i -= 1;
            }

            // Write fractional part if any
            if frac_count > 0 {
                memory::<i8>[current_offset] = '.';
                current_offset += 1;

                i = 0;
                while i < frac_count {
                    digit = frac_digits[i];
                    if digit < 10 {
                        c = '0'+ digit;
                    } else {
                        c = 'a' + (digit - 10);
                    }
                    memory::<i8>[current_offset] = c;
                    current_offset += 1;
                    i += 1;
                }

                // Trim trailing zeros
                while current_offset > offset && memory::<i8>[current_offset - 1] == '0' {
                    current_offset -= 1;
                }

                // Remove decimal point if no fractional digits left
                if current_offset > offset && memory::<i8>[current_offset - 1] == '.' {
                    current_offset -= 1;
                }
            }

            return current_offset - offset;
        }

        fn Object_create_simple(value: anyref) -> anyref {
            return Object_create(global_scope as Scope, null, create_arguments_1(value), null);
        }

        fn Object_create(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let object: Object = create_object();

            if len!(arguments) == 0 {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Object.create: At least 1 argument required, but only 0 passed")));
            }

            let prototype: anyref = arguments[0];

            if !is_null(prototype) && !is_object(prototype) {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Object.create: first argument has to be either null or an object")));
            }

            object.own_prototype = prototype;

            let properties: anyref = second_argument_or_null(arguments);

            Object_defineProperties(scope, null, create_arguments_2(object, properties), null);

            return object;
        }

        fn Object_keys(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let strings: StringArray = get_own_property_names(first_argument_or_null(arguments));
            let result: Array = create_array(len!(strings));
            let result_array: AnyrefArray = result.array;
            let length: i32 = len!(strings);
            let mut i: i32 = 0;
            while i < length {
                result_array[i] = strings[i];
                i+=1;
            }

            return result;
        }

        fn Object_getOwnPropertyDescriptor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            // right now it's the same as Object.keys(), but it should differ for arrays
            let obj: anyref = ToObject(first_argument_or_null(arguments));
            let key: anyref = ToPropertyKey(second_argument_or_null(arguments));
            let maybe_property: anyref;
            let property: Property;

            if ref_test!(key, Symbol) {
                maybe_property = get_own_property_sym(obj, key as Symbol);
            } else {
                maybe_property = get_own_property_str(obj, key);
            }

            if ref_test!(maybe_property, null) {
                return null;
            }

            property = maybe_property as Property;
            let result: Object = create_object();
            let configurable: i31ref = 0 as i31ref;
            let writable: i31ref = 0 as i31ref;
            let enumerable: i31ref = 0 as i31ref;
            let value: anyref = property.value;

            if ref_test!(value, AccessorMethod) {
                set_property_value(result, data!("get"), (value as AccessorMethod).get);
                set_property_value(result, data!("set"), (value as AccessorMethod).set);
            } else {
                set_property_value(result, data!("value"), value);
            }

            if property.flags & PROPERTY_CONFIGURABLE != 0 {
                configurable = 1 as i31ref;
            }

            if property.flags & PROPERTY_WRITABLE != 0 {
                writable = 1 as i31ref;
            }

            if property.flags & PROPERTY_ENUMERABLE != 0 {
                enumerable = 1 as i31ref;
            }

            set_property_value(result, data!("configurable"), configurable);
            set_property_value(result, data!("writable"), writable);
            set_property_value(result, data!("enumerable"), enumerable);

            return result;
        }

        fn Object_getOwnPropertyNames(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            // right now it's the same as Object.keys(), but it should differ for arrays
            let strings: StringArray = get_own_property_names(first_argument_or_null(arguments));
            let result: Array = create_array(len!(strings));
            let result_array: AnyrefArray = result.array;
            let length: i32 = len!(strings);
            let mut i: i32 = 0;
            while i < length {
                result_array[i] = strings[i];
                i+=1;
            }

            return result;
        }

        fn Object_hasOwnProperty(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let value: anyref = this;
            let result: i31ref;
            let property: Nullable<Property>;

            if is_primitive(value) {
                result = 0 as i31ref;
                return result;
            }

            let prop_name: anyref = first_argument_or_null(arguments);

            if ref_test!(prop_name, Symbol) {
                property = get_own_property_sym(value, prop_name as Symbol);
            } else {
                property = get_own_property_str(value, ToString(prop_name));
            }

            if ref_test!(property, null) {
                result = 0 as i31ref;
                return result;
            }

            result = 1 as i31ref;
            return result;
        }

        fn create_error(constructor_offset: i32, message: String) -> Object {
            let constructor: Function = get_variable(global_scope as Scope, constructor_offset) as Function;
            let new_instance: Object = create_object();
            call_function(constructor, new_instance, create_arguments_1(message), null);

            new_instance.own_prototype = get_property_value(constructor, data!("prototype"));
            set_property_value(new_instance, data!("constructor"), constructor);

            return new_instance;
        }

        fn Object_defineProperties(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let object: anyref = first_argument_or_null(arguments);
            let properties: anyref = second_argument_or_null(arguments);

            if is_primitive(object) {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Object.defineProperties called on non-object")));
            }

            let property_map_maybe: Nullable<PropertyMap> = get_propertymap(properties);

            if ref_test!(property_map_maybe, null) {
                return object;
            }

            let property_map: PropertyMap = property_map_maybe as PropertyMap;
            let size: i32 = property_map.size;
            let mut i: i32 = 0;
            let mut entry: Nullable<PropertyMapEntry>;
            let entries: PropertyEntriesArray = property_map.entries;

            while i < size {
                entry = entries[i] as PropertyMapEntry;
                Object_defineProperty(scope, this, create_arguments_3(object, get_property_name(property_map as PropertyMap, entry.key), get_value_of_property(entry.value, properties)), null);
                i += 1;
            }

            return object;
        }

        fn Object_defineProperty(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let args_len: i32 = len!(arguments);

            if args_len == 0 {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Object.defineProperty called on non-object")));
            }

            if args_len < 3 {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Property descriptor must be an object")));
            } else if !ref_test!(arguments[2], Object) {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Property descriptor must be an object")));
            }

            let target: anyref = arguments[0];

            if is_primitive(target) {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Object.defineProprty called on non-object")));
            }

            let existing_property: Nullable<Property>;
            if ref_test!(arguments[1], Symbol) {
                existing_property = get_own_property_sym(target, arguments[1] as Symbol);
            } else {
                existing_property = get_own_property_str(target, arguments[1]);
            }
            if !ref_test!(existing_property, null) {
                if (existing_property as Property).flags & PROPERTY_CONFIGURABLE == 0 {
                    throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Cannot redefine non-reconfigurable property")));
                }
            }

            let descriptor: Object = arguments[2] as Object;
            let get_or_set: i32 = !ref_test!(get_property(descriptor, data!("get")), null) ||
                                  !ref_test!(get_property(descriptor, data!("set")), null);

            if (!ref_test!(get_property(descriptor, data!("writable")), null) ||
                !ref_test!(get_property(descriptor, data!("value")), null)) &&
                get_or_set {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Invalid property descriptor. Cannot both specify accessors and a value or writable attribute")));
            }

            let value: anyref = null;
            let flags: i32 = 0;

            if get_or_set {
                let getter: anyref = get_property_value(descriptor, data!("get"));
                let setter: anyref = get_property_value(descriptor, data!("set"));

                // if either a getter or a setter are not undefined and are not a function raise an error
                if !ref_test!(getter, null) && !ref_test!(getter, Function) {
                    throw!(JSException, create_error(data!("TypeError"), create_string_from_array("getter must be a function")));
                }
                if !ref_test!(setter, null) && !ref_test!(setter, Function) {
                    throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Setter must be a function")));
                }

                let accessor: AccessorMethod = AccessorMethod { get: null, set: null };

                if ref_test!(getter, Function) {
                    accessor.get = getter as Function;
                    flags = flags | PROPERTY_IS_GETTER;
                }
                if ref_test!(setter, Function) {
                    accessor.set = setter as Function;
                    flags = flags | PROPERTY_IS_SETTER;
                }

                value = accessor;
            } else {
                value = get_property_value(descriptor, data!("value"));
            }

            let property: Property = Property {
                value: value,
                flags: flags
            };

            if js_is_true(get_property_value(descriptor, data!("configurable"))) {
                property.flags = property.flags | PROPERTY_CONFIGURABLE;
            }

            if js_is_true(get_property_value(descriptor, data!("enumerable"))) {
                property.flags = property.flags | PROPERTY_ENUMERABLE;
            }

            if js_is_true(get_property_value(descriptor, data!("writable"))) {
                property.flags = property.flags | PROPERTY_WRITABLE;
            }

            if ref_test!(arguments[1], Symbol) {
                set_property_sym(target, arguments[1] as Symbol, property);
            } else {
                set_property_str(target, arguments[1], property);
            }

            return target;
        }

        fn get_super(this: anyref) -> anyref {
            let constructor: anyref = get_property_value(this, data!("constructor"));
            let own_prototype: anyref = get_own_prototype(constructor);
            let prototype: anyref = get_property_value(own_prototype, data!("prototype"));
            return prototype;
        }

        fn js_is_true(value: anyref) -> i32 {
            if ref_test!(value, null) {
                return 0;
            } else if ref_test!(value, Number) {
                if (value as Number).value as i32 == 0 {
                    return 0;
                }
            } else if ref_test!(value, i31ref) {
                let value_i32: i32 = value as i31ref as i32;
                if value_i32 == 0 || value_i32 == 2 {
                    return 0;
                }
            }

            return 1;
        }

        fn is_primitive(value: anyref) -> i32 {
            return ref_test!(value, Number) || ref_test!(value, String) || ref_test!(value, StaticString) || ref_test!(value, i31ref) || ref_test!(value, BigInt) || ref_test!(value, null) || ref_test!(value, Symbol);
        }

        fn is_object(value: anyref) -> i32 {
            if ref_test!(value, null) {
                return 0;
            }

            if ref_test!(value, Object) || ref_test!(value, Promise) || ref_test!(value, Array) || ref_test!(value, AsyncGenerator) || ref_test!(value, Generator) {
                return 1;
            }

            return 0;
        }

        fn get_iterator(target: anyref) -> anyref {
            // TODO: handle lack of iterator or iterator not being a function
            let iterator_func: Function = get_property_value_sym(target, symbol_iterator as Symbol) as Function;
            return call_function(iterator_func, null, create_arguments_0(), null);
        }

        fn get_iterator_next(iterator: anyref) -> anyref {
            // TODO: handle next not being a function
            let next_func: Function = get_property_value(iterator, data!("next")) as Function;
            let result: anyref = call_function(next_func, null, create_arguments_0(), null);

            return result;
        }

        fn is_iterator_done(result: anyref) -> i32 {
            let done: anyref = get_property_value(result, data!("done"));

            return js_is_true(done);
        }

        fn get_iterator_result_value(result: anyref) -> anyref {
            return get_property_value(result, data!("value"));
        }

        fn Array_length(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            // TODO: can this be anything else?
            let array: Array = this as Array;

            return new_number(array.length as f64);
        }

        fn String_length(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            if ref_test!(this, String) {
                return new_number((this as String).length as f64);
            } else if ref_test!(this, StaticString) {
                return new_number((this as StaticString).length as f64);
            }

            return new_number(0 as f64);
        }

        fn ArraySetLength(array: Array, value: anyref) -> anyref {
            let length_number: Number = ToLength(value);

            if is_infinity(length_number) || is_nan(length_number) || !is_integer(length_number) {
                let error: anyref = create_error(data!("RangeError"), create_string_from_array("Failed to set the 'length' property on 'Array': Invalid array length"));
                throw!(JSException, error);
            }

            let lengthf64: f64 = length_number.value;
            let new_length: i32 = trunc!(lengthf64) as i32;

            return array_set_length(array, new_length);
        }

        fn array_set_length(array: Array, new_length: i32) -> Array {
            let empty: i31ref = 3 as i31ref;
            let new_array: AnyrefArray = [empty; new_length];
            let old_array: AnyrefArray = array.array;
            let stop: i32;
            if new_length >= array.length {
                stop = array.length;
            } else {
                stop = new_length;
            }
            array_copy(AnyrefArray, AnyrefArray, new_array, 0, old_array, 0, stop);

            array.array = new_array;
            array.length = new_length;
            return array;
        }

        fn is_infinity(number: Number) -> i32 {
            return number.value == f64::INFINITY || number.value == f64::NEG_INFINITY;
        }

        fn is_nan(number: Number) -> i32 {
            return number.value != number.value;
        }

        fn is_integer(number: Number) -> i32 {
            let value: f64 = number.value;
            return value == floor!(value);
        }

        fn ToInteger(target: anyref) -> Number {
            let number: Number = ToNumber(target);
            let value: f64 = number.value;
            if is_nan(number) {
                return new_number(0 as f64);
            }

            number.value = trunc!(value);
            return number;
        }

        fn min_i32(val1: i32, val2: i32) -> i32 {
            if val1 < val2 {
                return val1;
            }
            return val2;
        }

        fn min(val1: f64, val2: f64) -> f64 {
            if val1 < val2 {
                return val1;
            }
            return val2;
        }

        fn max(val1: f64, val2: f64) -> f64 {
            if val1 > val2 {
                return val1;
            }
            return val2;
        }

        fn min_num(val1: Number, val2: Number) -> Number {
            if val1.value < val2.value {
                return val1;
            }
            return val2;
        }

        fn max_num(val1: Number, val2: Number) -> Number {
            if val1.value > val2.value {
                return val1;
            }
            return val2;
        }

        fn JAWSM_DoesNotExceedSafeInteger(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return DoesNotExceedSafeInteger(first_argument_or_null(arguments) as Number);
        }

        fn DoesNotExceedSafeInteger(num: Number) -> Number {
            if num.value > SAFE_INTEGER {
                let error: anyref = create_error(data!("TypeError"), create_string_from_array("Maximum allowed index exceeded"));
                throw!(JSException, error);
            }

            return num;
        }

        fn JAWSM_LengthOfArrayLike(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return LengthOfArrayLike(first_argument_or_null(arguments));
        }

        fn LengthOfArrayLike(obj: anyref) -> Number {
            return ToLength(get_property_value(obj, data!("length")));
        }

        fn JAWSM_ToLength(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return ToLength(first_argument_or_null(arguments));
        }

        fn ToLength(value: anyref) -> Number {
            let number: Number = ToIntegerOrInfinity(value);
            if number.value > 0 as f64 {
                return new_number(min(number.value, SAFE_INTEGER));
            }

            return new_number(0 as f64);
        }

        fn JAWSM_ToAbsoluteIndex(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return ToAbsoluteIndex(first_argument_or_null(arguments), second_argument_or_null(arguments) as Number);
        }

        fn ToAbsoluteIndex(index: anyref, length: Number) -> Number {
            let integer: Number = ToIntegerOrInfinity(index);
            if integer.value < 0 as f64 {
                return new_number(max(integer.value + length.value, 0 as f64));
            }

            return min_num(integer, length);
        }

        fn ToIntegerOrInfinity(value: anyref) -> Number {
            let number: Number = ToNumber(value);
            if is_nan(number) || (number.value == 0 as f64) {
                return new_number(0 as f64);
            }
            let f: f64 = number.value;
            return new_number(trunc!(f));
        }

        fn ToNumber(target: anyref) -> Number {
            let result_i31ref: i31ref;
            let str: String;
            if ref_test!(target, null) {
                return new_number(f64::NAN);
            }

            if ref_test!(target, Number) {
                return target as Number;
            }

            if is_null(target) || is_false(target) {
                return new_number(0);
            }

            if is_true(target) {
                return new_number(1);
            }

            if ref_test!(target, Symbol) {
                let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot convert a Symbol value to a number"));
                throw!(JSException, error);
            }

            if ref_test!(target, StaticString) {
                str = convert_static_string_to_string(target as StaticString);
                return StringToNumber(str);
            }

            if ref_test!(target, String) {
                return StringToNumber(target as String);
            }

            return ToNumber(ToPrimitive(target, TO_PRIMITIVE_NUMBER));
        }

        fn ToNumeric(target: anyref) -> anyref {
            let mut result: anyref = ToPrimitive(target, TO_PRIMITIVE_NUMBER);
            if ref_test!(result, BigInt) {
                return result;
            }

            return ToNumber(result);
        }

        fn JAWSM_ToPropertyKey(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return ToPropertyKey(first_argument_or_null(arguments));
        }

        fn ToPropertyKey(target: anyref) -> anyref {
            let value: anyref = ToPrimitive(target, TO_PRIMITIVE_STRING);
            if ref_test!(value, Symbol) {
                return value;
            }

            return ToString(value);
        }

        fn JAWSM_ToObject(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return ToObject(first_argument_or_null(arguments));
        }

        fn ToObject(target: anyref) -> anyref {
            if is_null_or_undefined(target) {
                let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot convert null nor undefined to an object"));
                throw!(JSException, error);
            }

            if is_boolean(target) {
                return Boolean_constructor(global_scope as Scope, null, create_arguments_1(target), FunctionMetadata { new_target: get_variable(global_scope as Scope, data!("Boolean")) } );
            }

            if ref_test!(target, Number) {
                return Number_constructor(global_scope as Scope, null, create_arguments_1(target), FunctionMetadata { new_target: get_variable(global_scope as Scope, data!("Number")) } );
            }

            if ref_test!(target, String) {
                return String_constructor(global_scope as Scope, null, create_arguments_1(target), FunctionMetadata { new_target: get_variable(global_scope as Scope, data!("String")) } );
            }

            if ref_test!(target, Symbol) {
                return create_symbol_object(target as Symbol);
            }

            return target;
        }

        fn create_symbol_object(value: Symbol) -> anyref {
            // TODO: et constructor
            let obj: Object = create_object();
            obj.own_prototype = global_symbol_prototype;
            obj.value = value;
            set_property_value(obj, data!("constructor"), get_variable(global_scope as Scope, data!("Symbol")));
            return obj;
        }

        fn ToPrimitive(target: anyref, desired_type: i32) -> anyref {
            let to_primitive_maybe: anyref = get_property_value_sym(target, symbol_to_primitive as Symbol);
            let hint: StaticString = new_static_string(0, 0);
            let result: anyref;

            if !is_primitive(target) {
                if !ref_test!(to_primitive_maybe, null) {
                    if ref_test!(to_primitive_maybe, Function) {
                        if desired_type == TO_PRIMITIVE_NUMBER {
                            hint = new_static_string(data!("number"), 6);
                        } else if desired_type == TO_PRIMITIVE_STRING {
                            hint = new_static_string(data!("string"), 6);
                        } else {
                            hint = new_static_string(data!("default"), 6);
                        }

                        result = call_function(to_primitive_maybe as Function, target, create_arguments_1(hint), null);

                        if is_primitive(result) {
                            return result;
                        } else {
                            let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot convert object to primitive value"));
                            throw!(JSException, error);
                        }
                    } else {
                        let error: anyref = create_error(data!("TypeError"), create_string_from_array("Symbol.toPrimitive value has to be a function"));
                        throw!(JSException, error);
                    }
                } else {
                    if desired_type == 0 {
                        desired_type = TO_PRIMITIVE_NUMBER;
                    }

                    return OrdinaryToPrimtive(target, desired_type);
                }
            }

            return target;
        }

        fn ToBoolean(value: anyref) -> i31ref {
            let result: i31ref;
            let number: f64;
            if ref_test!(value, null) {
                result = 0 as i31ref;
                return result;
            } else if is_null(value) {
                result = 0 as i31ref;
                return result;
            } else if is_true(value) || is_false(value) {
                return value as i31ref;
            } else if ref_test!(value, Number) {
                number = (value as Number).value;
                if number == 0.0 || number != number {
                    result = 0 as i31ref;
                    return result;
                }
            } else if ref_test!(value, String) {
                if (value as String).length == 0 {
                    result = 0 as i31ref;
                    return result;
                }
            } else if ref_test!(value, StaticString) {
                if (value as StaticString).length == 0 {
                    result = 0 as i31ref;
                    return result;
                }
            }

            result = 1 as i31ref;
            return result;
        }

        fn OrdinaryToPrimtive(target: anyref, desired_type: i32) -> anyref {
            let result: anyref;
            if desired_type == TO_PRIMITIVE_STRING {
                result = try_method(target, data!("toString"));
                if !is_i31ref_value(result, -1) && !is_object(result) {
                    return result;
                }

                result = try_method(target, data!("valueOf"));
                if !is_i31ref_value(result, -1) && !is_object(result) {
                    return result;
                }

                let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot convert object to primitive value"));
                throw!(JSException, error);
            } else {
                result = try_method(target, data!("valueOf"));
                if !is_i31ref_value(result, -1) && !is_object(result) {
                    return result;
                }

                result = try_method(target, data!("toString"));
                if !is_i31ref_value(result, -1) && !is_object(result) {
                    return result;
                }

                let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot convert object to primitive value"));
                throw!(JSException, error);
            }

            return null;
        }

        // Will try to call a method with a given offset. Returns either
        // a result when it was able to call the function or i31ref -1
        // if it was not callable
        fn try_method(target: anyref, offset: i32) -> anyref {
            let maybe_func: anyref;
            let i31_result: i31ref = -1 as i31ref;

            maybe_func = get_property_value(target, offset);
            if ref_test!(maybe_func, Function) {
                return call_function(maybe_func as Function, target, create_arguments_0(), null);
            }

            return i31_result;
        }

        fn is_i31ref_value(value: anyref, expected: i32) -> i32 {
            // TODO: it looks like ref_test!(..., i31ref) will check for a "null i31ref". it would
            // be nice if that can be fixed, otherwise we have to check for null explicitly
            if !ref_test!(value, null) && ref_test!(value, i31ref) {
                let i32_value: i32 = value as i31ref as i32;
                if i32_value == expected {
                    return 1;
                }
            }

            return 0;
        }

        fn string_slice_start(target: String, index_start: i32) -> String {
            let len: i32 = target.length;
            if index_start < 0 {
                index_start = len + index_start;
                if index_start < 0 {
                    index_start = 0;
                }
            } else if index_start >= len {
                return create_empty_string();
            }

            return copy_string(target, index_start, len);
        }

        fn copy_string(target: String, index_start: i32, index_end: i32) -> String {
            let new_string_data: I32Array = [0; index_end - index_start];
            let i: i32 = index_start;
            let length: i32 = target.length;
            let target_data: I32Array = target.data;

            if index_end < 0 {
                index_end = length - 1;
            }

            while i <= index_end {
                new_string_data[i - index_start] = target_data[i];
                i += 1;
            }

            return String {
                data: new_string_data,
                length: index_end - index_start
            };
        }

        fn StringToNumber(target: String) -> Number {
            // Variable declarations
            let mut sign: f64 = 1.0;
            let mut start: i32 = 0;
            let mut value: f64 = 0.0;
            let mut fractional_multiplier: f64 = 0.0;
            let mut exponent_value: i32 = 0;
            let mut exponent_sign: i32 = 1;
            let mut decimal_point_found: i32 = 0;
            let mut valid_chars: i32 = 1;
            let mut base: i32 = 10;
            let mut offset: i32 = 0;
            let mut i: i32 = 0;
            let mut c: i32 = ' ';
            let mut digit: f64 = 0.0;
            let mut total_exp: i32 = 0;
            let mut remaining: i32 = 0;
            let mut prev: f64 = 0.0;
            let trimmed: String;
            let data: I32Array;
            let len: i32 = 0;

            // Implementation
            trimmed = string_trim(target);
            data = trimmed.data;
            len = trimmed.length;

            if len == 0 {
                return new_number(0);
            }

            if data[0] == '-' {
                sign = -1.0 as f64;
                start = 1;
            } else if data[0] == '+' {
                start = 1;
            }

            if compare_string_range_and_static_string(trimmed, start, len-1, new_static_string(data!("Infinity"), 8)) {
                return new_number(sign * f64::INFINITY);
            }

            base = 10;
            offset = start;
            if start < len - 1 && data[start] == '0' {
                if data[start+1] == 'x' || data[start+1] == 'X' {
                    base = 16;
                    offset = start + 2;
                } else if data[start+1] == 'o' || data[start+1] == 'O' {
                    base = 8;
                    offset = start + 2;
                } else if data[start+1] == 'b' || data[start+1] == 'B' {
                    base = 2;
                    offset = start + 2;
                }
            }

            if base != 10 {
                return parse_non_decimal(data, offset, len-1, base, sign);
            }

            i = start;
            while i < len && valid_chars {
                c = data[i];

                if c >= '0' && c <= '9' {
                    digit = (c - '0') as f64;

                    if decimal_point_found {
                        if fractional_multiplier == 0.0 {
                            fractional_multiplier = 0.1;
                        } else {
                            fractional_multiplier = fractional_multiplier * 0.1;
                        }
                        value = value + digit * fractional_multiplier;
                    } else {
                        value = value * 10.0 + digit;
                    }
                } else if c == '.' {
                    if decimal_point_found {
                        valid_chars = 0;
                    }
                    decimal_point_found = 1;
                } else if c == 'e' || c == 'E' {
                    i = i + 1;
                    if i >= len {
                        valid_chars = 0;
                        break;
                    }

                    if data[i] == '+' {
                        i = i + 1;
                    } else if data[i] == '-' {
                        exponent_sign = -1;
                        i = i + 1;
                    }

                    while i < len {
                        c = data[i];
                        if c >= '0' && c <= '9' {
                            exponent_value = exponent_value * 10 + (c - '0');
                        } else {
                            valid_chars = 0;
                            break;
                        }
                        i = i + 1;
                    }
                    break;
                } else {
                    valid_chars = 0;
                }

                i = i + 1;
            }

            if !valid_chars {
                return new_number(f64::NAN);
            }

            total_exp = exponent_sign * exponent_value;
            if total_exp > 0 {
                remaining = total_exp;
                while remaining > 0 {
                    if value == f64::INFINITY || value == f64::NEG_INFINITY {
                        break;
                    }

                    value = value * 10.0;
                    remaining = remaining - 1;
                }
            } else if total_exp < 0 {
                remaining = -total_exp;
                while remaining > 0 {
                    prev = value;
                    value = value / 10.0;
                    if value == prev {
                        value = 0.0;
                        break;
                    }
                    remaining = remaining - 1;
                }
            }

            value = value * sign;
            if value == 0.0 && sign < 0.0 {
                value = -0.0 as f64;
            }

            return new_number(value);
        }

        fn parse_non_decimal(data: I32Array, start: i32, end: i32, base: i32, sign: f64) -> Number {
            // Variable declarations
            let mut value: f64 = 0.0;
            let mut i: i32 = start;
            let mut c: i32 = ' ';
            let mut digit: f64 = 0.0;
            let mut numeric_value: i32 = 0;

            // Implementation
            while i <= end {
                c = data[i];
                numeric_value = c;
                digit = 0.0;

                if c >= '0' && c <= '9' {
                    digit = (numeric_value - '0') as f64;
                } else if c >= 'a' && c <= 'f' {
                    digit = (numeric_value - 'a' + 10) as f64;
                } else if c >= 'A' && c <= 'F' {
                    digit = (numeric_value - 'A' + 10) as f64;
                } else if c == '_' {
                    i = i + 1;
                    continue;
                } else {
                    return new_number(f64::NAN);
                }

                if digit >= base as f64 {
                    return new_number(f64::NAN);
                }

                value = value * base as f64 + digit;
                if value == f64::INFINITY {
                    break;
                }
                i = i + 1;
            }

            return new_number(sign * value);
        }
        // fn string_strip_prefix(prefix: String, target: String) -> String {
        //     let mut i: i32 = 0;
        //     let mut j: i32 = 0;
        //     let prefix_len: i32 = prefix.length;
        //     let target_len: i32 = target.length;
        //     let new_string_data: I32Array;
        //
        //     while i < prefix_len && i < target.length {
        //         if prefix.data[i] != target.data[i] {
        //             return target;
        //         }
        //         i += 1;
        //     }
        //
        //     if i == prefix_len {
        //         // we got to the end of the prefix, thus we can strip it
        //         if prefix_len == target_len {
        //             // prefix was the entire string, return an empty string
        //             return String {
        //                 data: [null; 0],
        //                 len: 0
        //             };
        //         }
        //
        //         new_string_data = [null; target_len - prefix_len];
        //         while j < target_len {
        //             new_string_data[j] = target.data[j + i];
        //             j += 1;
        //         }
        //
        //         return String {
        //             data: new_string_data,
        //             len: target_len - prefix_len
        //         };
        //     }
        //
        //     return target;
        // }

        fn string_trim(str: String) -> String {
            let mut i: i32 = 0;
            let old_data: I32Array = str.data;
            let l: i32 = len!(old_data);
            let mut j: i32 = l - 1;
            let new_data: I32Array;
            let mut offset: i32 = 0;

            while i < l {
                if old_data[i] != 32 && (old_data[i] < 9 || old_data[i] > 13) {
                    break;
                }
                i += 1;
            }

            if i == l {
                return create_empty_string();
            }

            while j >= 0 && (old_data[i] == 32 || (old_data[i] >= 9 && old_data[i] <= 13)) {
                j -= 1;
            }

            new_data = [0; j - i + 1];
            offset = i;

            while i <= j {
                new_data[i - offset] = old_data[i];
                i += 1;
            }

            return String {
                data: new_data,
                length: len!(new_data)
            };
        }

        fn Array_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return create_array(ToLength(first_argument_or_null(arguments)).value as i32);
        }

        fn String_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let arg: anyref = first_argument_or_null(arguments);
            let str: String = ToString(arg);

            if is_new_target(meta) {
                // TODO: et constructor
                let obj: Object = create_object();
                obj.own_prototype = global_string_prototype;
                obj.value = str;
                set_property_value(obj, data!("constructor"), get_variable(global_scope as Scope, data!("String")));
                return obj;
            }

            return str;
        }

        fn is_new_target(meta: anyref) -> i32 {
            if ref_test!(meta, null) {
                return 0;
            }

            return !ref_test!((meta as FunctionMetadata).new_target, null);
        }

        fn Number_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let arg: anyref = first_argument_or_null(arguments);
            let num: anyref = ToNumber(arg);

            if is_new_target(meta) {
                let obj: Object = create_object();
                obj.own_prototype = global_number_prototype;
                obj.value = num;
                set_property_value(obj, data!("constructor"), get_variable(global_scope as Scope, data!("Number")));
                return obj;
            }

            return num;
        }

        fn BigInt_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            // TODO: Handle calling this as a constructor
            let arg: anyref = first_argument_or_null(arguments);
            return ToBigInt(arg);
        }

        fn Boolean_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let arg: anyref = first_argument_or_null(arguments);
            let num: anyref = ToBoolean(arg);

            if is_new_target(meta) {
                let obj: Object = create_object();
                obj.own_prototype = global_boolean_prototype;
                obj.value = num;
                set_property_value(obj, data!("constructor"), get_variable(global_scope as Scope, data!("Boolean")));
                return obj;
            }

            return num;
        }

        fn Symbol_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let description: String = ToString(first_argument_or_null(arguments));
            let symbol: Symbol = Symbol { description: description };

            if is_new_target(meta) {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Symbol is not a constructor")));
            }

            return symbol;
        }

        fn Object_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let value: anyref = first_argument_or_null(arguments);

            if is_null_or_undefined(value) {
                return create_object();
            } else if is_object(value) {
                return value;
            } else {
                return ToObject(value);
            }

            return create_object();
        }

        fn Error_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            // TODO: here we should also handle a case when Error constructor is called without new
            let message: anyref = null;
            if len!(arguments) > 0 {
                message = arguments[0];
            }
            // let new_instance: Object = create_object();
            set_property_value(this, data!("message"), message);
            set_property_value(this, data!("name"), new_static_string(data!("Error"), 5));

            return this;
        }

        fn ReferenceError_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let message: anyref = null;
            if len!(arguments) > 0 {
                Error_constructor(scope, this, arguments, null);
            }
            set_property_value(this, data!("name"), new_static_string(data!("ReferenceError"), 14));

            return this;
        }

        fn TypeError_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let message: anyref = null;
            if len!(arguments) > 0 {
                Error_constructor(scope, this, arguments, null);
            }
            set_property_value(this, data!("name"), new_static_string(data!("TypeError"), 9));

            return this;
        }

        fn RangeError_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let message: anyref = null;
            if len!(arguments) > 0 {
                Error_constructor(scope, this, arguments, null);
            }
            set_property_value(this, data!("name"), new_static_string(data!("RangeError"), 10));

            return this;
        }

        fn SyntaxError_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let message: anyref = null;
            if len!(arguments) > 0 {
                Error_constructor(scope, this, arguments, null);
            }
            set_property_value(this, data!("name"), new_static_string(data!("SyntaxError"), 11));

            return this;
        }

        fn Math_floor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let target: anyref = first_argument_or_null(arguments);
            let number: Number = ToNumber(target);
            let value: f64 = number.value;

            return new_number(floor!(value));
        }

        fn Math_trunc(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let target: anyref = first_argument_or_null(arguments);
            let number: Number = ToNumber(target);
            let value: f64 = number.value;

            return new_number(trunc!(value));
        }

        fn create_function_prototype() -> Object {
            let object: Object = create_object();

            set_property(object, data!("toString"),
                create_property_function(global_scope as Scope, Function_toString, null));

            set_property(object, data!("call"),
                create_property_function(global_scope as Scope, Function_call, null));

            set_property(object, data!("bind"),
                create_property_function(global_scope as Scope, Function_bind, null));

            set_property(object, data!("apply"),
                create_property_function(global_scope as Scope, Function_apply, null));

            return object;
        }

        fn Function_apply(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let mut new_this: anyref = first_argument_or_null(arguments);
            let mut args: Array = array_from(second_argument_or_null(arguments));
            let mut args_data: AnyrefArray = args.array;
            let args_length: i32 = args.length;
            let mut i: i32 = 0;

            let new_args: JSArgs = [null; args_length];
            array_copy(JSArgs, AnyrefArray, new_args, 0, args_data, 0, args_length);

            // TODO: tail call
            return call_function(this, new_this, new_args, null);
        }

        fn Function_call(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let mut new_this: anyref = null;
            let mut rest: JSArgs = [null; 0];
            let args_length: i32 = len!(arguments);
            let mut i: i32 = 0;

            if args_length > 0 {
                new_this = arguments[0];
            }
            if args_length > 1 {
                rest = [null; args_length - 1];
                i = 1;
                while i < args_length {
                    rest[i - 1] = arguments[i];
                    i += 1;
                }
            }

            // fn call_function(func: anyref, this: anyref, arguments: JSArgs, meta:
            // Nullable<FunctionMetadata>) -> anyref {
            // TODO: tail call
            return call_function(this, new_this, rest, null);
        }

        fn Function_bind(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            // TODO: throw error if this is not true?
            if len!(arguments) > 0 && ref_test!(this, Function) {
                let function: Function = this as Function;
                let new_function: Function = Function {
                    scope: function.scope,
                    func: function.func,
                    this: arguments[0],
                    properties: clone_propertymap(function.properties),
                    own_prototype: function.own_prototype
                };

                return new_function;
            }

            return this;
        }

        fn Function_bind_simple(function: anyref, this: anyref) -> anyref {
            return Function_bind(global_scope as Scope, function, create_arguments_1(this), null);
        }

        fn Function_toString(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return new_static_string(data!("function () { [native code] }"), 29);
        }

        fn create_string_prototype() -> Object {
            let object: Object = create_object();

            set_property(object, data!("toString"),
                create_property_function(global_scope as Scope, String_toString, null));

            set_property(object, data!("valueOf"),
                create_property_function(global_scope as Scope, String_valueOf, null));

            let length_getter: Function = new_function(global_scope as Scope, String_length, null);
            set_property(object, data!("length"),
                Property {
                    value: AccessorMethod { get: length_getter, set: null },
                    flags: PROPERTY_IS_GETTER
                });

            return object;
        }

        fn create_boolean_prototype() -> Object {
            let object: Object = create_object();

            set_property(object, data!("toString"),
                create_property_function(global_scope as Scope, Boolean_toString, null));

            set_property(object, data!("valueOf"),
                create_property_function(global_scope as Scope, Boolean_valueOf, null));

            return object;
        }

        fn create_number_prototype() -> Object {
            let object: Object = create_object();

            set_property(object, data!("toString"),
                create_property_function(global_scope as Scope, Number_toString, null));

            set_property(object, data!("valueOf"),
                create_property_function(global_scope as Scope, Number_valueOf, null));

            return object;
        }

        fn create_bigint_prototype() -> Object {
            let object: Object = create_object();

            set_property(object, data!("toString"),
                create_property_function(global_scope as Scope, BigInt_toString, null));

            return object;
        }

        fn Symbol_toString(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let mut value: anyref = first_argument_or_null(arguments);

            if ref_test!(value, Object) {
                value = (value as Object).value;
            }

            if !ref_test!(value, Symbol) {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Symbol.prototype.toString requires that 'this' be a Symbol")));
            }

            let description: String = create_empty_string();
            let symbol: Symbol = value as Symbol;
            if !ref_test!(symbol.description, null) {
                description = symbol.description as String;
            }

            return add_strings(add_strings(create_string_from_array("Symbol("), description), create_string_from_array(")"));
        }

        fn Symbol_valueOf(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let mut value: anyref = first_argument_or_null(arguments);

            if ref_test!(value, Object) {
                value = (value as Object).value;
            }

            if !ref_test!(value, Symbol) {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Symbol.prototype.valueOf requires that 'this' be a Symbol")));
            }

            return value;
        }

        fn String_toString(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            if ref_test!(this, String) || ref_test!(this, StaticString) {
                return this;
            }

            if ref_test!(this, Object) {
                return ToString((this as Object).value);
            }

            return null;
        }

        fn String_valueOf(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            if ref_test!(this, Object) {
                if !ref_test!((this as Object).value, null) {
                    return (this as Object).value;
                }
            }

            return this;
        }

        fn Object_valueOf(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            if ref_test!(this, Object) {
                if !ref_test!((this as Object).value, null) {
                    return (this as Object).value;
                }
            }
            return this;
        }

        fn Boolean_toString(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let mut value: anyref = first_argument_or_null(arguments);

            if ref_test!(value, Object) {
                value = (value as Object).value;
            }

            if !is_boolean(value) {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Boolean.prototype.toString requires that 'this' be a Boolean")));
            }

            if (value as i31ref) == 0 {
                return new_static_string(data!("false"), 5);
            }

            return new_static_string(data!("true"), 4);
        }

        fn Boolean_valueOf(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let mut value: anyref = first_argument_or_null(arguments);

            if ref_test!(value, Object) {
                value = (value as Object).value;
            }

            if !is_boolean(value) {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Boolean.prototype.valueOf requires that 'this' be a Boolean")));
            }

            return value;
        }

        fn Number_valueOf(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return this;
        }

        fn Number_toString(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let mut value: anyref = first_argument_or_null(arguments);

            if ref_test!(value, Object) {
                value = (value as Object).value;
            }

            if !ref_test!(value, Number) {
                throw!(JSException, create_error(data!("TypeError"), create_string_from_array("Number.prototype.toString requires that 'this' be a Number")));
            }

            let mut radix: i32 = 10;
            let radix_maybe: anyref = first_argument_or_null(arguments);
            if !ref_test!(radix_maybe, null) {
                // TODO: handle NaN and other cases
                let radix_num: Number = ToNumber(radix_maybe) as Number;
                radix = radix_num.value as i32;
            }
            return NumberToString(value as Number, 10);
        }

        fn BigInt_toString(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let mut radix: i32 = 10;
            let radix_maybe: anyref = first_argument_or_null(arguments);
            if !ref_test!(radix_maybe, null) {
                // TODO: handle NaN and other cases
                let radix_num: Number = ToNumber(radix_maybe) as Number;
                radix = radix_num.value as i32;
            }
            return BigIntToString(this as BigInt, 10);
        }

        fn number_to_string_raw(number: Number) -> String {
            let written_length: i32 = writeF64AsAscii(number.value, free_memory_offset);
            return memory_to_string(free_memory_offset, written_length);
        }

        fn memory_to_string(offset: i32, length: i32) -> String {
            let string_data: I32Array = [0; length];

            // TODO: technically we could read bigger values and split them into i8s
            let mut i: i32 = 0;
            while i < length {
                string_data[i] = memory::<i8>[offset + i];
                i += 1;
            }

            return String {
                data: string_data,
                length: length,
            };
        }

        fn add_to_promise_chain(target_promise: Promise, promise: Promise) {
            let promises: PromisesArray = target_promise.chained_promises;
            let len: i32 = len!(promises);
            let mut i: i32 = 0;

            // Try to find an empty slot in the existing array
            while i < len {
                if ref_test!(promises[i], null) {
                    // Found empty slot, insert here
                    promises[i] = promise;
                    return;
                }
                i += 1;
            }

            // No empty slots found, double array size and try again
            let new_promises: PromisesArray = [null; len * 2];

            // Copy existing promises to new array
            // TODO: this should be done with array.copy, but array.copy is not
            // implemented in Tarnik yet
            i = 0;
            while i < len {
                new_promises[i] = promises[i];
                i += 1;
            }

            // Update target promise with new array
            target_promise.chained_promises = new_promises;

            // Try adding again with expanded array
            add_to_promise_chain(target_promise, promise);
        }

        fn evaluate_rejected(scope: Scope, previous_result: anyref, promise: Promise) {
            let mut arguments: JSArgs = create_arguments_1(previous_result);
            let mut result: anyref;

            try {
                // Handle finally callback if present
                if ref_test!(promise.finally_callback, Function) {
                    result = call_function(promise.finally_callback as Function, null, arguments, null);
                }

                // TODO: is the ordering here important?
                // Handle catch callback if present
                if ref_test!(promise.catch_callback, Function) {
                    result = call_function(promise.catch_callback as Function, null, arguments, null);
                } else {
                    result = previous_result;
                }

                // Reuse arguments array for resolve call
                arguments[0] = result;
                Promise_resolve(scope, promise, arguments, null);
            }
            catch(JSException, argument: anyref) {
                arguments = create_arguments_1(argument);
                Promise_reject(scope, promise, arguments, null);
            }
        }

        fn evaluate_resolved(scope: Scope, previous_result: anyref, promise: Promise) {
            let mut arguments: JSArgs = create_arguments_1(previous_result);
            let mut result: anyref;

            try {
                // Handle finally callback if present
                if ref_test!(promise.finally_callback, Function) {
                    result = call_function(promise.finally_callback as Function, null, arguments, null);
                }

                // Handle then callback if present
                if ref_test!(promise.then_callback, Function) {
                    result = call_function(promise.then_callback as Function, null, arguments, null);
                } else {
                    result = previous_result;
                }

                // Reuse arguments array for resolve call
                arguments[0] = result;
                Promise_resolve(scope, promise, arguments, null);
            }
            catch(JSException, argument: anyref) {
                arguments = create_arguments_1(argument);
                Promise_reject(scope, promise, arguments, null);
            }
        }

        fn Promise_finally(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let new_promise: Promise = create_new_promise();
            new_promise.own_prototype = promise_prototype;
            let promise: Promise = this as Promise;
            let callback_anyref: anyref;

            // If no arguments, return empty promise
            if len!(arguments) == 0 {
                return new_promise;
            }

            // Handle callback if present
            if len!(arguments) > 0 {
                callback_anyref = arguments[0];
                if ref_test!(callback_anyref, Function) {
                    new_promise.finally_callback = callback_anyref as Function;
                }
            }

            // Handle promise state based on current status
            if promise.resolved == 0 {
                // Not resolved - check if errored
                if promise.errored == 0 {
                    // Not resolved or errored - add to chain
                    add_to_promise_chain(promise, new_promise);
                } else {
                    // Errored - evaluate rejection handler
                    evaluate_rejected(scope, promise.error_result, new_promise);
                }
            } else {
                // Already resolved - evaluate fulfillment handler
                evaluate_resolved(scope, promise.success_result, new_promise);
            }

            return new_promise;
        }

        fn set_then_callback(thenable: anyref, callback: Function) {
            let then_function_any: anyref = get_property_value(thenable, data!("then"));

            if ref_test!(then_function_any, Function) {
                if ref_test!(thenable, Promise) {
                    // log(create_arguments_1(create_string_from_array("thenable is a promise")));
                    Promise_then(global_scope as Scope, thenable, create_arguments_1(callback), null);
                } else {
                    // log(create_arguments_1(create_string_from_array("thenable is not a promise")));
                    call_function(then_function_any, null, create_arguments_1(callback), null);
                }
            } else {
                let error: anyref = create_error(data!("TypeError"), create_string_from_array("then has to be a function"));
                throw!(JSException, error);
            }
        }

        fn Promise_then(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let new_promise: Promise = create_new_promise();
            new_promise.own_prototype = promise_prototype;
            let promise: Promise = this as Promise;
            let callback_anyref: anyref;

            // If no arguments, return empty promise
            if len!(arguments) == 0 {
                return new_promise;
            }

            // Handle onFulfilled callback if present
            if len!(arguments) > 0 {
                callback_anyref = arguments[0];
                if ref_test!(callback_anyref, Function) {
                    new_promise.then_callback = callback_anyref as Function;
                }
            }

            // Handle onRejected callback if present
            if len!(arguments) > 1 {
                callback_anyref = arguments[1];
                if ref_test!(callback_anyref, Function) {
                    new_promise.catch_callback = callback_anyref as Function;
                }
            }

            // Handle promise state based on current status
            if promise.resolved {
                // Already resolved - evaluate fulfillment handler
                evaluate_resolved(scope, promise.success_result, new_promise);
            } else {
                // Not resolved - check if errored
                if promise.errored {
                    // Errored - evaluate rejection handler
                    evaluate_rejected(scope, promise.error_result, new_promise);
                } else {
                    // Not resolved or errored - add to chain
                    add_to_promise_chain(promise, new_promise);
                }
            }

            return new_promise;
        }

        fn instantiate_promise(resolve: Function) -> Promise {
            let constructor: Function = get_variable(global_scope as Scope, data!("Promise")) as Function;
            let arguments: JSArgs = create_arguments_1(resolve);
            let promise: Promise = call_function(constructor, null, arguments, null) as Promise;

            return return_new_instance_result(promise, null, promise_prototype, constructor) as Promise;
        }

        fn AsyncGenerator_next(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            // this here is generator
            let generator: AsyncGenerator = this as AsyncGenerator;

            let callback: Function = generator.next_callback;

            let callback_scope: Scope = callback.scope;

            // set "special" variables (not accessible from JS), but accessible through our custom
            // generated code
            declare_variable(callback_scope, -1, generator, VARIABLE_VAR);
            // yield result
            declare_variable(callback_scope, -2, first_argument_or_null(arguments), VARIABLE_VAR);

            callback.scope = callback_scope;

            let promise: Promise = instantiate_promise(callback);

            return promise;
            // let generator_result: GeneratorResult = call_function(callback, null, arguments) as GeneratorResult;
            //
            // let result: Object = create_object();
            //
            // let done: i31ref;
            // if generator_result.done {
            //     done = 1 as i31ref;
            // } else {
            //     done = 0 as i31ref;
            // }
            // set_property_value(result, data!("done"), done);
            //
            // set_property_value(result, data!("value"), generator_result.result);
            //
            // generator.next_callback = generator_result.next_callback;
            //
            // return result;
        }

        fn Generator_iterator(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            return this;
        }

        fn Generator_next(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            // this here is generator
            let generator: Generator = this as Generator;

            let callback: Function = generator.next_callback;

            let generator_result: GeneratorResult = call_function(callback, null, arguments, null) as GeneratorResult;

            let result: Object = create_object();

            let done: i31ref;
            if generator_result.done {
                done = 1 as i31ref;
            } else {
                done = 0 as i31ref;
            }
            set_property_value(result, data!("done"), done);

            set_property_value(result, data!("value"), generator_result.result);

            generator.next_callback = generator_result.next_callback;

            return result;
        }

        fn Promise_resolve(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let mut resolve_result: anyref;
            let promise: Promise = this as Promise;
            let promises: PromisesArray = promise.chained_promises;
            let mut i: i32 = 0;
            let chained_promise: Promise;

            // Set resolve result based on arguments
            if len!(arguments) == 0 {
                resolve_result = null;
            } else {
                resolve_result = arguments[0];
            }

            // Update promise state
            promise.success_result = resolve_result;
            promise.resolved = 1;

            // Process chained promises
            let len: i32 = len!(promises);
            while i < len {
                if ref_test!(promises[i], null) == 0 {
                    chained_promise = promises[i] as Promise;
                    evaluate_resolved(scope, resolve_result, chained_promise);
                }
                i += 1;
            }

            return null;
        }

        fn Promise_reject(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let mut reject_result: anyref;
            let promise: Promise = this as Promise;
            let promises: PromisesArray = promise.chained_promises;
            let mut i: i32 = 0;
            let chained_promise: Promise;

            // Set reject result based on arguments
            if len!(arguments) == 0 {
                reject_result = null;
            } else {
                reject_result = arguments[0];
            }

            // Update promise state
            promise.error_result = reject_result;
            promise.errored = 1;

            // Process chained promises
            let len: i32 = len!(promises);
            while i < len {
                if ref_test!(promises[i], null) == 0 {
                    chained_promise = promises[i] as Promise;
                    evaluate_rejected(scope, reject_result, chained_promise);
                }
                i += 1;
            }

            return null;
        }

        fn Generator_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            // TODO: it looks like we may have to implement it:
            // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/GeneratorFunction/GeneratorFunction
            return null;
        }

        fn AsyncGenerator_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            // TODO: implement it
            return null;
        }

        fn Promise_constructor(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let promise: Promise = create_new_promise();
            let resolve_func: Function;
            let reject_func: Function;
            let callback_arguments: JSArgs;
            let resolver: Function;
            let reject_args: JSArgs;
            let offset: i32 = data!("Promise resolver undefined is not a function");

            if len!(arguments) == 0 {
                throw!(JSException, new_static_string(offset, 32));
            }

            let arg1: anyref = arguments[0];
            if ref_test!(arg1, Function) {
                resolve_func = new_function(global_scope as Scope, Promise_resolve, promise);
                reject_func = new_function(global_scope as Scope, Promise_reject, promise);
                callback_arguments = create_arguments_2(resolve_func, reject_func);

                try {
                    resolver = arg1 as Function;
                    call_function(resolver, null, callback_arguments, null);
                }
                catch(JSException, err: anyref) {
                    reject_args = create_arguments_1(err);
                    Promise_reject(scope, promise, reject_args, null);
                }

                return promise;
            } else {
                throw!(JSException, new_static_string(offset, 32));
            }

            return null;
        }

        fn first_argument_or_null(arguments: JSArgs) -> anyref {
            if len!(arguments) > 0 {
                return arguments[0];
            }

            return null;
        }

        fn second_argument_or_null(arguments: JSArgs) -> anyref {
            if len!(arguments) > 1 {
                return arguments[1];
            }

            return null;
        }

        fn return_custom_generator_callback(callback: Function, return_value: anyref) -> anyref {
            let result: GeneratorResult = GeneratorResult {
                next_callback: callback,
                result: return_value,
                done: 0,
            };

            return result;
        }

        fn return_generator_callback(return_value: anyref) -> anyref {
            let callback: Function = new_function(global_scope as Scope, empty_generator_callback, null);
            let result: GeneratorResult = GeneratorResult {
                next_callback: callback,
                result: return_value,
                done: 1,
            };

            return result;
        }

        fn empty_generator_callback(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let callback: Function = new_function(global_scope as Scope, empty_generator_callback, null);
            let result: GeneratorResult = GeneratorResult {
                next_callback: callback,
                result: null,
                done: 1,
            };

            return result;
        }

        // TODO: very similar to resolve_generator_callback, we could use this one but pass
        // null when needed
        fn resolve_custom_generator_callback(return_value: anyref, scope: Scope, resolve: Function, callback: Function) -> anyref {
            let generator: AsyncGenerator = get_variable(scope, -1) as AsyncGenerator;
            let result: Object = create_object();

            let done: i31ref = 0 as i31ref;
            set_property_value(result, data!("done"), done);
            set_property_value(result, data!("value"), return_value);

            generator.next_callback = callback;

            call_function(resolve, null, create_arguments_1(result), null);

            return null;
        }

        // TODO: very similar to resolve_empty_generator_callback, we could use this one but pass
        // null when needed
        fn resolve_generator_callback(return_value: anyref, scope: Scope, resolve: Function) -> anyref {
            let generator: AsyncGenerator = get_variable(scope, -1) as AsyncGenerator;
            let callback: Function = new_function(global_scope as Scope, empty_async_generator_callback, null);

            let result: Object = create_object();

            let done: i31ref = 1 as i31ref;
            set_property_value(result, data!("done"), done);
            set_property_value(result, data!("value"), return_value);

            generator.next_callback = callback;

            call_function(resolve, null, create_arguments_1(result), null);

            return null;
        }

        fn empty_async_generator_callback(scope: Scope, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let resolve: Function = first_argument_or_null(arguments) as Function;
            return resolve_empty_generator_callback(scope, resolve);
        }

        fn resolve_empty_generator_callback(scope: Scope, resolve: Function) -> anyref {
            let generator: AsyncGenerator = get_variable(scope, -1) as AsyncGenerator;
            let callback: Function = new_function(global_scope as Scope, empty_async_generator_callback, null);

            let result: Object = create_object();

            let done: i31ref = 1 as i31ref;
            set_property_value(result, data!("done"), done);
            set_property_value(result, data!("value"), null);

            generator.next_callback = callback;

            call_function(resolve, null, create_arguments_1(result), null);

            return null;
        }

        fn create_arguments_0() -> JSArgs {
            let arguments: JSArgs = [null; 0];
            return arguments;
        }

        fn create_arguments_1(arg1: anyref) -> JSArgs {
            let arguments: JSArgs = [null; 1];
            arguments[0] = arg1;
            return arguments;
        }

        fn create_arguments_2(arg1: anyref, arg2: anyref) -> JSArgs {
            let arguments: JSArgs = [null; 2];
            arguments[0] = arg1;
            arguments[1] = arg2;
            return arguments;
        }

        fn create_arguments_3(arg1: anyref, arg2: anyref, arg3: anyref) -> JSArgs {
            let arguments: JSArgs = [null; 3];
            arguments[0] = arg1;
            arguments[1] = arg2;
            arguments[2] = arg3;
            return arguments;
        }

        fn create_generator(callback: Function) -> anyref {
            let generator: Generator = Generator {
                properties: create_propertymap(),
                own_prototype: generator_prototype,
                next_callback: callback,
            };
            set_property(generator, data!("constructor"), create_bare_property(global_generator_constructor as Function));
            return generator;
        }

        fn create_async_generator(callback: Function) -> anyref {
            let generator: AsyncGenerator = AsyncGenerator {
                properties: create_propertymap(),
                own_prototype: async_generator_prototype,
                next_callback: callback,
            };
            set_property(generator, data!("constructor"), create_bare_property(global_async_generator_constructor as Function));
            return generator;
        }

        fn return_new_instance_result(first: anyref, second: anyref, prototype: anyref, constructor: Function) -> anyref {
            let mut result: anyref;
            let object: Object;
            let promise: Promise;
            let array: Array;
            if is_object(first) {
                result = first;
            } else {
                result = second;
            }

            if ref_test!(result, Object) {
                object = result as Object;
                object.own_prototype = prototype;
                //set_property(object, data!("constructor"), create_bare_property(constructor));
            } else if ref_test!(result, Promise) {
                promise = result as Promise;
                promise.own_prototype = prototype;
                //set_property(promise, data!("constructor"), create_bare_property(constructor));
            } else if ref_test!(result, Array) {
                array = result as Array;
                array.own_prototype = prototype;
                //set_property(array, data!("constructor"), create_bare_property(constructor));
            }

            return result;
        }

        fn convert_static_string_to_string(value: StaticString) -> String {
            let offset: i32 = value.offset;
            let length: i32 = value.length;
            let data: I32Array = [0; length];

            let i: i32 = 0;
            while i < length {
                data[i] = memory::<u16>[offset + (i * 2)];
                i += 1;
            }

            return String {
                data: data,
                length: length,
            };
        }

        fn add_static_strings(ptr1: i32, len1: i32, ptr2: i32, len2: i32) -> String {
            let total_length: i32 = len1 + len2;
            let string_data: I32Array = [0; total_length];
            let mut i: i32 = 0;

            // Copy first part
            while i < len1 {
                string_data[i] = memory::<u16>[ptr1 + (i * 2)];
                i += 1;
            }

            // Copy second part
            i = 0;
            while i < len2 {
                string_data::<u16>[len1 + i] = memory::<u16>[ptr2 + (i * 2)];
                i += 1;
            }

            return String {
                data: string_data,
                length: total_length
            };
        }

        fn create_empty_string() -> String {
            return String {
                data: "",
                length: 0,
            };
        }

        fn add_static_string_to_string(str: String, ptr: i32, len: i32) -> String {
            let str_len: i32 = str.length;
            let string_data: I32Array = str.data;
            let total_length: i32 = str_len + len;
            let new_string_data: I32Array = [0; total_length];

            // TODO: replace with array.copy when implemented
            let mut i: i32 = 0;
            while i < str_len {
                new_string_data[i] = string_data[i];
                i += 1;
            }

            i = 0;
            while i < len {
                new_string_data[str_len + i] = memory::<u16>[ptr + (i * 2)];
                i += 1;
            }

            return String {
                data: new_string_data,
                length: total_length
            };
        }

        fn add_string_to_static_string(static_string: StaticString, str: String) -> String {
            let str_len: i32 = str.length;
            let string_data: I32Array = str.data;
            let static_string_length: i32 = static_string.length;
            let static_string_offset: i32 = static_string.offset;
            let total_length: i32 = str_len + static_string_length;
            let new_string_data: I32Array = [0; total_length];

            // TODO: replace with array.copy when implemented
            let mut i: i32 = 0;
            while i < static_string_length {
                new_string_data[i] = memory::<u16>[static_string_offset + (i * 2)];
                i += 1;
            }

            i = 0;
            while i < str_len {
                new_string_data[i + static_string_length] = string_data[i];
                i += 1;
            }

            return String {
                data: new_string_data,
                length: total_length
            };
        }

        fn create_string_from_array(data: I32Array) -> String {
            return String {
                data: data,
                length: len!(data)
            };
        }

        fn create_propertymap() -> PropertyMap {
            return PropertyMap {
                entries: [null; 10],
                sym_entries: [null; 2],
                size: 0,
                sym_size: 0,
                interner: create_interner(),
            };
        }

        fn create_property(value: anyref) -> Property {
            return Property {
                value: value,
                flags: PROPERTY_WRITABLE | PROPERTY_ENUMERABLE | PROPERTY_CONFIGURABLE
            };
        }

        fn create_bare_property(value: anyref) -> Property {
            return Property { value: value, flags: 0 };
        }

        // TODO: this is almost the same as create_get_property, refactor
        fn create_get_property_str(value: anyref, target: anyref, name: String) -> Property {
            let property: Nullable<Property> = get_property_str(target, name);

            if ref_test!(property, null) {
                let new_property: Property = create_property(AccessorMethod {
                    get: value as Function,
                    set: null
                });
                new_property.flags = new_property.flags | PROPERTY_IS_GETTER;
                return new_property;
            } else {
                let existing_property: Property = property as Property;
                let accessor: AccessorMethod = existing_property.value as AccessorMethod;
                accessor.get = value as Function;
                // TODO: check if the existing property is a getter, otherwise we need to throw an
                // error
                existing_property.flags = existing_property.flags | PROPERTY_IS_GETTER;
                return existing_property;
            }

            throw!(JSException, 54545454 as i31ref);
        }

        fn create_get_property(value: anyref, target: anyref, name: i32) -> Property {
            let property: Nullable<Property> = get_property(target, name);

            if ref_test!(property, null) {
                let new_property: Property = create_property(AccessorMethod {
                    get: value as Function,
                    set: null
                });
                new_property.flags = new_property.flags | PROPERTY_IS_GETTER;
                return new_property;
            } else {
                let existing_property: Property = property as Property;
                let accessor: AccessorMethod = existing_property.value as AccessorMethod;
                accessor.get = value as Function;
                // TODO: check if the existing property is a getter, otherwise we need to throw an
                // error
                existing_property.flags = existing_property.flags | PROPERTY_IS_GETTER;
                return existing_property;
            }

            throw!(JSException, 54545454 as i31ref);
        }

        // TODO: this is almost the same as create_set_property, refactor
        fn create_set_property_str(value: anyref, target: anyref, name: String) -> Property {
            let property: Nullable<Property> = get_property_str(target, name);

            if ref_test!(property, null) {
                let new_property: Property = create_property(AccessorMethod {
                    get: null,
                    set: value as Function
                });
                new_property.flags = new_property.flags | PROPERTY_IS_SETTER;
                return new_property;
            } else {
                let existing_property: Property = property as Property;
                let accessor: AccessorMethod = existing_property.value as AccessorMethod;
                accessor.set = value as Function;
                // TODO: check if the existing property is a getter, otherwise we need to throw an
                // error
                existing_property.flags = existing_property.flags | PROPERTY_IS_SETTER;
                return existing_property;
            }

            throw!(JSException, 4545454 as i31ref);
        }


        fn create_set_property(value: anyref, target: anyref, name: i32) -> Property {
            let property: Nullable<Property> = get_property(target, name);

            if ref_test!(property, null) {
                let new_property: Property = create_property(AccessorMethod {
                    get: null,
                    set: value as Function
                });
                new_property.flags = new_property.flags | PROPERTY_IS_SETTER;
                return new_property;
            } else {
                let existing_property: Property = property as Property;
                let accessor: AccessorMethod = existing_property.value as AccessorMethod;
                accessor.set = value as Function;
                // TODO: check if the existing property is a getter, otherwise we need to throw an
                // error
                existing_property.flags = existing_property.flags | PROPERTY_IS_SETTER;
                return existing_property;
            }

            throw!(JSException, 4545454 as i31ref);
        }

        fn create_property_function(scope: Scope, function: JSFunc, this: anyref) -> Property {
            return Property {
                value: new_function(scope, function, this),
                flags: 0,
            };
        }

        fn new_variablemap() -> VariableMap {
            return VariableMap {
                entries: [null; 10],
                size: 0
            };
        }

        fn create_object() -> Object {
            return Object {
                properties: create_propertymap(),
                own_prototype: global_object_prototype,
                value: null,
                // interned: create_intern_map(),
            };
        }

        fn create_interner() -> Interner {
            return Interner {
                entries: [null; 2],
                size: 0,
                // interner offsets are negative, to make them different from memory offsets
                current_offset: -1
            };
        }

        fn create_array(size: i32) -> Array {
            let empty: i31ref = 3 as i31ref;
            return Array {
                array: [empty; size],
                length: size,
                properties: create_propertymap(),
                own_prototype: global_array_prototype
            };
        }

        fn new_function(scope: Scope, function: JSFunc, this: anyref) -> Function {
            let f: Function = Function {
                scope: scope,
                func: function,
                this: this,
                properties: create_propertymap(),
                own_prototype: global_function_prototype
            };

            let prototype: Object = create_object();
            set_property(f, data!("prototype"), create_property(prototype));
            set_property(prototype, data!("constructor"), create_property(f));

            return f;
        }

        fn new_number(number: f64) -> Number {
            return Number {
                value: number
            };
        }

        fn new_boolean(bool: i32) -> i31ref {
            return bool as i31ref;
        }

        fn cast_ref_to_i32_bool(arg: anyref) -> i32 {
            if ref_test!(arg, null) {
                return 0;
            }

            if ref_test!(arg, i31ref) {
                let res: i32 = arg as i31ref as i32;
                if res == 1 {
                    return 1;
                }
                return 0;
            }

            if ref_test!(arg, Number) {
                let num: Number = arg as Number;
                if num.value != 0.0 {
                    return 1;
                } else {
                    return 0;
                }
            }

            return 1;
        }

        fn new_static_string(offset: i32, length: i32) -> StaticString {
            return StaticString {
                offset: offset,
                length: length
            };
        }

        fn new_scope(parent: Nullable<Scope>) -> Scope {
            return Scope {
                parent: parent,
                variables: new_variablemap(),
            };
        }

        fn extract_parent_scope(scope: Scope) -> Scope {
            return scope.parent as Scope;
        }

        fn assign_variable(scope: Scope, name: i32, value: anyref) {
            let mut current_scope: Nullable<Scope> = scope;
            let maybe_var: Nullable<Variable>;

            while !ref_test!(current_scope, null) {
                maybe_var = variablemap_get(current_scope.variables, name);
                if ref_test!(maybe_var, null) {
                    current_scope = current_scope.parent;
                } else {
                    let var: Variable = maybe_var as Variable;
                    if var.flags & VARIABLE_CONST != 0 {
                        throw!(JSException, 13 as i31ref);
                    }

                    var.value = value;
                    return;
                }
            }

            if global_strict_mode == 0 {
                declare_variable(global_scope as Scope, name, value, VARIABLE_VAR);
            } else {
                throw!(JSException, create_string_from_array("ReferenceError: variable not defined"));
            }
        }

        fn delete_variable(scope: Scope, name: i32) {
            let existing: Nullable<Variable> = variablemap_get(scope.variables, name);
            let var: Variable;

            if !ref_test!(existing, null) {
                variablemap_delete(scope.variables, name);
            }
        }

        // this is a dummy function that I will use to figure out where does the arguments
        // definitions end
        fn arguments_declared() {}

        fn declare_arguments(scope: Scope, arguments: JSArgs) {
            let mut i: i32 = 0;
            let len: i32 = len!(arguments);
            let mut argument: anyref;
            let mut index: Number = new_number(0 as f64);
            let mut name_str: String;

            let args_object: Object = create_object();

            while i < len {
                argument = arguments[i];
                index = new_number(i as f64);
                name_str = to_string(index);
                set_property_value_str(args_object, name_str, argument);

                i+=1;
            }
            declare_variable(scope, data!("arguments"), args_object, VARIABLE_PARAM);
            set_property(args_object, data!("length"),
                Property {
                    value: new_number(len as f64),
                    flags: 0
                });
        }

        fn declare_variable(scope: Scope, name: i32, value: anyref, var_flag: i32) {
            let existing: Nullable<Variable> = variablemap_get(scope.variables, name);
            let var: Variable;

            if ref_test!(existing, null) {
                var = Variable {
                    value: value,
                    flags: var_flag,
                };

                variablemap_set(scope.variables, name, var);
            } else {
                var = existing as Variable;

                if var.flags & VARIABLE_PARAM != 0 || var.flags & VARIABLE_VAR != 0 || var.flags & VARIABLE_FUNC != 0{
                    var.value = value;
                } else if var.flags & VARIABLE_CONST != 0 {
                    // const declaration, throw error
                    throw!(JSException, 10 as i31ref);
                } else if var.flags & VARIABLE_LET {
                    // let declaration, throw error
                    throw!(JSException, 11 as i31ref);
                }
            }
        }

        fn variablemap_set(map: VariableMap, key: i32, value: Variable) {
            let mut entries: EntriesArray = map.entries;
            let new_entry: VariableMapEntry = VariableMapEntry { key: key, value: value };
            let mut found: i32 = 0;
            let mut i: i32 = 0;
            let new_size: i32;
            let mut new_entries: EntriesArray;
            let mut entry: VariableMapEntry;
            let len: i32 = len!(entries);
            let mut map_size: i32 = map.size;

            // First, search for existing key
            while i < map_size {
                if ref_test!(entries[i], VariableMapEntry) {
                    entry = entries[i] as VariableMapEntry;
                    if entry.key == key {
                        entry.value = value;
                        found = 1;
                        return;
                    }
                }
                i += 1;
            }

            // If key wasn't found, proceed with insertion
            if found == 0 {
                // Check if we need to resize
                if map_size >= len {
                    new_size  = len * 2;
                    new_entries = [null; new_size];

                    // Copy old entries to new array
                    i = 0;
                    while i < len {
                        new_entries[i] = entries[i];
                        i += 1;
                    }

                    map.entries = new_entries;
                    entries = new_entries;
                }

                // Add new entry and increment size
                entries[map.size] = new_entry;
                map.size = map.size + 1;
            }
        }

        fn variablemap_delete(map: VariableMap, key: i32) {
            let mut entries: EntriesArray = map.entries;
            let mut found: i32 = 0;
            let mut i: i32 = 0;
            let mut entry: VariableMapEntry;
            let mut map_size: i32 = map.size;

            // First, search for existing key
            while found == 0 && i < map_size {
                if ref_test!(entries[i], VariableMapEntry) {
                    entry = entries[i] as VariableMapEntry;
                    if entry.key == key {
                        found = i;
                    }
                }
                i += 1;
            }

            // If key was found, delete it
            if found != 0 {
                entries[found] = null;
                // TODO: shift and shrink? or maybe only after there is more nulls?
            }
        }

        fn create_reference_error(name: i32) -> anyref {
            let constructor: Function = get_variable(global_scope as Scope, data!("ReferenceError")) as Function;
            let object: Object = create_object();
            let arguments: JSArgs = create_arguments_1(new_static_string(data!("could not find reference"), 24));
            call_function(constructor, object, arguments, null);

            return return_new_instance_result(object, null, get_property(constructor, data!("prototype")), constructor);
        }

        // This is doing the same thing that get_variable, but if it can't find a variable it just
        // returns undefined, so it doesn't error out for typeof
        fn get_variable_for_typeof(scope: Scope, name: i32) -> anyref {
            let mut current_scope: Nullable<Scope> = scope;
            let var: Nullable<Variable>;

            while 1 {
                var = variablemap_get(current_scope.variables, name);
                if ref_test!(var, null) {
                    current_scope = current_scope.parent;
                    if ref_test!(current_scope, null) {
                        return null;
                    }
                } else {
                    return (var as Variable).value;
                }
            }

            return null;
        }

        fn get_variable(scope: Scope, name: i32) -> anyref {
            let mut current_scope: Nullable<Scope> = scope;
            let var: Nullable<Variable>;

            while 1 {
                var = variablemap_get(current_scope.variables, name);
                if ref_test!(var, null) {
                    current_scope = current_scope.parent;
                    if ref_test!(current_scope, null) {
                        // TODO: this should throw reference error, but we have to implement it in
                        // here, not in JS
                        throw!(JSException, offset_to_string(name));
                    }
                } else {
                    return (var as Variable).value;
                }
            }
            throw!(JSException, 103 as i31ref);
        }

        fn get_propertymap(target: anyref) -> Nullable<PropertyMap> {
            // TODO: as long as objects like Function and String are just another objects
            // we will have to reimplement a lot of stuff like this. It would be great
            // to research parent and child types
             if ref_test!(target, Object) {
                return (target as Object).properties;
            } else if ref_test!(target, Function) {
                return (target as Function).properties;
            } else if ref_test!(target, Promise) {
                return (target as Promise).properties;
            } else if ref_test!(target, Generator) {
                return (target as Generator).properties;
            } else if ref_test!(target, AsyncGenerator) {
                return (target as AsyncGenerator).properties;
            } else if ref_test!(target, GlobalThis) {
                return (target as GlobalThis).properties;
            } else if ref_test!(target, Array) {
                return (target as Array).properties;
            }

            return null;
        }

        fn get_enumerable_property_names(target: anyref) -> StringArray {
            return get_property_names(target, 0, PROPERTY_ENUMERABLE);
        }

        fn get_own_enumerable_property_names(target: anyref) -> StringArray {
            return get_property_names(target, 1, PROPERTY_ENUMERABLE);
        }

        fn get_own_property_names(target: anyref) -> StringArray {
            return get_property_names(target, 1, 0);
        }

        fn get_property_names(target: anyref, own: i32, flags: i32) -> StringArray {
            let mut maybe_propertymap: Nullable<PropertyMap> = get_propertymap(target);
            if ref_test!(maybe_propertymap, null) {
                return [];
            }

            let propertymap: PropertyMap = maybe_propertymap as PropertyMap;
            let mut i: i32 = 0;
            let mut j: i32 = 0;
            let entries: PropertyEntriesArray = propertymap.entries;
            let mut length: i32 = len!(entries);
            let mut entries_count: i32 = 0;
            let mut property: Property;
            let mut count: i32 = 0;
            let mut maybe_propertymap_entry: Nullable<PropertyMapEntry>;
            let mut propertymap_entry: PropertyMapEntry;

            // TODO: I don't like it that we need to loop through properties twice. On the other
            // hand if we wanted to loop only once, we might need to resize the array when getting
            // more properties. For now I'm leaving it as is, but might be a good candidate for
            // benchmarking and optimizing
            while i < length {
                maybe_propertymap_entry = entries[i];

                if !ref_test!(maybe_propertymap_entry, null) {
                    property = (maybe_propertymap_entry as PropertyMapEntry).value;
                    if flags == 0 || property.flags & flags != 0 {
                        count += 1;
                    }
                }
                i += 1;
            }

            let own_prototype: anyref = get_own_prototype(target);
            let mut proto_names: StringArray = [null; 0];
            if !own {
                if ref_test!(own_prototype, null) {
                    proto_names = [null; 0];
                } else {
                    proto_names = get_enumerable_property_names(own_prototype);
                    count += len!(proto_names);
                }
            }

            let names: StringArray = [null; count];

            i = 0;
            while i < length {
                maybe_propertymap_entry = entries[i];

                if !ref_test!(maybe_propertymap_entry, null) {
                    propertymap_entry = maybe_propertymap_entry as PropertyMapEntry;
                    property = propertymap_entry.value;
                    if flags == 0 || property.flags & flags != 0 {
                        names[j] = get_property_name(propertymap, propertymap_entry.key);

                        j += 1;
                    }
                }
                i += 1;
            }

            i = 0;
            length = len!(proto_names);
            while i < length {
                names[j] = proto_names[i];
                i += 1;
                j += 1;
            }

            return names;
        }

        fn get_property_name(propertymap: PropertyMap, key: i32) -> Nullable<String> {
            if key < 0 {
                return get_interned_string_by_key(propertymap.interner, key);
            }

            return offset_to_string(key);
        }

        fn get_enumerable_values(target: anyref) -> AnyrefArray {
            let mut maybe_propertymap: Nullable<PropertyMap> = get_propertymap(target);
            if ref_test!(maybe_propertymap, null) {
                return [];
            }

            let propertymap: PropertyMap = maybe_propertymap as PropertyMap;
            let mut i: i32 = 0;
            let mut j: i32 = 0;
            let entries: PropertyEntriesArray = propertymap.entries;
            let mut length: i32 = len!(entries);
            let mut entries_count: i32 = 0;
            let mut property: Property;
            let mut count: i32 = 0;

            // TODO: I don't like it that we need to loop through properties twice. On the other
            // hand if we wanted to loop only once, we might need to resize the array when getting
            // more properties. For now I'm leaving it as is, but might be a good candidate for
            // benchmarking and optimizing
            while i < length {
                let maybe_propertymap_entry: Nullable<PropertyMapEntry> = entries[i];

                if !ref_test!(maybe_propertymap_entry, null) {
                    property = (maybe_propertymap_entry as PropertyMapEntry).value;
                    if property.flags & PROPERTY_ENUMERABLE != 0 {
                        count += 1;
                    }
                }
                i += 1;
            }

            let own_prototype: anyref = get_own_prototype(target);
            let mut proto_values: AnyrefArray = [null; 0];
            if ref_test!(own_prototype, null) {
                proto_values = [null; 0];
            } else {
                proto_values = get_enumerable_values(own_prototype);
                count += len!(proto_values);
            }

            let values: AnyrefArray = [null; count];

            i = 0;
            while i < length {
                let maybe_propertymap_entry: Nullable<PropertyMapEntry> = entries[i];

                if !ref_test!(maybe_propertymap_entry, null) {
                    property = (maybe_propertymap_entry as PropertyMapEntry).value;
                    if property.flags & PROPERTY_ENUMERABLE != 0 {
                        values[j] = property.value;
                        j += 1;
                    }
                }
                i += 1;
            }

            i = 0;
            length = len!(proto_values);
            while i < length {
                values[j] = proto_values[i];
                i += 1;
                j += 1;
            }

            return values;
        }

        fn get_property_str(target: anyref, name: anyref) -> Nullable<Property> {
            let offset: i32;

            let name_str: String = ToString(name);
            let offset: i32 = get_data_offset_str(name_str.data);

            if offset == 0 {
                current_string_lookup = name_str;
            }

            return get_property(target, offset);
        }

        fn get_property_sym(target: anyref, key: Symbol) -> Nullable<Property> {
            // TODO: this is very similar to get_property, might be good to refactor
            let mut result: Nullable<Property> = null;
            let function: Function;
            let property: Property;
            let global_this: GlobalThis;

            let properties: Nullable<PropertyMap> = get_propertymap(target);
            let own_prototype: anyref = get_own_prototype(target);
            if !ref_test!(properties, null) {
                result = propertymap_get_sym(properties as PropertyMap, key);
                if ref_test!(result, null) {
                    if !ref_test!(own_prototype, null) {
                        result = get_property_sym(own_prototype, key);
                    }
                }
            } else {
                if !ref_test!(own_prototype, null) {
                    result = get_property_sym(own_prototype, key);
                }
            }

            if ref_test!(result, null) {
                return null as Nullable<Property>;
            }

            // at this point the result has to be a property
            let property = result as Property;
            if ref_test!(property.value, Function) {
                (property.value as Function).this = target;
            }

            return result;
        }

        fn get_own_property_sym(target: anyref, key: Symbol) -> Nullable<Property> {
            let mut result: Nullable<Property> = null;
            let property: Property;

            let properties: Nullable<PropertyMap> = get_propertymap(target);
            if !ref_test!(properties, null) {
                result = propertymap_get_sym(properties as PropertyMap, key);
            }

            if ref_test!(result, null) {
                return null as Nullable<Property>;
            }

            // at this point the result has to be a property
            let property = result as Property;
            if ref_test!(property.value, Function) {
                (property.value as Function).this = target;
            }

            return result;
        }

        fn get_own_property_str(target: anyref, name: anyref) -> Nullable<Property> {
            let offset: i32;

            if ref_test!(name, String) {
                offset = get_data_offset_str((name as String).data);
            } else if ref_test!(name, StaticString) {
                offset = get_data_offset_static_str(name as StaticString);
            } else {
                // should we try to convert to string again?
                // TODO: implement
                return null as Nullable<Property>;
            }

            return get_own_property(target, offset);
        }

        fn delete_property_str(target: anyref, name: anyref) {
            let offset: i32;

            if ref_test!(name, String) {
                offset = get_data_offset_str((name as String).data);
            } else if ref_test!(name, StaticString) {
                offset = get_data_offset_static_str(name as StaticString);
            } else {
                // should we try to convert to string again?
                // TODO: implement
                return;
            }

            if offset != 0 {
                delete_property(target, offset);
            }
        }

        fn get_class_property_value(target: anyref, offset: i32) -> anyref {
            let result: anyref = get_property_value(target, offset);
            if ref_test!(result, Function) {
                (result as Function).this = target;
            }
            return result;
        }

        fn get_property_value(target: anyref, offset: i32) -> anyref {
            let property: Nullable<Property> = get_property(target, offset);

            if !ref_test!(property, null) {
                return get_value_of_property(property, target);
            }

            return null;
        }

        fn get_property_value_str(target: anyref, name: String) -> anyref {
            let property: Nullable<Property> = get_property_str(target, name);

            if !ref_test!(property, null) {
                return get_value_of_property(property, target);
            }

            return null;
        }

        fn get_property_value_sym(target: anyref, key: Symbol) -> anyref {
            let property: Nullable<Property> = get_property_sym(target, key);

            if !ref_test!(property, null) {
                return get_value_of_property(property, target);
            }

            return null;
        }

        fn get_value_of_property(property_arg: anyref, target: anyref) -> anyref {
            if ref_test!(property_arg, Property) {
                let property: Property = property_arg as Property;
                if (property.flags & PROPERTY_IS_GETTER) != 0 {
                    let accessor: AccessorMethod = property.value as AccessorMethod;
                    let function: Function = accessor.get as Function;
                    function.this = target;
                    let value: anyref = call_function(function, target, create_arguments_0(), null);
                    return value;
                } else {
                    return property.value;
                }
            }

            return null;
        }

        fn to_string(value: anyref) -> String {
            return ToString(value);
        }

        fn set_property_value_sym(target: anyref, key: Symbol, value: anyref) {
            // TODO: this code is almost the same as set_property_value
            let property: Nullable<Property> = get_property_sym(target, key);
            let value_property: Property;
            if ref_test!(property, null) {
                value_property = create_property(value);
                if ref_test!(target, Object) {
                    propertymap_set_sym((target as Object).properties, key, value_property);
                    return;
                }

                if ref_test!(target, Function) {
                    propertymap_set_sym((target as Function).properties, key, value_property);
                    return;
                }

                if ref_test!(target, Promise) {
                    propertymap_set_sym((target as Promise).properties, key, value_property);
                    return;
                }

                if ref_test!(target, Generator) {
                    propertymap_set_sym((target as Generator).properties, key, value_property);
                    return;
                }

                if ref_test!(target, AsyncGenerator) {
                    propertymap_set_sym((target as AsyncGenerator).properties, key, value_property);
                    return;
                }

                if ref_test!(target, Array) {
                    propertymap_set_sym((target as Array).properties, key, value_property);
                    return;
                }

                if ref_test!(target, GlobalThis) {
                    propertymap_set_sym((target as GlobalThis).properties, key, value_property);
                    // TODO: need to implement this
                    // eclare_variable(global_scope as Scope, key, value_property.value, VARIABLE_VAR);
                    return;
                }
            } else {
                value_property = property as Property;
                if value_property.flags & PROPERTY_IS_SETTER != 0 {
                    let accessor: AccessorMethod = value_property.value as AccessorMethod;
                    let function: Function = accessor.set as Function;
                    let arguments: JSArgs = create_arguments_1(value);
                    call_function(function, target, arguments, null);
                } else {
                    value_property.value = value;
                }
                return;
            }

            throw!(JSException, create_string_from_array("could not set property by symbol"));
        }

        fn set_property_value_str(target: anyref, name: anyref, value: anyref) {
            // TODO: this code is almost the same as set_property_str. We should
            // extract the code that fetches an offset to a separate function
            let offset: i32 = 0;
            let name_str: String = String {
                data: [0; 0],
                length: 0
            };

            if ref_test!(name, String) {
                offset = get_data_offset_str((name as String).data);
                if offset == 0 {
                    name_str = name as String;
                }
            } else if ref_test!(name, StaticString) {
                offset = get_data_offset_static_str(name as StaticString);
                if offset == 0 {
                    name_str = convert_static_string_to_string(name as StaticString);
                }
            } else {
                name_str = to_string(name);
                offset = get_data_offset_str(name_str.data);
            }

            if offset == 0 {
                current_string_lookup = name_str;
            }

            set_property_value(target, offset, value);
        }

        fn convert_to_string(value: anyref) -> String {
            let data: I32Array = [0; 0];

            if ref_test!(value, null) {
                data = "undefined";
            } else if ref_test!(value, String) {
                data = (value as String).data;
            } else if ref_test!(value, StaticString) {
                data = convert_static_string_to_string(value as StaticString).data;
            } else if ref_test!(value, Number) {
                data = number_to_string_raw(value as Number).data;
            } else {
                throw!(JSException, create_string_from_array("Can't convert value to string"));
            }

            return String {
                data: data,
                length: len!(data)
            };
        }

        fn get_own_property(target: anyref, name: i32) -> Nullable<Property> {
            let mut result: Nullable<Property> = null;
            let property: Property;

            if ref_test!(target, Array) && name == data!("length") {
                return Property {
                    value: new_number((target as Array).length as f64),
                    flags: PROPERTY_WRITABLE
                };
            }

            let properties: Nullable<PropertyMap> = get_propertymap(target);
            if !ref_test!(properties, null) {
                result = propertymap_get(properties as PropertyMap, name);
            }

            if ref_test!(result, null) {
                return null as Nullable<Property>;
            }

            // at this point the result has to be a property
            let property = result as Property;
            if ref_test!(property.value, Function) {
                (property.value as Function).this = target;
            }

            return result;
        }

        // TODO: this should also handle prototypes
        fn get_property(target: anyref, name: i32) -> Nullable<Property> {
            let mut result: Nullable<Property> = null;
            let promise: Promise;
            let function: Function;
            let object: Object;
            let property: Property;
            let global_this: GlobalThis;

            let properties: Nullable<PropertyMap> = get_propertymap(target);
            let own_prototype: anyref = get_own_prototype(target);
            result = get_own_property(target, name);

            if ref_test!(result, null) {
                if !ref_test!(own_prototype, null) {
                    result = get_property(own_prototype, name);
                }
            }

            if ref_test!(result, null) {
                return null as Nullable<Property>;
            }

            // at this point the result has to be a property
            let property = result as Property;
            if ref_test!(property.value, Function) {
                (property.value as Function).this = target;
            }

            return result;
        }

        fn clone_interner(interner: Interner) -> Interner {
            let mut entry: InternedString;
            let mut new_entry: InternedString;
            let new_interner: Interner = Interner {
                entries: [null; interner.length],
                length: interner.length,
                current_offset: interner.current_offset
            };
            let entries: InternedStringArray = interner.entries;
            let new_entries: InternedStringArray = new_interner.entries;
            let mut maybe_entry: Nullable<InternedString>;

            let mut i: i32 = 0;
            let length: i32 = interner.length;
            while i < length {
                maybe_entry = entries[i];
                if !ref_test!(maybe_entry, null) {
                    entry = maybe_entry as InternedString;
                    new_entries[i] = InternedString {
                        value: entry.value,
                        offset: entry.offset
                    };
                }
            }

            return new_interner;
        }

        fn clone_propertymap(map: PropertyMap) -> PropertyMap {
            let mut entry: PropertyMapEntry;
            let mut sym_entry: SymPropertyMapEntry;
            let mut maybe_entry: Nullable<PropertyMapEntry>;
            let mut maybe_sym_entry: Nullable<SymPropertyMapEntry>;
            let new_propertymap: PropertyMap = PropertyMap {
                entries: [null; map.size],
                sym_entries: [null; map.sym_size],
                size: map.size,
                sym_size: map.sym_size,
                interner: clone_interner(map.interner)
            };
            let entries: PropertyEntriesArray = map.entries;
            let new_entries: PropertyEntriesArray = new_propertymap.entries;
            let sym_entries: SymPropertyEntriesArray = map.sym_entries;
            let new_sym_entries: SymPropertyEntriesArray = new_propertymap.sym_entries;

            let mut i: i32 = 0;
            let mut size: i32 = map.size;
            while i < size {
                maybe_entry = entries[i];
                if !ref_test!(maybe_entry, null) {
                    entry = maybe_entry as PropertyMapEntry;
                    new_entries[i] = PropertyMapEntry {
                        key: entry.key,
                        value: entry.value
                    };
                }
                i+=1;
            }

            i = 0;
            size = map.sym_size;
            while i < size {
                maybe_sym_entry = sym_entries[i];
                if !ref_test!(maybe_sym_entry, null) {
                    sym_entry = maybe_sym_entry as SymPropertyMapEntry;
                    new_sym_entries[i] = SymPropertyMapEntry {
                        key: sym_entry.key,
                        value: sym_entry.value
                    };
                }
                i+=1;
            }

            return new_propertymap;
        }

        fn clone(target: anyref) -> anyref {
            let mut result: Nullable<Property> = null;
            let promise: Promise;
            let function: Function;
            let object: Object;
            let property: Property;

            if ref_test!(target, Object) {
                object = target as Object;
                return Object {
                    properties: clone_propertymap(object.properties),
                    own_prototype: object.own_prototype,
                    value: object.value
                };
            } else if ref_test!(target, Function) {
                function = target as Function;
                return Function {
                    scope: function.scope,
                    func: function.func,
                    this: function.this,
                    properties: clone_propertymap(function.properties),
                    own_prototype: function.own_prototype
                };
            } else if ref_test!(target, Promise) {
                promise = target as Promise;
            } else if ref_test!(target, GlobalThis) {
                let global_this: GlobalThis = target as GlobalThis;
            } else if ref_test!(target, Array) {
                let array: Array = target as Array;
            }

            return target;
        }

        fn coalesce(arg1: anyref, arg2: anyref) -> anyref {
            if !ref_test!(arg1, null) {
                return arg1;
            }

            return arg2;
        }

        fn get_array_rest(obj: anyref, i: i32) -> anyref {
            let mut j: i32 = i;
            let length: i32;
            if ref_test!(obj, Array) {
                let array: AnyrefArray = (obj as Array).array;
                length = len!(array);
                if i < length {
                    let new_array: Array = create_array(length - i);
                    let new_array_array: AnyrefArray = new_array.array;
                    while j < length {
                        new_array_array[j - i] = array[j];
                        j += 1;
                    }

                    return new_array;
                }
            }

            return create_array(0);
        }

        fn is_array_element_available(array: Array, i: i32) -> i32 {
            let data: AnyrefArray = array.array;
            if i < array.length && i >= 0 {
                return !is_empty(data[i]);
            }

            return 0;
        }

        fn get_array_element(obj: anyref, i: i32) -> anyref {
            if ref_test!(obj, Array) {
                let array: AnyrefArray = (obj as Array).array;
                if i < len!(array) {
                    if is_empty(array[i]) {
                        return null;
                    } else {
                        return array[i];
                    }
                }
            }

            return null;
        }

        fn get_string_character(str: String, i: i32) -> anyref {
            if i < 0 {
                return null;
            }

            let data: I32Array = str.data;
            if i < len!(data) {
                return create_string_from_array([data[i]]);
            }

            return null;
        }

        fn get_arguments_element(arguments: JSArgs, i: i32) -> anyref {
            if i < len!(arguments) {
                return arguments[i];
            }

            return null;
        }

        fn get_property_or_array_value(target: anyref, prop_name: anyref) -> anyref {
            if ref_test!(target, Array) && !ref_test!(prop_name, Symbol) {
                let index: Number = ToNumber(prop_name);

                if !is_nan(index) && !is_infinity(index) && is_integer(index) {
                    if index.value < 0 as f64 {
                        return null;
                    }
                    return get_array_element(target, index.value as i32);
                }
            }

            if ref_test!(target, StaticString) {
                target = convert_static_string_to_string(target as StaticString);
            }

            if ref_test!(target, String) {
                let index: Number = ToNumber(prop_name);

                if !is_nan(index) && !is_infinity(index) && is_integer(index) {
                    return get_string_character(target as String, index.value as i32);
                }
            }

            if is_object(target) {
                let converted_prop_name: anyref = ToPropertyKey(prop_name);

                if ref_test!(prop_name, Symbol) {
                    return get_property_value_sym(target, converted_prop_name as Symbol);
                } else {
                    return get_property_value_str(target, converted_prop_name as String);
                }
            }

            return null;
        }

        fn set_property_or_array_value(target: anyref, prop_name: anyref, value: anyref) {
            if ref_test!(target, Array) {
                let index: Number = ToNumber(prop_name);

                if !is_nan(index) && !is_infinity(index) && is_integer(index) {
                    set_array_element(target, index.value as i32, value);
                    return;
                }
            }

            if is_object(target) {
                let converted_prop_name: anyref = ToPropertyKey(prop_name);

                if ref_test!(prop_name, Symbol) {
                    set_property_value_sym(target, converted_prop_name as Symbol, value);
                    return;
                } else {
                    set_property_value_str(target, converted_prop_name as String, value);
                    return;
                }
            }

            // TODO: do we have to throw an error here? It looks like it just silently ignores on
            // primitives
        }

        fn set_array_element(target: anyref, index: i32, value: anyref) {
            let mut array: Array = target as Array;

            if index > array.length - 1 {
                array = array_set_length(array, index + 1);
            }

            let raw_array: AnyrefArray = array.array;

            raw_array[index] = value;
        }

        fn destructure_property_single_name(target: anyref, property_name: i32, var_name: i32, scope: Scope, init: anyref, var_type_index: i32) {
            let value: anyref = get_property_value(target, property_name);
            delete_property(target, property_name);

            if ref_test!(value, null) {
                value = init;
            }
            if var_type_index == -1 {
                assign_variable(scope, var_name, value);
            } else {
                declare_variable(scope, var_name, value, var_type_index);
            }
        }

        fn destructure_property_single_name_str(target: anyref, property_name: String, var_name: i32, scope: Scope, init: anyref, var_type_index: i32) {
            let value: anyref = get_property_value_str(target, property_name);
            delete_property_str(target, property_name);

            if ref_test!(value, null) {
                value = init;
            }
            if var_type_index == -1 {
                assign_variable(scope, var_name, value);
            } else {
                declare_variable(scope, var_name, value, var_type_index);
            }
        }

        fn delete_property(target: anyref, name: i32) {
            let mut result: Nullable<Property> = null;
            let promise: Promise;
            let generator: Generator;
            let async_generator: AsyncGenerator;
            let function: Function;
            let object: Object;
            let property: Property;

            if ref_test!(target, Object) {
                object = target as Object;
                propertymap_delete(object.properties, name);
            } else if ref_test!(target, Function) {
                function = target as Function;
                propertymap_delete(function.properties, name);
            } else if ref_test!(target, Generator) {
                generator = target as Generator;
                propertymap_delete(generator.properties, name);
            } else if ref_test!(target, AsyncGenerator) {
                async_generator = target as AsyncGenerator;
                propertymap_delete(async_generator.properties, name);
            } else if ref_test!(target, Promise) {
                promise = target as Promise;
                propertymap_delete(promise.properties, name);
            } else if ref_test!(target, GlobalThis) {
                let global_this: GlobalThis = target as GlobalThis;
                propertymap_delete(global_this.properties, name);
                delete_variable(global_scope as Scope, name);
            } else if ref_test!(target, Array) {
                let array: Array = target as Array;
                propertymap_delete(array.properties, name);
            } else if ref_test!(target, Number) {
                // do nothing?
            }
        }

        fn set_property_sym(target: anyref, key: Symbol, property: Property) {
            if ref_test!(target, Object) {
                propertymap_set_sym((target as Object).properties, key, property);
                return;
            }

            if ref_test!(target, Function) {
                propertymap_set_sym((target as Function).properties, key, property);
                return;
            }

            if ref_test!(target, Promise) {
                propertymap_set_sym((target as Promise).properties, key, property);
                return;
            }

            if ref_test!(target, Generator) {
                propertymap_set_sym((target as Generator).properties, key, property);
                return;
            }

            if ref_test!(target, AsyncGenerator) {
                propertymap_set_sym((target as AsyncGenerator).properties, key, property);
                return;
            }

            if ref_test!(target, Array) {
                propertymap_set_sym((target as Array).properties, key, property);
                return;
            }

            if ref_test!(target, GlobalThis) {
                propertymap_set_sym((target as GlobalThis).properties, key, property);
                // TODO: implement this
                // declare_variable(global_scope as Scope, key, property.value, VARIABLE_VAR);
            return;
             }

            throw!(JSException, 9999101 as i31ref);
        }

        fn set_property_str(target: anyref, name: anyref, property: Property) {
            // TODO: this code is almost the same as set_property_value_str. We should
            // extract the code that fetches an offset to a separate function
            let offset: i32 = 0;
            let name_str: String = String {
                data: [0; 0],
                length: 0
            };

            if ref_test!(name, String) {
                offset = get_data_offset_str((name as String).data);
                if offset == 0 {
                    name_str = name as String;
                }
            } else if ref_test!(name, StaticString) {
                offset = get_data_offset_static_str(name as StaticString);
                if offset == 0 {
                    name_str = convert_static_string_to_string(name as StaticString);
                }
            } else {
                let maybe_to_string: Nullable<Property> =
                    get_property_str(name, create_string_from_array("toString"));
                if ref_test!(maybe_to_string, null) {
                    // we can't convert to string
                    throw!(JSException, create_string_from_array("Can't convert property name to string"));
                } else {
                    let to_string: Property = maybe_to_string as Property;
                    if ref_test!(to_string.value, Function) {
                        let result: anyref = call_function(to_string.value as Function, target, create_arguments_0(), null);
                        if ref_test!(result, String) {
                            name_str = result as String;
                            offset = get_data_offset_str(name_str.data);
                        } else if ref_test!(result, StaticString) {
                            offset = get_data_offset_static_str(result as StaticString);
                            if offset == 0 {
                                name_str = convert_static_string_to_string(result as StaticString);
                            }
                        } else {
                            name_str = convert_to_string(result);
                            offset = get_data_offset_str(name_str.data);
                        }
                    } else {
                        // toString is not a function, error out
                        throw!(JSException, create_string_from_array("toString is not a function"));
                    }
                }
            }

            if offset == 0 {
                current_string_lookup = name_str;
            }

            set_property(target, offset, property);
        }

        fn set_property(target: anyref, name: i32, property: Property) {
            if ref_test!(target, Object) {
                propertymap_set((target as Object).properties, name, property);
                return;
            }

            if ref_test!(target, Function) {
                propertymap_set((target as Function).properties, name, property);
                return;
            }

            if ref_test!(target, Promise) {
                propertymap_set((target as Promise).properties, name, property);
                return;
            }

            if ref_test!(target, Array) {
                propertymap_set((target as Array).properties, name, property);
                return;
            }

            if ref_test!(target, Generator) {
                propertymap_set((target as Generator).properties, name, property);
                return;
            }

            if ref_test!(target, AsyncGenerator) {
                propertymap_set((target as AsyncGenerator).properties, name, property);
                return;
            }

            if ref_test!(target, GlobalThis) {
                let key: i32 = propertymap_set((target as GlobalThis).properties, name, property);
                declare_variable(global_scope as Scope, key, property.value, VARIABLE_VAR);
                return;
            }

            throw!(JSException, 99101 as i31ref);
        }

        fn set_property_value(target: anyref, name: i32, value: anyref) {
            if ref_test!(target, Array) && name == data!("length") {
                ArraySetLength(target as Array, value);
                return;
            }

            let property: Nullable<Property> = get_own_property(target, name);
            let value_property: Property;
            if ref_test!(property, null) {
                value_property = create_property(value);
                set_property(target, name, value_property);
                return;
            } else {
                value_property = property as Property;
                if value_property.flags & PROPERTY_IS_SETTER != 0 {
                    let accessor: AccessorMethod = value_property.value as AccessorMethod;
                    let function: Function = accessor.set as Function;
                    function.this = target;
                    let arguments: JSArgs = create_arguments_1(value);
                    call_function(function, target, arguments, null);
                } else {
                    value_property.value = value;
                }
                return;
            }

            throw!(JSException, 101 as i31ref);
        }

        fn are_strings_eq(first: String, second: String) -> i32 {
            if first.length != second.length {
                return 0;
            }

            let length: i32 = first.length;
            let i: i32 = 0;
            let mut first_data: I32Array;
            let mut second_data: I32Array;
            while i < length {
                first_data = first.data;
                second_data = second.data;
                if first_data[i] != second_data[i] {
                    return 0;
                }
                i += 1;
            }

            return 1;
        }

        fn get_or_create_interned_string(interner: Interner, name: String) -> i32 {
            let mut entries: InternedStringArray = interner.entries;
            let capacity: i32 = len!(entries);
            let mut entry: InternedString;
            let new_size: i32;
            let new_entries: InternedStringArray;
            let length: i32 = interner.length;

            let i: i32 = 0;
            while i < length {
                if !ref_test!(entries[i], null) {
                    entry = entries[i] as InternedString;
                    if are_strings_eq(entry.value, name) {
                        return entry.offset;
                    }
                }
                i += 1;
            }

            // we haven't found the string we were looking for, so let's add it
            if interner.length >= capacity {
                // there is still capacity
                new_size  = capacity * 2;
                new_entries = [null; new_size];

                // Copy old entries to new array
                i = 0;
                while i < length {
                    new_entries[i] = entries[i];
                    i += 1;
                }

                interner.entries = new_entries;
                entries = new_entries;
            }

            interner.current_offset = interner.current_offset - 1;
            entries[interner.length] = InternedString {
                value: name,
                offset: interner.current_offset
            };
            interner.length = interner.length + 1;

            return interner.current_offset;
        }

        fn propertymap_set_sym(map: PropertyMap, key: Symbol, value: Property) {
            let mut entries: SymPropertyEntriesArray = map.sym_entries;
            let new_entry: SymPropertyMapEntry;
            let mut found: i32 = 0;
            let mut i: i32 = 0;
            let new_size: i32;
            let mut new_entries: SymPropertyEntriesArray;
            let mut entry: SymPropertyMapEntry;
            let len: i32 = len!(entries);
            let mut map_size: i32 = map.sym_size;

            new_entry = SymPropertyMapEntry { key: key, value: value };
            // First, search for existing key
            while i < map_size {
                if ref_test!(entries[i], SymPropertyMapEntry) {
                    entry = entries[i] as SymPropertyMapEntry;
                    if entry.key == key {
                        entry.value = value;
                        found = 1;
                        return;
                    }
                }
                i += 1;
            }

            // If key wasn't found, proceed with insertion
            if found == 0 {
                // Check if we need to resize
                if map_size >= len {
                    new_size  = len * 2;
                    new_entries = [null; new_size];

                    // Copy old entries to new array
                    i = 0;
                    while i < len {
                        new_entries[i] = entries[i];
                        i += 1;
                    }

                    map.sym_entries = new_entries;
                    entries = new_entries;
                }

                // Add new entry and increment size
                entries[map.sym_size] = new_entry;
                map.sym_size = map.sym_size + 1;
            }

            return;
        }

        fn propertymap_set(map: PropertyMap, key_param: i32, value: Property) -> i32 {
            let mut key: i32 = key_param;
            let mut entries: PropertyEntriesArray = map.entries;
            let new_entry: PropertyMapEntry;
            let mut found: i32 = 0;
            let mut i: i32 = 0;
            let new_size: i32;
            let mut new_entries: PropertyEntriesArray;
            let mut entry: PropertyMapEntry;
            let len: i32 = len!(entries);
            let mut map_size: i32 = map.size;

            if key == 0 {
                // if key is 0, it means that we haven't found it in interned strings in memory.
                // for now, in order to not have to rewrite a big part of the code, I decided to
                // save the string we look up as a global
                // TODO: remove the need for a global
                key = get_or_create_interned_string(map.interner, current_string_lookup as String);
            }

            new_entry = PropertyMapEntry { key: key, value: value };
            // First, search for existing key
            while i < map_size {
                if ref_test!(entries[i], PropertyMapEntry) {
                    entry = entries[i] as PropertyMapEntry;
                    if entry.key == key {
                        entry.value = value;
                        found = 1;
                        return key;
                    }
                }
                i += 1;
            }

            // If key wasn't found, proceed with insertion
            if found == 0 {
                // Check if we need to resize
                if map_size >= len {
                    new_size  = len * 2;
                    new_entries = [null; new_size];

                    // Copy old entries to new array
                    i = 0;
                    while i < len {
                        new_entries[i] = entries[i];
                        i += 1;
                    }

                    map.entries = new_entries;
                    entries = new_entries;
                }

                // Add new entry and increment size
                entries[map.size] = new_entry;
                map.size = map.size + 1;
            }

            return key;
        }

        fn get_interned_string_by_key(interner: Interner, key: i32) -> Nullable<String> {
            let mut entries: InternedStringArray = interner.entries;
            let length: i32 = interner.length;
            let mut entry: InternedString;

            let i: i32 = 0;
            while i < length {
                if !ref_test!(entries[i], null) {
                    // TODO: not sure why we would ahve a null here
                    entry = entries[i] as InternedString;

                    if key == entry.offset {
                        return entry.value;
                    }

                }
                i += 1;
            }

            return null as Nullable<String>;
        }

        fn get_interned_string(interner: Interner, name: String) -> i32 {
            let mut entries: InternedStringArray = interner.entries;
            let length: i32 = interner.length;
            let mut entry: InternedString;

            let i: i32 = 0;
            while i < length {
                if !ref_test!(entries[i], null) {
                    // not sure why we would ahve a null here
                    entry = entries[i] as InternedString;
                    if are_strings_eq(entry.value, name) {
                        return entry.offset;
                    }

                }
                i += 1;
            }

            return 0;
        }

        fn propertymap_get_sym(map: PropertyMap, key: Symbol) -> Nullable<Property> {
            let entries: SymPropertyEntriesArray = map.sym_entries;
            let mut i: i32 = 0;

            while i < map.sym_size {
                if entries[i].key == key {
                    return entries[i].value;
                }
                i += 1;
            }

            return null as Nullable<Property>;
        }

        fn propertymap_get(map: PropertyMap, key: i32) -> Nullable<Property> {
            let entries: PropertyEntriesArray = map.entries;
            let mut i: i32 = 0;

            if key == 0 {
                // if key is 0, it means that we haven't found it in interned strings in memory.
                // for now, in order to not have to rewrite a big part of the code, I decided to
                // save the string we look up as a global
                // TODO: remove the need for a global
                key = get_interned_string(map.interner, current_string_lookup as String);
                // there is no such interned string, return null
                if key == 0 {
                    return null as Nullable<Property>;
                }
            }

            while i < map.size {
                if entries[i].key == key {
                    return entries[i].value;
                }
                i += 1;
            }

            return null as Nullable<Property>;
        }

        fn propertymap_delete(map: PropertyMap, key: i32) {
            let entries: PropertyEntriesArray = map.entries;
            let mut i: i32 = 0;

            while i < map.size {
                if entries[i].key == key {
                    entries[i] = null;
                    // shift all the elements and reduce size
                    i += 1;
                    while i < map.size {
                        entries[i - 1] = entries[i];
                        i += 1;
                    }
                    map.size = map.size - 1;
                    return;
                }
                i += 1;
            }
        }

        fn variablemap_get(map: VariableMap, key: i32) -> Nullable<Variable> {
            let entries: EntriesArray = map.entries;
            let mut i: i32 = 0;

            while i < map.size {
                if entries[i].key == key {
                    return entries[i].value;
                }
                i += 1;
            }

            return null;
        }

        fn __await__(p: anyref) -> anyref {
            return null;
        }

        fn __await_drop__() {
        }

        fn __yield__(p: anyref) -> anyref {
            return null;
        }

        fn __yield_drop__() {
        }

        fn call_function(func: anyref, this: anyref, arguments: JSArgs, meta: anyref) -> anyref {
            let function: Function;
            let js_func: JSFunc;
            let current_this: anyref;

            if !ref_test!(func, Function) {
                throw!(JSException, create_string_from_array("TypeError: Can't call a non-function object"));
            }

            function = func as Function;
            js_func = function.func;

            if ref_test!(this, null) {
                current_this = function.this;
            } else {
                current_this = this;
            }

            return js_func(function.scope, current_this, arguments, meta);
        }

        fn add_strings(str1: String, str2: String) -> String {
            let str1_len: i32 = str1.length;
            let str2_len: i32 = str2.length;
            let str1_data: I32Array = str1.data;
            let str2_data: I32Array = str2.data;

            let total_length: i32 = str1_len + str2_len;
            let new_string_data: I32Array = [0; total_length];

            // TODO: replace with array.copy when implemented
            let mut i: i32 = 0;
            while i < str1_len {
                new_string_data[i] = str1_data[i];
                i += 1;
            }

            i = 0;
            while i < str2_len {
                new_string_data[str1_len + i] = str2_data[i];
                i += 1;
            }

            return String {
                data: new_string_data,
                length: total_length
            };
        }

        fn is_string(value: anyref) -> i32 {
            return ref_test!(value, String) || ref_test!(value, StaticString);
        }

        fn add(arg1: anyref, arg2: anyref) -> anyref {
            let str1: String;
            let str2: String;
            let result: f64;

            arg1 = ToPrimitive(arg1, 0);
            arg2 = ToPrimitive(arg2, 0);

            if is_string(arg1) || is_string(arg2) {
                str1 = ToString(arg1);
                str2 = ToString(arg2);

                return add_strings(str1, str2);
            }

            arg1 = ToNumeric(arg1);
            arg2 = ToNumeric(arg2);

            if ref_test!(arg1, Number) && ref_test!(arg2, Number) {
                result = (arg1 as Number).value + (arg2 as Number).value;
                return new_number(result);
            } else if ref_test!(arg1, BigInt) || ref_test!(arg2, BigInt) {
                let error: anyref = create_error(data!("TypeError"), create_string_from_array("Not yet implemented: BigInt add"));
                throw!(JSException, error);
            }

            // TODO: handle BigInt

            let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot mixed different types for addition, use explicit conversion"));
            throw!(JSException, error);
        }

        fn div(arg1: anyref, arg2: anyref) -> anyref {
            arg1 = ToNumeric(arg1);
            arg2 = ToNumeric(arg2);

            if ref_test!(arg1, Number) || ref_test!(arg2, Number) {
                let result: f64 = (arg1 as Number).value / (arg2 as Number).value;
                return new_number(result);
            } else if ref_test!(arg1, BigInt) || ref_test!(arg2, BigInt) {
                let error: anyref = create_error(data!("TypeError"), create_string_from_array("Not yet implemented: BigInt div"));
                throw!(JSException, error);
            }

            let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot mixed different types for division, use explicit conversion"));
            throw!(JSException, error);

        }

        fn sub(arg1: anyref, arg2: anyref) -> anyref {
            arg1 = ToNumeric(arg1);
            arg2 = ToNumeric(arg2);

            if ref_test!(arg1, Number) || ref_test!(arg2, Number) {
                let result: f64 = (arg1 as Number).value - (arg2 as Number).value;
                return new_number(result);
            } else if ref_test!(arg1, BigInt) || ref_test!(arg2, BigInt) {
                let error: anyref = create_error(data!("TypeError"), create_string_from_array("Not yet implemented: BigInt sub"));
                throw!(JSException, error);
            }

            let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot mixed different types for subtraction, use explicit conversion"));
            throw!(JSException, error);
        }

        fn mul(arg1: anyref, arg2: anyref) -> anyref {
            arg1 = ToNumeric(arg1);
            arg2 = ToNumeric(arg2);

            if ref_test!(arg1, Number) || ref_test!(arg2, Number) {
                let result: f64 = (arg1 as Number).value * (arg2 as Number).value;
                return new_number(result);
            } else if ref_test!(arg1, BigInt) || ref_test!(arg2, BigInt) {
                let error: anyref = create_error(data!("TypeError"), create_string_from_array("Not yet implemented: BigInt mul"));
                throw!(JSException, error);
            }

            let error: anyref = create_error(data!("TypeError"), create_string_from_array("Cannot mixed different types for multiplication, use explicit conversion"));
            throw!(JSException, error);
        }

        fn mod_op(arg1: anyref, arg2: anyref) -> anyref {
            if ref_test!(arg1, Number) && ref_test!(arg2, Number) {
                let num1: Number = arg1 as Number;
                let num2: Number = arg2 as Number;
                // TODO: this is wrong as in JS you can run modulo on floats, but for now I want to
                // handle at least integers
                let int1: i64 = num1.value as i64;
                let int2: i64 = num2.value as i64;
                let modulo_result: i64 = int1 % int2;
                let result: f64 = modulo_result as f64;
                return new_number(result);
            }
            return null;
        }

        fn strict_not_equal(arg1: anyref, arg2: anyref) -> i31ref {
            return !strict_equal(arg1, arg2);
        }

        fn loose_not_equal(arg1: anyref, arg2: anyref) -> i31ref {
            return !loose_equal(arg1, arg2);
        }

        fn operator_in(arg1: anyref, arg2: anyref) -> i31ref {
            let property: Nullable<Property>;
            let mut is_wrapped_string: i32 = 0;
            let mut index_num: Number = new_number(0 as f64);
            let index: i32;
            let array: Array = create_empty_array();
            let mut string: String = create_empty_string();
            let array_data: AnyrefArray = [null; 0];
            let string_data: I32Array = [0; 0];

            if ref_test!(arg2, Array) {
                array = arg2 as Array;
                array_data = array.array;
                index_num = ToNumber(arg1);

                if !is_nan(index_num) && !is_infinity(index_num) && is_integer(index_num) {
                    index = index_num.value as i32;

                    if index > array.length || index < 0 {
                        return 0 as i31ref;
                    }

                    return is_array_element_available(array, index) as i31ref;
                }
            } else if ref_test!(arg2, StaticString) {
                arg2 = convert_static_string_to_string(arg2 as StaticString);
            } else if ref_test!(arg2, Object) {
                if ref_test!((arg2 as Object).value, StaticString) {
                    // normalize
                    (arg2 as Object).value = convert_static_string_to_string((arg2 as Object).value as StaticString);
                    is_wrapped_string = 1;
                } else if ref_test!((arg2 as Object).value, String) {
                    is_wrapped_string = 1;
                }
            }

            if ref_test!(arg2, String) || is_wrapped_string {
                if is_wrapped_string {
                    string = (arg2 as Object).value as String;
                } else {
                    string = arg2 as String;
                }

                string_data = string_data;
                index_num = ToNumber(arg1);

                if !is_nan(index_num) && !is_infinity(index_num) && is_integer(index_num) {
                    index = index_num.value as i32;

                    if index > string.length || index < 0 {
                        return 0 as i31ref;
                    }

                    return 1 as i31ref;
                }
            }

            let key: anyref = ToPropertyKey(arg1);
            if ref_test!(key, Symbol) {
                property = get_property_sym(arg2, key as Symbol);
            } else {
                property = get_property_str(arg2, key);
            }

            if ref_test!(property, null) {
                return 0 as i31ref;
            }

            return 1 as i31ref;
        }

        fn bind_this(function: Function, this: anyref) -> Function {
            function.this = this;
            return function;
        }

        fn strict_equal(arg1: anyref, arg2: anyref) -> i31ref {
            let mut i: i32 = 0;
            let mut len: i32 = 0;
            let str1: String;
            let str2: String;
            let static_str1: StaticString;
            let static_str2: StaticString;
            let char_array1: I32Array;
            let char_array2: I32Array;
            let offset: i32;
            let result: i32;

            if ref_test!(arg1, null) && ref_test!(arg2, null) {
                return 1 as i31ref;
            }

            if ref_test!(arg1, Symbol) || ref_test!(arg2, Symbol) {
                return (arg1 as Symbol == arg2 as Symbol) as i31ref;
            }

            if ref_test!(arg1, null) || ref_test!(arg2, null) {
                return 0 as i31ref;
            }

            if ref_test!(arg1, Number) && ref_test!(arg2, Number) {
                let num1: Number = arg1 as Number;
                let num2: Number = arg2 as Number;
                return (num1.value == num2.value) as i31ref;
            }

            if ref_test!(arg1, i31ref) && ref_test!(arg2, i31ref) {
                return ((arg1 as i31ref) == (arg2 as i31ref)) as i31ref;
            }

            if ref_test!(arg1, String) && ref_test!(arg2, String) {
                str1 = arg1 as String;
                str2 = arg2 as String;

                if str1.length != str2.length {
                    return 0 as i31ref;
                }
                len = str1.length;
                char_array1 = str1.data;
                char_array2 = str2.data;
                while i < len {
                    if char_array1[i] != char_array2[i] {
                        return 0 as i31ref;
                    }
                    i += 1;
                }

                return 1 as i31ref;
            }

            if ref_test!(arg1, StaticString) && ref_test!(arg2, StaticString) {
                static_str1 = arg1 as StaticString;
                static_str2 = arg2 as StaticString;

                if static_str1.offset != static_str2.offset {
                    return 0 as i31ref;
                }
                return 1 as i31ref;
            }

            if ref_test!(arg1, String) && ref_test!(arg2, StaticString) {
                str1 = arg1 as String;
                static_str2 = arg2 as StaticString;

                return compare_string_and_static_string(str1, static_str2) as i31ref;
            }

            if ref_test!(arg1, StaticString) && ref_test!(arg2, String) {
                static_str1 = arg1 as StaticString;
                str2 = arg2 as String;

                if static_str1.length != str2.length {
                    return 0 as i31ref;
                }
                len = static_str1.length;
                offset = static_str1.offset;
                char_array2 = str2.data;
                while i < len {
                    if memory::<u16>[offset + (i * 2)] != char_array2[i] {
                        return 0 as i31ref;
                    }
                    i += 1;
                }

                return 1 as i31ref;
            }

            if ref_test!(arg1, Function) && ref_test!(arg2, Function) {
                result = arg1 as Function == arg2 as Function;
                return result as i31ref;
            }

            return 0 as i31ref;
        }

        fn compare_string_range_and_static_string(str: String, start: i32, end: i32, static_str: StaticString) -> i32 {
            let char_array: I32Array;
            let len: i32;
            let static_str_len: i32 = static_str.length;
            let offset: i32;
            let mut i: i32;
            len = end - start;

            if len != static_str.length {
                return 0;
            }
            offset = static_str.offset;
            char_array = str.data;
            while i <= end {
                if char_array[i] != memory::<u16>[offset + (i * 2)] {
                    return 0;
                }
                i += 1;
            }

            return 1;
        }

        fn compare_string_and_static_string(str: String, static_str: StaticString) -> i32 {
            let char_array: I32Array;
            let len: i32;
            let offset: i32;
            let mut i: i32;

            if str.length != static_str.length {
                return 0;
            }
            len = str.length;
            offset = static_str.offset;
            char_array = str.data;
            while i < len {
                if char_array[i] != memory::<u16>[offset + (i * 2)] {
                    return 0;
                }
                i += 1;
            }

            return 1;
        }

        fn is_a_string(arg: anyref) -> i32 {
            return ref_test!(arg, String) || ref_test!(arg, StaticString);
        }

        fn is_empty(arg: anyref) -> i32 {
            if !ref_test!(arg, null) && ref_test!(arg, i31ref) {
                return (arg as i31ref == 3);
            }

            return 0;
        }

        fn is_null(arg: anyref) -> i32 {
            if ref_test!(arg, i31ref) {
                return (arg as i31ref == 2);
            }

            return 0;
        }

        fn is_boolean(value: anyref) -> i32 {
            if ref_test!(value, null) {
                return 0;
            }

            if ref_test!(value, i31ref) {
                return (value as i31ref) == 0 || (value as i31ref) == 1;
            }
            return 0;
        }

        fn is_true(arg: anyref) -> i32 {
            if ref_test!(arg, null) {
                return 0;
            }

            if ref_test!(arg, i31ref) {
                return (arg as i31ref == 1);
            }

            return 0;
        }

        fn is_false(arg: anyref) -> i32 {
            if ref_test!(arg, null) {
                return 0;
            }

            if ref_test!(arg, i31ref) {
                return (arg as i31ref == 0);
            }

            return 0;
        }

        fn is_null_or_undefined(arg: anyref) -> i32 {
            if ref_test!(arg, null) {
                return 1;
            }

            if ref_test!(arg, i31ref) {
                return (arg as i31ref == 2);
            }

            return 0;
        }

        fn not_use_equal(arg1: anyref, arg2: anyref) -> i31ref {
            return !loose_equal(arg1, arg2);
        }

        fn loose_equal(arg1: anyref, arg2: anyref) -> i31ref {
            if (ref_test!(arg1, null) && ref_test!(arg2, null)) ||
               (is_a_string(arg1) && is_a_string(arg2)) ||
               (ref_test!(arg1, Number) && ref_test!(arg2, Number)) {
                return strict_equal(arg1, arg2);
            }

            if is_null_or_undefined(arg1) && is_null_or_undefined(arg2) {
                return 1 as i31ref;
            }

            if ref_test!(arg1, Number) && is_a_string(arg2) {
                return loose_equal(arg1, ToNumber(arg2));
            }

            if is_a_string(arg1) && ref_test!(arg2, Number) {
                return loose_equal(ToNumber(arg1), arg2);
            }

            if is_boolean(arg1) {
                return loose_equal(ToNumber(arg1), arg2);
            }

            if is_boolean(arg2) {
                return loose_equal(arg1, ToNumber(arg2));
            }

            if (is_a_string(arg1) || ref_test!(arg1, Number) || ref_test!(arg1, Symbol) || ref_test!(arg1, BigInt)) && !is_primitive(arg2) {
                return loose_equal(arg1, ToPrimitive(arg2, 0));
            }

            if !is_primitive(arg1) && (is_a_string(arg2) || ref_test!(arg2, Number) || ref_test!(arg2, Symbol) || ref_test!(arg2, BigInt)) {
                return loose_equal(ToPrimitive(arg1, 0), arg2);
            }

            // TODO handle numbers + bigints

            return 0 as i31ref;
        }

        fn set_own_prototype(target: anyref, prototype: anyref) {
             if ref_test!(target, Object) {
                return (target as Object).own_prototype = prototype;
            } else if ref_test!(target, Function) {
                return (target as Function).own_prototype = prototype;
            } else if ref_test!(target, Promise) {
                return (target as Promise).own_prototype = prototype;
            } else if ref_test!(target, Generator) {
                return (target as Generator).own_prototype = prototype;
            } else if ref_test!(target, AsyncGenerator) {
                return (target as AsyncGenerator).own_prototype = prototype;
            } else if ref_test!(target, Array) {
                return (target as Array).own_prototype = prototype;
            }// else if ref_test!(target, GlobalThis) {
            //     return (target as GlobalThis).own_prototype = prototype;
            // }
        }

        fn get_own_prototype(target: anyref) -> anyref {
             if ref_test!(target, Object) {
                return (target as Object).own_prototype;
            } else if ref_test!(target, Function) {
                return (target as Function).own_prototype;
            } else if ref_test!(target, Promise) {
                return (target as Promise).own_prototype;
            } else if ref_test!(target, Generator) {
                return (target as Generator).own_prototype;
            } else if ref_test!(target, AsyncGenerator) {
                return (target as AsyncGenerator).own_prototype;
            } else if ref_test!(target, Array) {
                return global_array_prototype;
            } else if ref_test!(target, GlobalThis) {
                return (target as GlobalThis).own_prototype;
            } else if ref_test!(target, Number) {
                return global_number_prototype;
            } else if ref_test!(target, BigInt) {
                return global_bigint_prototype;
            } else if ref_test!(target, String) {
                return global_string_prototype;
            } else if ref_test!(target, StaticString) {
                return global_string_prototype;
            }

            return null;
        }

        fn instance_of(instance: anyref, constructor: anyref) -> i31ref {
            if !ref_test!(constructor, Function) {
                return 0 as i31ref;
            }

            let target_prototype_anyref: anyref = get_property_value(constructor, data!("prototype"));
            // is it possible to define a local of ref type and not assign anything right away?
            let mut target_prototype: Object = create_object();
            let mut prototype: anyref = null;

            if ref_test!(target_prototype_anyref, Object) {
                target_prototype = target_prototype_anyref as Object;
            } else {
                return 0 as i31ref;
            }

            prototype = get_own_prototype(instance);

            while !ref_test!(prototype, null) {
                if prototype as Object == target_prototype {
                    return 1 as i31ref;
                }

                prototype = get_own_prototype(prototype);
            }
            return 0 as i31ref;
        }

        fn logical_or(arg1: anyref, arg2: anyref) -> anyref {
            let bool1: i31ref = ToBoolean(arg1);
            let bool2: i31ref = ToBoolean(arg2);

            if is_false(bool1) {
                return arg2;
            }

            if is_false(bool2) {
                return arg1;
            }

            return arg1;
        }

        fn logical_and(arg1: anyref, arg2: anyref) -> anyref {
            let bool1: i31ref = ToBoolean(arg1);
            let bool2: i31ref = ToBoolean(arg2);

            if is_false(bool1) {
                return arg1;
            }

            if is_false(bool2) {
                return arg2;
            }

            return arg2;
        }

        fn logical_not(arg: anyref) -> i31ref {
            let bool: i31ref = ToBoolean(arg);

            if is_true(bool) {
                return 0 as i31ref;
            }

            return 1 as i31ref;
        }

        fn type_of(arg: anyref) -> StaticString {
            if ref_test!(arg, null) {
                return new_static_string(data!("undefined"), 9);
            }

            if ref_test!(arg, i31ref) {
                let val: i32 = arg as i31ref as i32;
                if val == 0 || val == 1 {
                    return new_static_string(data!("boolean"), 7);
                }
                return new_static_string(data!("undefined"), 9);
            }

            if ref_test!(arg, Number) {
                return new_static_string(data!("number"), 6);
            }

            if ref_test!(arg, Object) || ref_test!(arg, Promise) || ref_test!(arg, Array) || ref_test!(arg, Generator) || ref_test!(arg, AsyncGenerator) {
                return new_static_string(data!("object"), 6);
            }

            if ref_test!(arg, StaticString) {
                return new_static_string(data!("string"), 6);
            }

            if ref_test!(arg, Function) {
                return new_static_string(data!("function"), 8);
            }

            return new_static_string(data!("undefined"), 9);
        }

        fn IsStringPrefix(prefix: String, str: String) -> i32 {
            let prefix_len: i32 = prefix.length as i32;
            let str_len: i32 = str.length as i32;
            let prefix_data: I32Array = prefix.data;
            let str_data: I32Array = str.data;

            if prefix_len > str_len {
                return 0;
            }

            let i: i32 = 0;
            while i < prefix_len {
                if prefix_data[i] != str_data[i] {
                    return 0;
                }
                i += 1;
            }

            return 1;
        }

        // https://262.ecma-international.org/13.0/#sec-islessthan
        fn IsLessThan(x: anyref, y: anyref, left_first: i32) -> i32 {
            let mut px: anyref;
            let mut py: anyref;
            let mut strx: String = create_empty_string();
            let mut stry: String = create_empty_string();
            let mut strx_data: I32Array = [0; 0];
            let mut stry_data: I32Array = [0; 0];
            let mut i: i32;
            let mut stop: i32;
            let mut bi: Nullable<BigInt>;
            let mut nx: anyref;
            let mut ny: anyref;

            if left_first {
                px = ToPrimitive(x, TO_PRIMITIVE_NUMBER);
                py = ToPrimitive(y, TO_PRIMITIVE_NUMBER);
            } else {
                py = ToPrimitive(y, TO_PRIMITIVE_NUMBER);
                px = ToPrimitive(x, TO_PRIMITIVE_NUMBER);
            }

            if ref_test!(px, String) && ref_test!(py, String) {
                strx = px as String;
                stry = py as String;
                if IsStringPrefix(stry, strx) {
                    return 0;
                }
                if IsStringPrefix(strx, stry) {
                    return 1;
                }
                strx_data = strx.data;
                stry_data = stry.data;
                i = 0;
                stop = min_i32(strx.length, stry.length);
                while i < stop {
                    if strx_data[i] != stry_data[i] {
                        return strx_data[i] < stry_data[i];
                    }
                    i += 1;
                }
                throw!(JSException, create_string_from_array("IsLessThan string comparison: this point should be unreachable"));
            }

            if ref_test!(px, BigInt) && ref_test!(py, String) {
                // TODO: this should be StringToBigInt, and it should be able to return undefined
                // rather than throw
                bi = ToBigInt(py);
                if ref_test!(bi, null) {
                    return 0;
                }

                throw!(JSException, create_string_from_array("not implemented: BigInt::lessThan"));
            }

            if ref_test!(px, String) && ref_test!(py, BigInt) {
                // TODO: this should be StringToBigInt, and it should be able to return undefined
                // rather than throw
                bi = ToBigInt(px);
                if ref_test!(bi, null) {
                    return 0;
                }

                throw!(JSException, create_string_from_array("not implemented: BigInt::lessThan"));
            }

            nx = ToNumeric(px);
            ny = ToNumeric(py);

            if ref_test!(nx, Number) && ref_test!(ny, Number) {
                return Number_lessThan(nx as Number, ny as Number);
            }

            throw!(JSException, create_string_from_array("not implemented: comparing bigints and numbers"));
        }

        fn Number_lessThan(x: Number, y: Number) -> i32 {
            // The spec says that we should return undefined, but browsers return false, so let's
            // simplify that too
            if is_nan(x) || is_nan(y) {
                return 0;
            }

            if x.value == f64::INFINITY {
                return 0;
            }

            if y.value == f64::INFINITY {
                return 1;
            }

            if y.value == f64::NEG_INFINITY {
                return 0;
            }

            if x.value == f64::NEG_INFINITY {
                return 1;
            }

            return x.value < y.value;
        }

        fn less_than_or_equal(arg1: anyref, arg2: anyref) -> i31ref {
            // a <= b is the same as !(a > b)
            let result: i32 = !IsLessThan(arg2, arg1, 0);
            return result as i31ref;
        }

        fn less_than(arg1: anyref, arg2: anyref) -> i31ref {
            let result: i32 = IsLessThan(arg1, arg2, 1);
            return result as i31ref;
        }

        fn greater_than(arg1: anyref, arg2: anyref) -> i31ref {
            let result: i32 = IsLessThan(arg2, arg1, 0);
            return result as i31ref;
        }

        fn greater_than_or_equal(arg1: anyref, arg2: anyref) -> i31ref {
            // a >= b is the same as !(a < b)
            let result: i32 = !IsLessThan(arg1, arg2, 1);
            return result as i31ref;
        }

        fn increment_number(arg1: anyref) -> anyref {
            if ref_test!(arg1, Number) {
                let num: Number = arg1 as Number;
                let result: f64 = num.value + 1.0;
                return new_number(result);
            }
            return null;
        }

        // TODO: can we update in-place?
        fn decrement_number(arg1: anyref) -> anyref {
            if ref_test!(arg1, Number) {
                let num: Number = arg1 as Number;
                let result: f64 = num.value - 1.0;
                return new_number(result);
            }
            return null;
        }

        // Function to convert little-endian UTF-16 to UTF-8
        // Parameters:
        //   $src_offset: pointer to UTF-16 encoded string (little-endian)
        //   $src_length: length of UTF-16 string in bytes
        //   $dst_offset: pointer to output buffer for UTF-8 string
        // Returns:
        //   Length of the resulting UTF-8 string in bytes
        fn utf16le_to_utf8(src_offset: i32, src_length: i32, dst_offset: i32) -> i32 {
            let mut i: i32 = 0;
            let mut j: i32 = 0;
            let mut code_unit: i32;
            let mut code_point: i32;
            let mut high_surrogate: i32;

            while i < src_length {
                // load current code unit
                code_unit = memory::<u16>[src_offset + i];
                i = i + 2;
                if code_unit >= 0xD800 && code_unit <= 0xDBFF {
                    // code unit is a high surrogate
                    high_surrogate = code_unit;
                    code_unit = memory::<u16>[src_offset + i];
                    i = i + 2;

                    code_point = ((high_surrogate - 0xD800) << 10) + (code_unit - 0xDC00) + 0x10000;
                } else {
                    code_point = code_unit;
                }

                if code_point < 0x80 {
                    // 1 byte: 0xxxxxxx
                    memory::<i8>[dst_offset + j] = code_point;
                    j += 1;
                } else if code_point < 0x800 {
                    // 2 bytes: 110xxxxx 10xxxxxx
                    memory::<i8>[dst_offset + j] = 0xC0 | (code_point >> 6);
                    memory::<i8>[dst_offset + j + 1] = 0x80 | (code_point & 0x3F);
                    j += 2;
                } else if code_point < 0x10000 {
                    // 3 bytes: 1110xxxx 10xxxxxx 10xxxxxx
                    memory::<i8>[dst_offset + j] = 0xE0 | (code_point >> 12);
                    memory::<i8>[dst_offset + j + 1] = 0x80 | ((code_point >> 6) & 0x3F);
                    memory::<i8>[dst_offset + j + 2] = 0x80 | (code_point & 0x3F);
                    j += 3;
                } else {
                    // 4 bytes: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                    memory::<i8>[dst_offset + j] = 0xF0 | (code_point >> 18);
                    memory::<i8>[dst_offset + j + 1] = 0x80 | ((code_point >> 12) & 0x3F);
                    memory::<i8>[dst_offset + j + 2] = 0x80 | ((code_point >> 6) & 0x3F);
                    memory::<i8>[dst_offset + j + 3] = 0x80 | (code_point & 0x3F);
                    j += 4;
                }
            }

            return j;
        }

        fn writeF64AsAscii(value: f64, offset: i32) -> i32 {
            let mut current_offset: i32 = offset;
            let mut remaining_value: f64 = value;
            let divisor: f64 = 10.0;

            if value == f64::INFINITY {
                memory[offset] = 'I';
                memory[offset + 1] = 'n';
                memory[offset + 2] = 'f';
                memory[offset + 3] = 'i';
                memory[offset + 4] = 'n';
                memory[offset + 5] = 'i';
                memory[offset + 6] = 't';
                memory[offset + 7] = 'y';
                return 8;
            } else if value == f64::NEG_INFINITY {
                memory[offset] = '-';
                memory[offset + 1] = 'I';
                memory[offset + 2] = 'n';
                memory[offset + 3] = 'f';
                memory[offset + 4] = 'i';
                memory[offset + 5] = 'n';
                memory[offset + 6] = 'i';
                memory[offset + 7] = 't';
                memory[offset + 8] = 'y';
                return 9;
            } else if value != value {
                memory[offset] = 'N';
                memory[offset + 1] = 'a';
                memory[offset + 2] = 'N';
                return 3;
            }

            // Handle negative numbers
            if value < 0.0 {
                memory::<i8>[current_offset] = '-';
                current_offset += 1;
                remaining_value = -value;
            }

            // Handle zero
            if value == 0.0 {
                memory::<i8>[current_offset] = '0';
                return current_offset - offset + 1;
            }

            let mut exponent: i32 = 0;

            if remaining_value >= 1e21 || remaining_value < 1e-6 {
                return write_in_exponential_notation(remaining_value, current_offset);
            }

            // Split into integer and fractional parts
            let int_part: f64 = trunc!(remaining_value);
            let frac_part: f64 = remaining_value - int_part;

            // Convert integer part
            let mut int_digits: I32Array = [0; 20];  // Max digits for f64
            let mut int_count: i32 = 0;
            let mut int_val: f64 = int_part;
            let mut divided: f64;
            let mut truncated: f64;
            let mut digit_value: i32;
            let mut truncated_times_divisor: f64;

            let digit: i32;
            while int_val >= 1.0 {
                divided = int_val / divisor;
                truncated = trunc!(divided); // Floor the division
                truncated_times_divisor = truncated * divisor;
                digit_value = (trunc!(int_val) - trunc!(truncated_times_divisor)) as i32; // Last digit
                int_digits[int_count] = '0' + digit_value;
                int_count += 1;
                int_val = truncated; // Remove the last digit
            }

            // Write integer part in correct order
            let mut i: i32 = int_count - 1;
            while i >= 0 {
                memory::<i8>[current_offset] = int_digits[i];
                current_offset += 1;
                i -= 1;
            }

            // Handle fractional part if present
            if frac_part > 0.0 {
                memory::<i8>[current_offset] = '.';
                current_offset += 1;

                let mut remaining_frac: f64 = frac_part;
                let mut precision: i32 = 0;

                // Write up to 16 decimal places
                while (remaining_frac > 0.0) && (precision < 16) {
                    remaining_frac = remaining_frac * 10.0;
                    let digit = remaining_frac as i32;
                    memory::<i8>[current_offset] = ('0' as i32 + digit as i32);
                    current_offset = current_offset + 1;
                    remaining_frac = remaining_frac - digit as f64;
                    precision = precision + 1;
                }

                // walk back to remove trailing zeroes
                while memory::<i8>[current_offset - 1] == '0' {
                    current_offset -= 1;
                }
            }

            return current_offset - offset;
        }

        fn write_in_exponential_notation(value: f64, offset: i32) -> i32 {
            let mut exponent: i32 = 0;

            // Normalize value to 1.0 <= value < 10.0
            while value >= 10.0 {
                value = value / 10.0;
                exponent += 1;
            }
            while value < 1.0 && value != 0.0 {
                value = value * 10.0;
                exponent -= 1;
            }

            let mut chars_written: i32 = writeF64AsAscii(value, offset);

            // Write 'e' and exponent
            memory::<i8>[offset + chars_written] = 'e';
            chars_written += 1;

            chars_written += writeF64AsAscii(exponent as f64, offset + chars_written);
            return chars_written;
        }

        fn log_string(str: StaticString) {
            let args: JSArgs = create_arguments_1(str);
            log(args);
        }

        fn log(arguments: JSArgs) {
            let len: i32 = len!(arguments);
            let mut iovectors_offset: i32 = free_memory_offset;
            let original_offset: i32 = free_memory_offset;
            let mut offset: i32 = free_memory_offset + (len * 16);
            let mut handled: i32;
            let mut current: anyref;
            let mut val: i32;
            let mut num_val: f64;
            let mut written_length: i32;
            let mut number: Number;
            let mut static_str: StaticString;
            let mut str: String;
            let mut str_len: i32 = 0;
            let mut str_utf8_len: i32 = 0;
            let mut j: i32;
            let mut str_data: I32Array;

            let mut i: i32 = 0;
            while i < len {
                current = arguments[i];
                handled = 0;

                if ref_test!(current, BigInt) {
                    current = BigIntToString(current as BigInt, 10);
                    current = add_strings(current as String, create_string_from_array("n"));
                }

                if ref_test!(current, StaticString) {
                    static_str = current as StaticString;
                    str_utf8_len = utf16le_to_utf8(static_str.offset, static_str.length * 2, offset + static_str.length * 2);
                    memory[iovectors_offset] = offset + static_str.length * 2;
                    memory[iovectors_offset + 4] = str_utf8_len;
                    // we wrote to memory, now we have to advance the offset
                    offset += static_str.length * 2 + str_utf8_len;
                    free_memory_offset = offset;
                    iovectors_offset += 8;
                    handled = 1;
                } else {
                    str = ToString(current);
                    str_len = str.length;

                    // Copy string data to memory
                    j = 0;
                    str_data = str.data;
                    while j < str_len {
                        memory::<u16>[offset + (j * 2)] = str_data[j];
                        j += 1;
                    }

                    str_utf8_len = utf16le_to_utf8(offset, str_len * 2, offset + str_len * 2);

                    memory[iovectors_offset] = offset + str_len * 2;
                    memory[iovectors_offset + 4] = str_utf8_len;

                    offset += str_len * 2 + str_utf8_len;
                    free_memory_offset = offset;

                    iovectors_offset += 8;
                    handled = 1;
                }

                // Add space between arguments (except for last one)
                if i < len - 1 {
                    memory[iovectors_offset] = data!(" ");
                    memory[iovectors_offset + 4] = 1;
                    iovectors_offset += 8;
                }

                i += 1;
            }

            // Add newline at the end
            memory[iovectors_offset] = data!("\n");
            memory[iovectors_offset + 4] = 1;

            // Write to stdout
            write(1, original_offset, len * 2, 50);

            free_memory_offset = original_offset;
        }

        fn add_pollable(pollable: Pollable) -> i32 {
            let mut len: i32 = len!(pollables);

            let mut i: i32 = 0;
            while i < len {
                if ref_test!(pollables[i], null) {
                    // log(create_arguments_2(create_string_from_array("added pollable index:"), new_number(i as f64)));
                    // log(create_arguments_2(create_string_from_array("added pollable isNull:"), new_number(ref_test!(pollable, null) as f64)));
                    pollables[i] = pollable;
                    return pollable.id;
                }

                i += 1;
            }

            // No free space found, double array size
            let old_pollables: PollablesArray = pollables;
            pollables = [null; len * 2];

            // TODO: Replace with array.copy when implemented
            i = 0;
            while i < len {
                pollables[i] = old_pollables[i];
                i += 1;
            }

            return add_pollable(pollable);
        }

        fn find_pollable(id: i32) -> i32 {
            let len: i32 = len!(pollables);

            let mut i: i32 = 0;
            while i < len {
                let current: Nullable<Pollable> = pollables[i];
                if ref_test!(current, null) == 0 {
                    if current.id == id {
                        return i;
                    }
                }

                i += 1;
            }

            return -1;
        }

        fn execute_pollables(offset: i32) {
            let len: i32 = memory[offset];
            let mut current_offset: i32 = offset + 4;
            let mut index: i32;
            let func: anyref;

            let mut i: i32 = 0;
            while i < len {
                index = find_pollable(memory[current_offset]);

                if index != -1 {
                    func = pollables[index].func;
                    if ref_test!(func, Function) {
                        // TODO: handle this differently for arrow functions or binding
                        call_function(func, null, [], null);
                        return;
                    }
                    // TODO: handle async variant
                    throw!(JSException, 20002 as i31ref);
                }

                current_offset += 4;
                i += 1;
            }
        }

        fn clear_pollables(offset: i32) {
            let len: i32 = memory[offset];
            let mut current_offset: i32 = offset + 4;
            let mut index: i32;

            // log(create_arguments_2(create_string_from_array("clearing pollables, len:"), new_number(len as f64)));
            let mut i: i32 = 0;
            while i < len {
                 index = find_pollable(memory[current_offset]);

                if index != -1 {
                    // log(create_arguments_2(create_string_from_array("clearing pollable with index"), new_number(index as f64)));
                    pollables[index] = null;
                }

                current_offset += 4;
                i += 1;
            }
        }

        fn store_pollables(offset: i32) -> i32 {
            let len: i32 = len!(pollables);
            let mut current_offset: i32 = offset;
            let mut stored_length: i32 = 0;
            let mut current: Nullable<Pollable> = null;

            let mut i: i32 = 0;
            while i < len {
                current = pollables[i];
                if ref_test!(current, null) == 0 {
                    memory[current_offset] = current.id;
                    current_offset += 4;
                    stored_length += 1;
                }
                i += 1;
            }

            return stored_length;
        }

        fn set_timeout(func: anyref, duration_arg: anyref) -> Number {
            let duration: i64;
            if ref_test!(func, Function) {
                if ref_test!(duration_arg, Number) {
                    // Convert ms to ns
                    duration = (duration_arg as Number).value as i64 * 1000000;
                } else {
                    duration = 0;
                }

                let id: i32 = subscribe_duration(duration);
                let pollable_id: i32 = add_pollable(new_pollable(id, func));
                // log(create_arguments_1(create_string_from_array("added pollable for a timeout")));
                return new_number(pollable_id as f64);
            }

            throw!(JSException, 20001 as i31ref);
        }

        fn try_i31ref(r: anyref) -> i31ref {
            if ref_test!(r, i31ref) {
                return r as i31ref;
            }
            let result: i32 = -1;
            return result as i31ref;
        }

        fn op_minus(arg: anyref) -> Number {
            // TODO: convert arg to primitive
            let num: f64 = (arg as Number).value;
            return new_number(-num);
        }

        // fn to_string(target: anyref) -> String {
        //     let static_str: StaticString;
        //     let result: anyref;
        //     if ref_test!(target, String) {
        //         return target as String;
        //     } else if ref_test!(target, StaticString) {
        //         static_str = target as StaticString;
        //         // If we only return static strings for stuff that is in memory, maybe
        //         // we could use it directly for fetching props
        //         return memory_to_string(static_str.offset, static_str.length);
        //     }
        //
        //     // TODO: handle the case where we don't get anything
        //     let to_string_func: Function = (get_property(target, data!("toString")) as Property).value as Function;
        //     // TODO: handle a case when toString does not return a String
        //     let result = call_function(to_string_func, target, create_arguments_0()), null;
        //
        //     if ref_test!(result, StaticString) {
        //         return memory_to_string((result as StaticString).offset, (result as StaticString).length);
        //     } else if ref_test!(result, String) {
        //         return result as String;
        //     }
        //
        //     throw!(JSException, 2222222 as i31ref);
        // }

        fn offset_to_string(target_offset: i32) -> Nullable<String> {
            let mut offset: i32 = data_offsets_offset;
            let mut current_offset: i32;
            let mut current_length: i32;
            let length: i32 = memory[offset];
            let mut i: i32 = 0;
            let mut j: i32;
            let result: String;
            let data: I32Array;

            offset += 4;

            while i < length {
                current_offset = memory[offset];
                current_length = memory[offset + 4];

                if current_offset == target_offset {
                    data = [0; current_length];
                    j = 0;
                    while j < current_length {
                        data[j] = memory::<u16>[current_offset + (j * 2)];
                        j+=1;
                    }

                    return String {
                        data: data,
                        length: current_length
                    };

                }

                offset += 8;
                i += 1;
            }

            return null as Nullable<String>;
        }

        fn get_data_offset_static_str(str: StaticString) -> i32 {
            let mut offset: i32 = data_offsets_offset;
            let str_offset: i32 = str.offset;
            let str_length: i32 = str.length;
            let mut current_offset: i32;
            let mut current_length: i32;
            let length: i32 = memory[offset];
            let mut i: i32 = 0;
            let mut j: i32;
            let mut found: i32 = 0;

            offset += 4;

            while i < length {
                current_offset = memory[offset];
                current_length = memory[offset + 4];

                if str_length == current_length {
                    j = 0;
                    found = 1;
                    while j < str_length {
                        if memory::<u16>[str_offset + j] != memory::<u16>[current_offset + j] {
                            found = 0;
                            j = str_length; // poor man's break
                        }

                        j += 2;
                    }

                    if found {
                        return str_offset;
                    }
                }

                i += 1;
                offset += 8;
            }

            // we can return 0 as we don't store anything at offset 0
            return 0;
        }

        fn get_data_offset_str(str: I32Array) -> i32 {
            let mut offset: i32 = data_offsets_offset;
            let length: i32 = memory[offset];
            let mut i: i32 = 0;
            let mut str_offset: i32;
            let mut str_length: i32;
            let mut target_length: i32 = len!(str);
            let mut j: i32;
            let mut found: i32 = 0;

            offset += 4;

            while i < length {
                str_offset = memory[offset];
                str_length = memory[offset + 4];

                if str_length == target_length {
                    j = 0;
                    found = 1;
                    while j < str_length {
                        if memory::<u16>[str_offset + (j * 2)] != str[j] {
                            found = 0;
                            j = str_length; // poor man's break
                        }

                        j += 1;
                    }

                    if found {
                        return str_offset;
                    }
                }

                i += 1;
                offset += 8;
            }

            // we can return 0 as we don't store anything at offset 0
            return 0;
        }

        // This is not how the run loop will run in the future. `poll_many`
        // is supposed to wait for the next pollable to resolve, thus blocking
        // the execution. In order to do that on the host there has to be a way
        // to call async functions. The problem is that passing an async function
        // as an WebAssembly import doesn't await it. There are ways around it,
        // for example to use asyncify, but `wasm-opt` with asyncify crashes
        // when trying to optimize the project (I'm guessing due to exception handling
        // or GC proposals). So instead of running in a loop (like the commented code would suggest)
        // we're calling poll_many and the poll_many polyfill will call main_loop again
        //
        // This should be fixed once th code can be executed on a runtime with proper
        // WASI preview2 support
        #[export("main_loop")]
        fn main_loop() {
            let offset: i32 = free_memory_offset;
            // Shift offset to avoid overwriting pollables memory
            free_memory_offset = offset + (memory[offset] * 4) + 4;  // N 32-bit numbers + length

            // log(create_arguments_2(create_string_from_array("started processing pollables, len:"), new_number(memory[offset] as f64)));

            execute_pollables(offset);

            process_thenables();

            clear_pollables(offset);

            // Restore original free_memory_offset
            free_memory_offset = offset;

            let length: i32 = store_pollables(free_memory_offset);

            poll_many(offset, length, offset);
        }

        fn setup_jawsm_global_object() {
            let JAWSM: anyref = create_object();
            declare_variable(global_scope as Scope, data!("JAWSM"), JAWSM, VARIABLE_CONST);

            set_property(JAWSM, data!("ToLength"),
                create_property_function(global_scope as Scope, JAWSM_ToLength, null));
            set_property(JAWSM, data!("ToObject"),
                create_property_function(global_scope as Scope, JAWSM_ToObject, null));
            set_property(JAWSM, data!("LengthOfArrayLike"),
                create_property_function(global_scope as Scope, JAWSM_LengthOfArrayLike, null));
            set_property(JAWSM, data!("DoesNotExceedSafeInteger"),
                create_property_function(global_scope as Scope, JAWSM_DoesNotExceedSafeInteger, null));
            set_property(JAWSM, data!("ToAbsoluteIndex"),
                create_property_function(global_scope as Scope, JAWSM_ToAbsoluteIndex, null));
            set_property(JAWSM, data!("ToString"),
                create_property_function(global_scope as Scope, JAWSM_ToString, null));
        }

        fn setup_math_namespace() {
            let Math: anyref = create_object();
            declare_variable(global_scope as Scope, data!("Math"), Math, VARIABLE_CONST);

            set_property(Math, data!("floor"),
                create_property_function(global_scope as Scope, Math_floor, null));

            set_property(Math, data!("trunc"),
                create_property_function(global_scope as Scope, Math_trunc, null));
        }

        fn install_globals() {
            global_scope = new_scope(null);

            setup_jawsm_global_object();
            setup_math_namespace();

            let promise_constructor: Function = new_function(global_scope as Scope, Promise_constructor, null);
            let generator_constructor: Function = new_function(global_scope as Scope, Generator_constructor, null);
            global_generator_constructor = generator_constructor;
            let async_generator_constructor: Function = new_function(global_scope as Scope, AsyncGenerator_constructor, null);
            global_async_generator_constructor = async_generator_constructor;
            let object_constructor: Function = new_function(global_scope as Scope, Object_constructor, null);
            let array_constructor: Function = new_function(global_scope as Scope, Array_constructor, null);
            let symbol_constructor: Function = new_function(global_scope as Scope, Symbol_constructor, null);
            let number_constructor: Function = new_function(global_scope as Scope, Number_constructor, null);
            let string_constructor: Function = new_function(global_scope as Scope, String_constructor, null);
            let bigint_constructor: Function = new_function(global_scope as Scope, BigInt_constructor, null);
            let boolean_constructor: Function = new_function(global_scope as Scope, Boolean_constructor, null);
            let error_constructor: Function = new_function(global_scope as Scope, Error_constructor, null);
            let reference_error_constructor: Function = new_function(global_scope as Scope, ReferenceError_constructor, null);
            let type_error_constructor: Function = new_function(global_scope as Scope, TypeError_constructor, null);
            let range_error_constructor: Function = new_function(global_scope as Scope, RangeError_constructor, null);
            let syntax_error_constructor: Function = new_function(global_scope as Scope, SyntaxError_constructor, null);

            global_object_prototype = create_object_prototype();
            global_function_prototype = create_function_prototype();

            global_symbol_prototype = create_symbol_prototype(symbol_constructor);
            setup_symbol_constructor(symbol_constructor);

            promise_prototype = create_promise_prototype();
            generator_prototype = create_generator_prototype();
            async_generator_prototype = create_async_generator_prototype();
            global_array_prototype = create_array_prototype();
            global_number_prototype = create_number_prototype();
            global_boolean_prototype = create_boolean_prototype();
            global_string_prototype = create_string_prototype();
            global_bigint_prototype = create_bigint_prototype();

            set_property(promise_constructor, data!("prototype"), create_bare_property(promise_prototype));
            set_property(async_generator_constructor, data!("prototype"), create_bare_property(async_generator_prototype));
            set_property(object_constructor, data!("prototype"), create_bare_property(global_object_prototype));
            setup_array_constructor(array_constructor);
            set_property(number_constructor, data!("prototype"), create_bare_property(global_number_prototype));
            set_property(boolean_constructor, data!("prototype"), create_bare_property(global_boolean_prototype));
            set_property(string_constructor, data!("prototype"), create_bare_property(global_string_prototype));

            let error_prototype: Object = Object_create(global_scope as Scope, null, create_arguments_1(global_object_prototype), null) as Object;
            set_property_value(error_prototype, data!("constructor"), error_constructor);
            set_property_value(error_constructor, data!("prototype"), error_prototype);

            let reference_error_prototype: Object = Object_create(global_scope as Scope, null, create_arguments_1(error_prototype), null) as Object;
            set_property_value(reference_error_prototype, data!("constructor"), reference_error_constructor);
            set_property_value(reference_error_constructor, data!("prototype"), reference_error_prototype);

            let type_error_prototype: Object = Object_create(global_scope as Scope, null, create_arguments_1(error_prototype), null) as Object;
            set_property_value(type_error_prototype, data!("constructor"), type_error_constructor);
            set_property_value(type_error_constructor, data!("prototype"), type_error_prototype);

            let range_error_prototype: Object = Object_create(global_scope as Scope, null, create_arguments_1(error_prototype), null) as Object;
            set_property_value(range_error_prototype, data!("constructor"), range_error_constructor);
            set_property_value(range_error_constructor, data!("prototype"), range_error_prototype);

            declare_variable(global_scope as Scope, data!("Promise"), promise_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("Object"), object_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("Error"), error_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("Array"), array_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("Number"), number_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("String"), string_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("BigInt"), bigint_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("Boolean"), boolean_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("Symbol"), symbol_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("ReferenceError"), reference_error_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("TypeError"), type_error_constructor, VARIABLE_CONST);
            declare_variable(global_scope as Scope, data!("SyntaxError"), syntax_error_constructor, VARIABLE_CONST);

            setup_object_constructor(object_constructor);

            global_this = create_global_this();
            declare_variable(global_scope as Scope, data!("globalThis"), global_this, VARIABLE_CONST);
        }

        #[export("wasi:cli/run@0.2.1#run")]
        fn outer_init() -> i32 {
            try {
                install_globals();
                init();

                process_thenables();

                let offset: i32 = free_memory_offset;
                free_memory_offset = offset + (len!(pollables) * 4) + 4;  // N 32-bit numbers + length
                let length: i32 = store_pollables(offset);
                poll_many(offset, length, offset);
                free_memory_offset = offset;

                return 0;
            }
            catch(JSException, error: anyref) {
                let message: anyref = get_property_value(error, data!("message"));

                if !ref_test!(message, null) {
                    let args: JSArgs = create_arguments_2(
                        new_static_string(data!("error encountered"), 17),
                        message
                    );
                    log(args);
                } else {
                    let args: JSArgs = create_arguments_2(
                        new_static_string(data!("error encountered"), 17),
                        error
                    );
                    log(args);
                }
                throw!(JSException, error);
                return 1;
            }

            return 0;
        }

        #[export("_start")]
        fn start() {
            proc_exit(outer_init());
        }
    }
}
