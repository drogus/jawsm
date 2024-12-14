use tarnik::wasm;
use tarnik_ast::WatModule;

pub fn generate_module() -> WatModule {
    wasm! {
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

        // Basic types for strings
        type CharArray = [mut i8];

        struct String {
            data: mut CharArray,
            length: mut i32
        }

        struct StaticString {
            offset: i32,
            length: i32
        }

        // HashMap types
        struct HashMapEntryI32 {
            key: mut i32,
            value: mut i32
        }
        type EntriesArrayI32 = [mut Nullable<HashMapEntryI32>];
        struct HashMapI32 {
            entries: mut EntriesArrayI32,
            size: mut i32
        }

        struct HashMapEntry {
            key: mut i32,
            value: mut anyref
        }
        type EntriesArray = [mut Nullable<HashMapEntry>];
        struct HashMap {
            entries: mut EntriesArray,
            size: mut i32
        }

        // Scope and variable management
        struct Scope {
            parent: mut Nullable<Scope>,
            variables: mut HashMap,
            var_types: mut HashMapI32
        }

        // Function types
        type JSArgs = [mut anyref];
        type JSFunc = fn(scope: Scope, this: anyref, arguments: JSArgs) -> anyref;

        struct Function {
            scope: mut Scope,
            func: mut JSFunc,
            this: mut anyref,
            properties: mut HashMap,
            own_prototype: mut anyref
        }

        // Object types
        struct Object {
            properties: mut HashMap,
            own_prototype: mut anyref,
        }

        struct Number {
            value: mut f64
        }

        type AnyrefArray = [mut anyref];

        // at the moment it doesn't have to be a struct, but in the future
        // we will need support for ptototype and properties and what not
        struct Array {
            array: mut AnyrefArray
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
                properties: mut HashMap,
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

        // TODO; allow to insert valus from outside the macro
        static mut free_memory_offset: i32 = 1000;
        static mut global_scope: Nullable<Scope> = null;
        static mut promise_prototype: Nullable<Object> = null;
        static mut global_object_prototype: Nullable<Object> = null;
        static mut global_function_prototype: Nullable<Object> = null;
        static mut global_number_prototype: Nullable<Object> = null;
        static mut pollables: PollablesArray = [null; 2];

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

        fn new_pollable(id: i32, func: anyref) -> Pollable {
            return Pollable { id: id, func: func };
        }

        fn create_new_promise() -> Promise {
            return Promise {
                properties: new_hashmap(),
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

        fn create_promise_prototype() -> Object {
            let object: Object = new_object();

            set_property(object, data!("then"),
                new_function(global_scope as Scope, Promise_then, null));

            set_property(object, data!("catch"),
                new_function(global_scope as Scope, Promise_catch, null));

            set_property(object, data!("finally"),
                new_function(global_scope as Scope, Promise_finally, null));

            set_property(object, data!("toString"),
                new_function(global_scope as Scope, Promise_toString, null));

            set_property(object, data!("constructor"),
                new_function(global_scope as Scope, Promise_constructor, null));

            return object;
        }

        fn Promise_toString(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
            return new_static_string(data!("[object Promise]"), 16);
        }

        fn Promise_catch(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
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
                Promise_then(scope, new_promise, catch_args);
            }

            return new_promise;
        }

        fn create_object_prototype() -> Object {
            let object: Object = new_object();

            set_property(object, data!("toString"),
                new_function(global_scope as Scope, Object_toString, null));

            set_property(object, data!("constructor"),
                new_function(global_scope as Scope, Object_constructor, null));

            return object;
        }

        fn setup_object_constructor(constructor: Function) {
            set_property(constructor, data!("create"),
                new_function(global_scope as Scope, Object_create, null));
        }

        fn Object_toString(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
            return new_static_string(data!("[object Object]"), 15);
        }

        fn Object_create(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
            let object: Object = new_object();
            object.own_prototype = arguments[0];
            return object;
        }

        fn Object_constructor(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
            // this is wrong, but for now I need to have anything that works
            return new_object();
        }

        fn create_function_prototype() -> Object {
            let object: Object = new_object();

            set_property(object, data!("toString"),
                new_function(global_scope as Scope, Function_toString, null));

            set_property(object, data!("call"),
                new_function(global_scope as Scope, Function_call, null));

            return object;
        }

        fn Function_call(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
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

            // fn call_function(func: anyref, this: anyref, arguments: JSArgs) -> anyref {
            // TODO: tail call
            return call_function(this, new_this, rest);
        }

        fn Function_toString(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
            return new_static_string(data!("function () { [native code] }"), 29);
        }

        fn create_number_prototype() -> Object {
            let object: Object = new_object();

            set_property(object, data!("toString"),
                new_function(global_scope as Scope, Number_toString, null));

            return object;
        }

        fn Number_toString(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
            // TODO: we first write to memory and then extract to an array. it will be better
            // to build two versions of methods that convert stuff - one that writes to memory and
            // one that writes to an array
            return number_to_string_raw(this as Number);
        }

        fn number_to_string_raw(number: Number) -> String {
            let written_length: i32 = writeF64AsAscii(number.value, free_memory_offset);
            return memory_to_string(free_memory_offset, written_length);
        }

        fn memory_to_string(offset: i32, length: i32) -> String {
            let string_data: CharArray = [0; length];

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
                    result = call_function(promise.finally_callback as Function, null, arguments);
                }

                // Handle then callback if present
                if ref_test!(promise.then_callback, Function) {
                    result = call_function(promise.catch_callback as Function, null, arguments);
                } else {
                    result = previous_result;
                }

                // Reuse arguments array for resolve call
                arguments[0] = result;
                Promise_resolve(scope, promise, arguments);
            }
            catch(JSException, argument: anyref) {
                arguments = create_arguments_1(argument);
                Promise_reject(scope, promise, arguments);
            }
        }

        fn evaluate_resolved(scope: Scope, previous_result: anyref, promise: Promise) {
            let mut arguments: JSArgs = create_arguments_1(previous_result);
            let mut result: anyref;

            try {
                // Handle finally callback if present
                if ref_test!(promise.finally_callback, Function) {
                    result = call_function(promise.finally_callback as Function, null, arguments);
                }

                // Handle then callback if present
                if ref_test!(promise.then_callback, Function) {
                    result = call_function(promise.then_callback as Function, null, arguments);
                } else {
                    result = previous_result;
                }

                // Reuse arguments array for resolve call
                arguments[0] = result;
                Promise_resolve(scope, promise, arguments);
            }
            catch(JSException, argument: anyref) {
                arguments = create_arguments_1(argument);
                Promise_reject(scope, promise, arguments);
            }
        }

        fn Promise_finally(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
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

        fn Promise_then(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
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

        fn Promise_resolve(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
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

        fn Promise_reject(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
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

        fn Promise_constructor(scope: Scope, this: anyref, arguments: JSArgs) -> anyref {
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
                    call_function(resolver, null, callback_arguments);
                }
                catch(JSException, err: anyref) {
                    reject_args = create_arguments_1(err);
                    Promise_reject(scope, promise, reject_args);
                }

                return promise;
            } else {
                throw!(JSException, new_static_string(offset, 32));
            }

            return null;
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

        fn return_new_instance_result(first: anyref, second: anyref, prototype: anyref) -> anyref {
            let mut result: anyref;
            if ref_test!(first, Object) || ref_test!(first, Promise) {
                result = first;
            } else {
                result = second;
            }

            if ref_test!(result, Object) {
                (result as Object).own_prototype = prototype;
            } else if ref_test!(result, Promise) {
                (result as Promise).own_prototype = prototype;
            }

            return result;
        }

        fn add_static_strings(ptr1: i32, len1: i32, ptr2: i32, len2: i32) -> String {
            let total_length: i32 = len1 + len2;
            let string_data: CharArray = [0; total_length];
            let mut i: i32 = 0;

            // Copy first part
            while i < len1 {
                string_data[i] = memory::<i8>[ptr1 + i];
                i += 1;
            }

            // Copy second part
            i = 0;
            while i < len2 {
                string_data::<i8>[len1 + i] = memory::<i8>[ptr2 + i];
                i += 1;
            }

            return String {
                data: string_data,
                length: total_length
            };
        }

        fn add_static_string_to_string(str: String, ptr: i32, len: i32) -> String {
            let str_len: i32 = str.length;
            let string_data: CharArray = str.data;
            let total_length: i32 = str_len + len;
            let new_string_data: CharArray = [0; total_length];

            // TODO: replace with array.copy when implemented
            let mut i: i32 = 0;
            while i < str_len {
                new_string_data[i] = string_data[i];
                i += 1;
            }

            i = 0;
            while i < len {
                new_string_data[str_len + i] = memory::<i8>[ptr + i];
                i += 1;
            }

            return String {
                data: new_string_data,
                length: total_length
            };
        }

        fn new_hashmap() -> HashMap {
            return HashMap {
                entries: [null; 10],
                size: 0
            };
        }

        fn new_hashmap_i32() -> HashMapI32 {
            return HashMapI32 {
                entries: [null; 10],
                size: 0
            };
        }

        fn new_object() -> Object {
            return Object {
                properties: new_hashmap(),
                own_prototype: global_object_prototype,
            };
        }

        fn new_array(size: i32) -> Array {
            return Array {
                array: [null; size]
            };
        }

        fn new_function(scope: Scope, function: JSFunc, this: anyref) -> Function {
            let f: Function = Function {
                scope: scope,
                func: function,
                this: this,
                properties: new_hashmap(),
                own_prototype: global_function_prototype
            };

            // TODO: this should also have a constructor set
            set_property(f, data!("prototype"), new_object());

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

            if ref_test!(arg, StaticString) {
                return 1;
            }

            return 0;
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
                variables: new_hashmap(),
                var_types: new_hashmap_i32()
            };
        }

        fn assign_variable(scope: Scope, name: i32, value: anyref) {
            let mut current_scope: Nullable<Scope> = scope;
            let found_value: anyref;
            let mut run: i32 = 1;

            while run {
                found_value = hashmap_get(current_scope.variables, name);
                if is_no_value_found(found_value) {
                    current_scope = current_scope.parent;
                    if ref_test!(current_scope, null) {
                        throw!(JSException, name as i31ref);
                    }
                } else {
                    set_variable(current_scope as Scope, name, value);
                    run = 0;
                }
            }

            // TODO: if there's no variable found JS still lets you assign, but
            // it saves it on the global scope
            set_variable(global_scope as Scope, name, value);
        }

        fn set_variable(scope: Scope, name: i32, value: anyref) {
            let existing_type: i32 = hashmap_get_i32(scope.var_types, name);

            if existing_type == 0 {
                // 0 means const, throw error
                throw!(JSException, 12 as i31ref);
            }

            hashmap_set(scope.variables, name, value);
        }

        fn declare_variable(scope: Scope, name: i32, value: anyref, var_type: i32) {
            let existing_type: i32 = hashmap_get_i32(scope.var_types, name);

            if existing_type == 3 || existing_type == -1 || existing_type == 2 {
                // -1: no var in hashmap, can declare
                // 2: var and 3: param, can overwrite
                hashmap_set(scope.variables, name, value);
                hashmap_set_i32(scope.var_types, name, var_type);
                return;
            }

            if existing_type == 0 {
                // const declaration, throw error
                throw!(JSException, 10 as i31ref);
            }

            if existing_type == 1 {
                // let declaration, throw error
                throw!(JSException, 11 as i31ref);
            }
        }

        fn is_no_value_found(arg: anyref) -> i32 {
            if ref_test!(arg, null) {
                return 0;
            }

            if ref_test!(arg, i31ref) {
                if (arg as i31ref) == -1 {
                    return 1;
                }
            }

            return 0;
        }

        fn create_reference_error(name: i32) -> anyref {
            let constructor: Function = get_variable(global_scope as Scope, data!("ReferenceError")) as Function;
            let object: Object = new_object();
            let arguments: JSArgs = create_arguments_1(new_static_string(data!("could not find reference"), 24));
            call_function(constructor, object, arguments);

            return return_new_instance_result(object, null, get_property(constructor, data!("prototype")));
        }

        fn get_variable(scope: Scope, name: i32) -> anyref {
            let mut current_scope: Nullable<Scope> = scope;
            let value: anyref;

            while 1 {
                value = hashmap_get(current_scope.variables, name);
                if is_no_value_found(value) {
                    current_scope = current_scope.parent;
                    if ref_test!(current_scope, null) {
                        // TODO: this should throw reference error, but we have to implement it in
                        // here, not in JS
                        throw!(JSException, 999999999 as i31ref);
                    }
                } else {
                    return value;
                }
            }
            throw!(JSException, 103 as i31ref);
        }

        // TODO: this should also handle prototypes
        fn get_property(target: anyref, name: i32) -> anyref {
            let mut result: anyref;
            let promise: Promise;
            let function: Function;
            let object: Object;

            // TODO: as long as objects like Function and String are just another objects
            // we will have to reimplement a lot of stuff like this. It would be great
            // to research parent and child types
            if ref_test!(target, Object) {
                object = target as Object;
                result = hashmap_get(object.properties, name);
                if ref_test!(result, i31ref) {
                    if !ref_test!(result, null) {
                        if (result as i31ref) == -1 {
                            // if the result is i31ref -1, it means nothing was found, so
                            // we need to search prototypes
                            if !ref_test!(object.own_prototype, null) {
                                result = get_property(object.own_prototype, name);
                            }
                        }
                    }
                }
            } else if ref_test!(target, Function) {
                function = target as Function;
                result = hashmap_get(function.properties, name);
                if ref_test!(result, i31ref) {
                    if !ref_test!(result, null) {
                        if (result as i31ref) == -1 {
                            // if the result is i31ref -1, it means nothing was found, so
                            // we need to search prototypes
                            if !ref_test!(function.own_prototype, null) {
                                result = get_property(function.own_prototype, name);
                            }
                        }
                    }
                }
            } else if ref_test!(target, Promise) {
                promise = target as Promise;
                result = hashmap_get(promise.properties, name);
                if ref_test!(result, i31ref) {
                    if !ref_test!(result, null) {
                        if (result as i31ref) == -1 {
                            // if the result is i31ref -1, it means nothing was found, so
                            // we need to search prototypes
                            if !ref_test!(promise.own_prototype, null) {
                                result = get_property(promise.own_prototype, name);
                            }
                        }
                    }
                }
            } else if ref_test!(target, Number) {
                // numbers don't have their own properties, but use the Number prototype
                result = get_property(global_number_prototype, name);
            }

            if ref_test!(result, i31ref) {
                if (result as i31ref) == -1 {
                    return result;
                }
            }

            if ref_test!(result, Function) {
                (result as Function).this = target;
            }

            return result;
        }

        fn set_property(target: anyref, name: i32, value: anyref) {
            if ref_test!(target, Object) {
                hashmap_set((target as Object).properties, name, value);
                return;
            }

            if ref_test!(target, Function) {
                hashmap_set((target as Function).properties, name, value);
                return;
            }

            if ref_test!(target, Promise) {
                hashmap_set((target as Promise).properties, name, value);
                return;
            }

            throw!(JSException, 101 as i31ref);
        }

        fn hashmap_set(map: HashMap, key: i32, value: anyref) {
            let mut entries: EntriesArray = map.entries;
            let new_entry: HashMapEntry = HashMapEntry { key: key, value: value };
            let mut found: i32 = 0;
            let mut i: i32 = 0;
            let new_size: i32;
            let mut new_entries: EntriesArray;
            let mut entry: HashMapEntry;
            let len: i32 = len!(entries);
            let mut map_size: i32 = map.size;

            // First, search for existing key
            while i < map_size {
                if ref_test!(entries[i], HashMapEntry) {
                    entry = entries[i] as HashMapEntry;
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

        fn hashmap_set_i32(map: HashMapI32, key: i32, value: i32) {
            let mut entries: EntriesArrayI32 = map.entries;
            let new_entry: HashMapEntryI32 = HashMapEntryI32 { key: key, value: value };
            let mut found: i32 = 0;
            let mut i: i32 = 0;
            let new_size: i32;
            let mut new_entries: EntriesArrayI32;
            let len: i32 = len!(entries);

            // First, search for existing key
            while i < map.size {
                if entries[i].key == key {
                    entries[i] = new_entry;
                    found = 1;
                    return;
                }
                i += 1;
            }

            // If key wasn't found, proceed with insertion
            if found == 0 {
                // Check if we need to resize
                if map.size >= len!(entries) {
                    new_size = len!(entries) * 2;
                    new_entries = [null; new_size];

                    // Copy old entries to new array
                    i = 0;
                    while i < len!(entries) {
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

        fn hashmap_get(map: HashMap, key: i32) -> anyref {
            let entries: EntriesArray = map.entries;
            let mut i: i32 = 0;

            while i < map.size {
                if entries[i].key == key {
                    return entries[i].value;
                }
                i += 1;
            }

            // we need to somehow differentiate between value not being found and the found
            // value being null
            // TODO: it would be much easier to check the result on the call site if we
            // returned a pair of values - i32 and anyref, where i32 is for checking if
            // the value was found. Tarnik doesn't support multiple return values, so it would
            // be nice to fix it
            // TODO: trying to return -1 as i31ref fails, cause tarnik first tries to get
            // i31.get_s and only then negate. It should be fixed
            let res_i32: i32 = -1;
            let res: i31ref = res_i32 as i31ref;
            return res;
        }

        fn hashmap_get_i32(map: HashMapI32, key: i32) -> i32 {
            let entries: EntriesArrayI32 = map.entries;
            let mut i: i32 = 0;

            while i < map.size {
                if entries[i].key == key {
                    return entries[i].value;
                }
                i += 1;
            }

            // not sure if there is a better way to handle this, but since the result is i32,
            // we have to return something on not found key
            // if it was used for anything else than values greater than zero, we should probably
            // make a function like hashmap_exists_i32 and then if a key exists, assume hashmap_get returns
            // a proper key
            return -1;
        }

        fn call_function(func: anyref, this: anyref, arguments: JSArgs) -> anyref {
            let function: Function = func as Function;
            let js_func: JSFunc = function.func;
            let current_this: anyref;

            if ref_test!(this, null) {
                current_this = function.this;
            } else {
                current_this = this;
            }

            return js_func(function.scope, current_this, arguments);
        }

        fn add(arg1: anyref, arg2: anyref) -> anyref {
            let result: f64;
            let static_str1: StaticString;
            let static_str2: StaticString;
            let str1: String;
            let str2: String;

            // TODO: this doesn't take into account casting, it can only add two objects
            // of the same type (and only numbers and strings for now)
            if ref_test!(arg1, Number) && ref_test!(arg2, Number) {
                result = (arg1 as Number).value + (arg2 as Number).value;
                return new_number(result);
            }

            if ref_test!(arg1, StaticString) && ref_test!(arg2, StaticString) {
                static_str1 = arg1 as StaticString;
                static_str2 = arg2 as StaticString;
                return add_static_strings(static_str1.offset, static_str1.length, static_str2.offset, static_str2.length);
            }

            if ref_test!(arg1, String) && ref_test!(arg2, StaticString) {
                str1 = arg1 as String;
                static_str2 = arg2 as StaticString;
                return add_static_string_to_string(str1, static_str2.offset, static_str2.length);
            }

            return null;
        }

        fn div(arg1: anyref, arg2: anyref) -> anyref {
            if ref_test!(arg1, Number) && ref_test!(arg2, Number) {
                let num1: Number = arg1 as Number;
                let num2: Number = arg2 as Number;
                let result: f64 = num1.value / num2.value;
                return new_number(result);
            }
            return null;
        }

        fn sub(arg1: anyref, arg2: anyref) -> anyref {
            if ref_test!(arg1, Number) && ref_test!(arg2, Number) {
                let num1: Number = arg1 as Number;
                let num2: Number = arg2 as Number;
                let result: f64 = num1.value - num2.value;
                return new_number(result);
            }
            return null;
        }

        fn strict_not_equal(arg1: anyref, arg2: anyref) -> i31ref {
            return !strict_equal(arg1, arg2);
        }

        fn strict_equal(arg1: anyref, arg2: anyref) -> i31ref {
            let mut i: i32 = 0;
            let mut len: i32 = 0;
            let str1: String;
            let str2: String;
            let static_str1: StaticString;
            let static_str2: StaticString;
            let char_array1: CharArray;
            let char_array2: CharArray;
            let offset: i32;

            if ref_test!(arg1, null) && ref_test!(arg2, null) {
                return 1 as i31ref;
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
                return 1 as i31ref;
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

                if str1.length != static_str2.length {
                    return 0 as i31ref;
                }
                len = str1.length;
                offset = static_str2.offset;
                char_array1 = str1.data;
                while i < len {
                    if char_array1[i] != memory::<i8>[offset + i] {
                        return 0 as i31ref;
                    }
                    i += 1;
                }

                return 1 as i31ref;

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
                    if memory::<i8>[offset + i] != char_array2[i] {
                        return 0 as i31ref;
                    }
                    i += 1;
                }

                return 1 as i31ref;
            }

            return 0 as i31ref;
        }

        fn is_a_string(arg: anyref) -> i32 {
            return ref_test!(arg, String) || ref_test!(arg, StaticString);
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

        fn loose_equal(arg1: anyref, arg2: anyref) -> i31ref {
            if (ref_test!(arg1, null) && ref_test!(arg2, null)) ||
               (is_a_string(arg1) && is_a_string(arg2)) ||
               (ref_test!(arg1, Number) && ref_test!(arg2, Number)) {
                return strict_equal(arg1, arg2);
            }

            if is_null_or_undefined(arg1) && is_null_or_undefined(arg2) {
                return 1 as i31ref;
            }

            // TODO: this is wrong, it should be a string converted to a number
            if ref_test!(arg1, Number) && is_a_string(arg2) {
                return strict_equal(number_to_string_raw(arg1 as Number), arg2);
            }

            if is_a_string(arg1) && ref_test!(arg2, Number) {
                return strict_equal(arg1, number_to_string_raw(arg2 as Number));
            }

            // TODO: implement the rest of the cases
            // if ref_test!(arg1, Number) && ref_test!(arg2, Number) {
            //     let num1: Number = arg1 as Number;
            //     let num2: Number = arg2 as Number;
            //     return (num1.value == num2.value) as i31ref;
            // }
            //
            // if ref_test!(arg1, i31ref) && ref_test!(arg2, i31ref) {
            //     return 1 as i31ref;
            // }

            return 0 as i31ref;
        }

        fn logical_or(arg1: anyref, arg2: anyref) -> anyref {
            if ref_test!(arg1, null) == 0 {
                return arg1;
            }

            if ref_test!(arg1, i31ref) {
                let val: i32 = arg1 as i31ref as i32;
                if val != 0 && val != 2 {
                    return arg1;
                }
            }

            return arg2;
        }

        fn logical_and(arg1: anyref, arg2: anyref) -> anyref {
            // TODO: handle numbers properly - 0 is false
            if ref_test!(arg1, null) {
                return arg1;
            }

            if ref_test!(arg1, i31ref) {
                let val: i32 = arg1 as i31ref as i32;
                if val == 0 || val == 2 {
                    return arg1;
                }
            }

            return arg2;
        }

        fn logical_not(arg: anyref) -> i31ref {
            if ref_test!(arg, null) {
                return 1 as i31ref;
            }

            if ref_test!(arg, i31ref) {
                let val: i32 = arg as i31ref as i32;
                if val == 0 || val == 2 {
                    return 1 as i31ref;
                }
            }

            return 0 as i31ref;
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

            if ref_test!(arg, Object) || ref_test!(arg, Promise) {
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

        fn less_than(arg1: anyref, arg2: anyref) -> i31ref {
            if ref_test!(arg1, Number) && ref_test!(arg2, Number) {
                let num1: Number = arg1 as Number;
                let num2: Number = arg2 as Number;
                let result: i32 = num1.value < num2.value;
                return result as i31ref;
            }
            return 0 as i31ref;
        }

        fn greater_than_or_equal(arg1: anyref, arg2: anyref) -> i31ref {
            if ref_test!(arg1, Number) && ref_test!(arg2, Number) {
                let num1: Number = arg1 as Number;
                let num2: Number = arg2 as Number;
                let result: i32 = num1.value >= num2.value;
                return result as i31ref;
            }
            return 0 as i31ref;
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
        fn writeF64AsAscii(value: f64, offset: i32) -> i32 {
            let mut current_offset: i32 = offset;
            let mut remaining_value: f64 = value;

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

            // Split into integer and fractional parts
            let int_part: f64 = remaining_value as i64 as f64;
            let frac_part: f64 = remaining_value - int_part;

            // Convert integer part
            let mut int_digits: CharArray = [0; 20];  // Max digits for f64
            let mut int_count: i32 = 0;
            let mut int_val: f64 = int_part;

            let digit: i32;
            while int_val >= 1.0 {
                digit = (int_val as i64 % 10) as i32;
                int_digits[int_count] = '0' as i32 + digit;
                int_count += 1;
                int_val = int_val / 10.0;
                // floor
                int_val = int_val as i64 as f64;
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

        fn log_string(str: StaticString) {
            let args: JSArgs = create_arguments_1(str);
            log(args);
        }

        fn log(arguments: JSArgs) {
            let len: i32 = len!(arguments);
            let mut iovectors_offset: i32 = free_memory_offset;
            let mut offset: i32 = free_memory_offset + (len * 16);
            let mut handled: i32;
            let mut current: anyref;
            let mut val: i32;
            let mut num_val: f64;
            let mut written_length: i32;
            let mut number: Number;
            let mut static_str: StaticString;
            let mut str: String;
            let mut str_len: i32;
            let mut j: i32;
            let mut str_data: CharArray;

            let mut i: i32 = 0;
            while i < len {
                current = arguments[i];
                handled = 0;

                // Handle undefined
                if ref_test!(current, null) {
                    memory[iovectors_offset] = data!("undefined");
                    memory[iovectors_offset + 4] = 9;
                    iovectors_offset += 8;
                    handled = 1;
                }

                // Handle i31ref (null, boolean, number)
                if ref_test!(current, null) == 0 && ref_test!(current, i31ref) {
                    val = current as i31ref as i32;
                    if val == 0 {
                        memory[iovectors_offset] = data!("false");
                        memory[iovectors_offset + 4] = 5;
                        iovectors_offset += 8;
                        handled = 1;
                    } else if val == 1 {
                        memory[iovectors_offset] = data!("true");
                        memory[iovectors_offset + 4] = 4;
                        iovectors_offset += 8;
                        handled = 1;
                    } else if val == 2 {
                        memory[iovectors_offset] = data!("null");
                        memory[iovectors_offset + 4] = 4;
                        iovectors_offset += 8;
                        handled = 1;
                    } else {
                        num_val = val as f64;
                        written_length = writeF64AsAscii(num_val, offset);

                        memory[iovectors_offset] = offset;
                        memory[iovectors_offset + 4] = written_length;

                        offset += written_length;
                        iovectors_offset += 8;
                        handled = 1;
                    }
                }

                // Handle Number
                if ref_test!(current, Number) {
                    number = current as Number;
                    written_length = writeF64AsAscii(number.value, offset);

                    memory[iovectors_offset] = offset;
                    memory[iovectors_offset + 4] = written_length;

                    offset += written_length;
                    iovectors_offset += 8;
                    handled = 1;
                }

                // Handle StaticString
                if ref_test!(current, StaticString) {
                    static_str = current as StaticString;
                    memory[iovectors_offset] = static_str.offset;
                    memory[iovectors_offset + 4] = static_str.length;
                    iovectors_offset += 8;
                    handled = 1;
                }

                // Handle String
                if ref_test!(current, String) {
                    str = current as String;
                    str_len = str.length;

                    memory[iovectors_offset] = offset;
                    memory[iovectors_offset + 4] = str_len;

                    // Copy string data to memory
                    j = 0;
                    str_data = str.data;
                    while j < str_len {
                        memory[offset + j] = str_data[j];
                        j += 1;
                    }

                    offset += str_len;
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
            write(1, free_memory_offset, len * 2, 50);
        }

        fn add_pollable(pollable: Pollable) -> i32 {
            let mut len: i32 = len!(pollables);

            let mut i: i32 = 0;
            while i < len {
                if ref_test!(pollables[i], null) {
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
                        call_function(func, null, []);
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

            let mut i: i32 = 0;
            while i < len {
                 index = find_pollable(memory[current_offset]);

                if index != -1 {
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

            let mut i: i32 = 0;
            while i < len {
                let current: Nullable<Pollable> = pollables[i];
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
            // TODO: just a stup for now
            return new_number(1);
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

            execute_pollables(offset);
            clear_pollables(offset);

            // Restore original free_memory_offset
            free_memory_offset = offset;

            let length: i32 = store_pollables(free_memory_offset);
            poll_many(free_memory_offset, length, free_memory_offset);
        }

        fn install_globals() {
            global_scope = new_scope(null);

            promise_prototype = create_promise_prototype();
            global_object_prototype = create_object_prototype();
            global_function_prototype = create_function_prototype();
            global_number_prototype = create_number_prototype();

            let promise_constructor: Function = new_function(global_scope as Scope, Promise_constructor, null);
            let object_constructor: Function = new_function(global_scope as Scope, Object_constructor, null);

            set_property(promise_constructor, data!("prototype"), promise_prototype);
            set_property(object_constructor, data!("prototype"), global_object_prototype);

            set_variable(global_scope as Scope, data!("Promise"), promise_constructor);
            set_variable(global_scope as Scope, data!("Object"), object_constructor);

            setup_object_constructor(object_constructor);
        }

        #[export("wasi:cli/run@0.2.1#run")]
        fn outer_init() -> i32 {
            try {
                install_globals();
                init();

                let length: i32 = store_pollables(free_memory_offset);
                poll_many(free_memory_offset, length, free_memory_offset);

                return 0;
            }
            catch(JSException, error: anyref) {
                let args: JSArgs = create_arguments_2(
                    new_static_string(data!("error encountered"), 17),
                    error
                );
                log(args);
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
