(module $mod
  (type (func (param i32)))
  (type (func (param anyref)))
  (type $write_type (func (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_write" (func $write (type $write_type)))
  (import "wasi_snapshot_preview1" "proc_exit" (func $proc_exit (param i32)))
  ;;(import "console" "log" (func $log_value (param i32)))

  ;;(import "wasi:io/poll@0.2.1" "[method]pollable.ready" (func $poll-ready (param i32) (result i32)))
  (import "wasi:io/poll@0.2.1" "poll" (func $poll-many (param i32 i32 i32)))

  (import "wasi:clocks/monotonic-clock@0.2.1" "subscribe-duration" (func $subscribe-duration (param i64) (result i32)))

  ;; 64KB
  (memory (export "memory") 1)

  (tag $InternalException (type 0))
  (tag $JSException (type 1))

  (global $free_memory_offset (mut i32) (i32.const {{free_memory_offset}}))
  (global $scope (mut (ref null $Scope)) (ref.null $Scope))

  (data (i32.const 0) "\n")

  ;; Types that can be passed as reference types:
  ;; i31ref 0 - false
  ;; i31ref 1 - true
  ;; i31ref 2 - null
  ;; null     - undefined

  {{ data_entries }}

  ;; Memory management functions required by the Component Model
  (func $cabi_realloc (export "cabi_realloc")
    (param $old_ptr i32) (param $old_size i32) (param $align i32) (param $new_size i32)
    (result i32)
    (local $ptr i32)
    
    ;; Simple bump allocator for this example
    (local.set $ptr (i32.const 1024))
    (local.get $ptr)
  )

  (func $canonical_abi_free (export "canonical_abi_free")
    (param $ptr i32) (param $size i32)
    ;; TODO: implement something sensinle here or find examples on how
    ;; it's usually done
  )


  (type $CharArray (array (mut i8)))

  (type $String (struct
    (field $data (mut (ref $CharArray)))
    (field $length (mut i32))
  ))

  (type $StaticString (struct
    (field $offset i32)
    (field $length i32)
  ))

  (type $HashMapEntry (struct
    (field $key (mut i32))
    (field $value (mut anyref))
  ))

  (type $HashMapEntryI32 (struct
    (field $key (mut i32))
    (field $value (mut i32))
  ))

  (type $EntriesArray (array (mut (ref null $HashMapEntry))))
  (type $EntriesArrayI32 (array (mut (ref null $HashMapEntryI32))))

  (type $HashMap (struct
    (field $entries (mut (ref $EntriesArray)))
    (field $size (mut i32))
  ))

  (type $HashMapI32 (struct
    (field $entries (mut (ref $EntriesArrayI32)))
    (field $size (mut i32))
  ))

  (type $Scope (struct
    (field $parent (mut (ref null $Scope)))
    (field $variables (mut (ref $HashMap)))
    (field $var_types (mut (ref $HashMapI32)))
  ))

  (type $JSArgs (array (mut anyref)))

  (type $JSFunc
    (func 
      (param $scope (ref $Scope))
      (param $this anyref)
      (param $arguments (ref $JSArgs))
      (result anyref)
    )
  )

  (type $Function (struct
    (field $scope (mut (ref $Scope)))
    (field $func (mut (ref $JSFunc)))
    (field $this (mut anyref)) ;; some functions have a default this
    (field $properties (mut (ref $HashMap)))
  ))

  (type $Object (struct
    (field $properties (mut (ref $HashMap)))
    (field $prototype (mut anyref))
  ))

  (type $Number (struct 
    (field $value (mut f64)))
  )

  (type $AnyrefArray (array (mut anyref)))

  ;; at the moment it doesn't have to be a struct, but in the future
  ;; we will need support for ptototype and properties and what not
  (type $Array (struct
    (field $array (mut (ref $AnyrefArray)))
  ))

  (type $PollableFunction
    (func 
      (param $scope (ref $Scope))
      (param $this anyref)
      (result anyref)
    )
  )

  (type $Pollable (struct
    (field $id i32)
    (field $func (mut anyref))
  ))

  (type $PollablesArray (array (mut (ref null $Pollable))))

  (type $FunctionArray (array (mut (ref null $Function))))
  (rec
    (type $PromisesArray (array (mut (ref null $Promise))))
    (type $Promise (struct
      (field $properties (mut (ref $HashMap)))
      (field $prototype (mut (ref $Object))) ;; the prototype here cannot be changed
      (field $success-result (mut anyref))
      (field $error-result (mut anyref))
      (field $then-callback (mut (ref null $Function)))
      (field $catch-callback (mut (ref null $Function)))
      (field $finally-callback (mut (ref null $Function)))
      (field $resolved (mut i32))
      (field $errored (mut i32))
      (field $chained-promises (mut (ref $PromisesArray)))
    ))
  )

  (global $pollables (mut (ref $PollablesArray))
      (array.new $PollablesArray (ref.null $Pollable) (i32.const 2)))

  (func $new-pollable (param $id i32) (param $func anyref) (result (ref $Pollable))
    (struct.new $Pollable
      (local.get $id)
      (local.get $func)
    )
  )

  (global $promise-prototype (mut (ref null $Object)) (ref.null $Object))

  (func $new-promise (result (ref $Promise))
    (struct.new $Promise
      (call $new_hashmap)
      (ref.cast (ref $Object) (global.get $promise-prototype))
      (ref.null $Object)
      (ref.null $Object)
      ;; TODO: is there a way to create an array without any elements, but not
      ;; force them to be null?
      (ref.null $Function)
      (ref.null $Function)
      (ref.null $Function)
      (i32.const 0)
      (i32.const 0)
      (array.new $PromisesArray (ref.null $Promise) (i32.const 1))
    )
  )

  (func $create-promise-prototype (result (ref $Object))
    (local $object (ref $Object))
    (local.set $object (call $new_object))

    (local.get $object)
    (i32.const {{ data(str="then") }})
    (call $new_function (ref.cast (ref $Scope) (global.get $scope)) (ref.func $Promise-then) (ref.null any))
    (call $set_property)

    (local.get $object)
    (i32.const {{ data(str="catch") }})
    (call $new_function (ref.cast (ref $Scope) (global.get $scope)) (ref.func $Promise-catch) (ref.null any))
    (call $set_property)

    (local.get $object)
    (i32.const {{ data(str="finally") }})
    (call $new_function (ref.cast (ref $Scope) (global.get $scope)) (ref.func $Promise-finally) (ref.null any))
    (call $set_property)

    (return (local.get $object))
  )

  (func $Promise-then
    (type $JSFunc)
    (param $scope (ref $Scope))
    (param $this anyref)
    (param $arguments (ref $JSArgs))
    (result anyref)

    (local $resolver (ref $Function))
    (local $new-promise (ref $Promise))
    (local $promise (ref $Promise))
    (local $callback-anyref anyref)
    (local $callback (ref $Function))
    (local $callback-arguments (ref $JSArgs))

    (local.set $new-promise (call $new-promise))
    (local.set $promise (ref.cast (ref $Promise) (local.get $this)))

    (array.len (local.get $arguments))
    (i32.eqz)
    (if (then
      ;; no functions to register, just return an empty promise
      (return (local.get $new-promise))
    ))

    (array.len (local.get $arguments))
    (i32.const 0)
    (i32.gt_s)
    (if (then
      ;; we have at least one argument, so onresolved is passed
      (array.get $JSArgs (local.get $arguments) (i32.const 0))
      (local.tee $callback-anyref)
      (if (ref.test (ref $Function))
        (then
          ;; set the then callback on the new promise
          (struct.set $Promise $then-callback (local.get $new-promise) (ref.cast (ref $Function) (local.get $callback-anyref)))
        )
      )
    ))

    (array.len (local.get $arguments))
    (i32.const 1)
    (i32.gt_s)
    (if (then
      ;; we have the second argument, onrejected
      (array.get $JSArgs (local.get $arguments) (i32.const 1))
      (local.tee $callback-anyref)
      (if (ref.test (ref $Function))
        (then
          ;; set the then callback on the new promise
          (struct.set $Promise $catch-callback (local.get $new-promise) (ref.cast (ref $Function) (local.get $callback-anyref)))
        )
      )
    ))

    (if (i32.eqz (struct.get $Promise $resolved (local.get $promise)))
      (then
        ;; it's not resolved, check if it not errored
        (if (i32.eqz (struct.get $Promise $errored (local.get $promise)))
          (then
            (call $add-to-promise-chain
              (local.get $promise) ;; target promise
              (local.get $new-promise) ;; promise to add
            )
          )
          (else
            ;; it errored already, evaluate on-rejected
            (local.get $scope)
            (struct.get $Promise $error-result (local.get $promise))
            (local.get $new-promise)
            (call $evaluate-rejected)
          )
        )
      )
      (else
        ;; if it's already resolved, evaluate onresolved
        (local.get $scope)
        (struct.get $Promise $success-result (local.get $promise))
        (local.get $new-promise)
        (call $evaluate-resolved)
      )
    )

    (return (local.get $new-promise))
  )

  (func $Promise-finally
    (type $JSFunc)
    (param $scope (ref $Scope))
    (param $this anyref)
    (param $arguments (ref $JSArgs))
    (result anyref)

    (local $resolver (ref $Function))
    (local $new-promise (ref $Promise))
    (local $promise (ref $Promise))
    (local $callback-anyref anyref)
    (local $callback (ref $Function))
    (local $callback-arguments (ref $JSArgs))

    (local.set $new-promise (call $new-promise))
    (local.set $promise (ref.cast (ref $Promise) (local.get $this)))

    (array.len (local.get $arguments))
    (i32.eqz)
    (if (then
      ;; no functions to register, just return an empty promise
      (return (local.get $new-promise))
    ))

    (array.len (local.get $arguments))
    (i32.const 0)
    (i32.gt_s)
    (if (then
      ;; we have at least one argument, so a callback was passed
      (array.get $JSArgs (local.get $arguments) (i32.const 0))
      (local.tee $callback-anyref)
      (if (ref.test (ref $Function))
        (then
          ;; set the then callback on the new promise
          (struct.set $Promise $finally-callback (local.get $new-promise) (ref.cast (ref $Function) (local.get $callback-anyref)))
        )
      )
    ))

    (if (i32.eqz (struct.get $Promise $resolved (local.get $promise)))
      (then
        ;; it's not resolved, check if it not errored
        (if (i32.eqz (struct.get $Promise $errored (local.get $promise)))
          (then
            (call $add-to-promise-chain
              (local.get $promise) ;; target promise
              (local.get $new-promise) ;; promise to add
            )
          )
          (else
            ;; it errored already, evaluate on-rejected
            (local.get $scope)
            (struct.get $Promise $error-result (local.get $promise))
            (local.get $new-promise)
            (call $evaluate-rejected)
          )
        )
      )
      (else
        ;; if it's already resolved, evaluate onresolved
        (local.get $scope)
        (struct.get $Promise $success-result (local.get $promise))
        (local.get $new-promise)
        (call $evaluate-resolved)
      )
    )

    (return (local.get $new-promise))
  )

  (func $Promise-catch
    (type $JSFunc)
    (param $scope (ref $Scope))
    (param $this anyref)
    (param $arguments (ref $JSArgs))
    (result anyref)

    (local $resolver (ref $Function))
    (local $new-promise (ref $Promise))
    (local $promise (ref $Promise))
    (local $callback-anyref anyref)
    (local $callback (ref $Function))
    (local $callback-arguments (ref $JSArgs))

    (local.set $new-promise (call $new-promise))
    (local.set $promise (ref.cast (ref $Promise) (local.get $this)))

    (array.len (local.get $arguments))
    (i32.eqz)
    (if (then
      ;; no functions to register, just return an empty promise
      (return (local.get $new-promise))
    ))

    (array.len (local.get $arguments))
    (i32.const 0)
    (i32.gt_s)
    (if (then
      ;; we have at least one argument, just pass it as a second argument to `then`
      (ref.null any)
      (array.get $JSArgs (local.get $arguments) (i32.const 0))
      (call $create-arguments-2)
      (local.set $arguments)

      (call $Promise-then (local.get $scope) (local.get $new-promise) (local.get $arguments))
      (drop)
    ))

    (return (local.get $new-promise))
  )

  (func $evaluate-resolved (param $scope (ref $Scope)) (param $previous-result anyref) (param $promise (ref $Promise))
    (local $arguments (ref $JSArgs))
    (local $anyref_function anyref)
    (local $result anyref)

    try
      (call $create-arguments-1 (local.get $previous-result))
      (local.set $arguments)

      (struct.get $Promise $finally-callback (local.get $promise))
      (local.tee $anyref_function)
      (if (ref.test (ref $Function))
        (then
          (ref.cast (ref $Function) (local.get $anyref_function))
          (ref.null any) ;; this
          (local.get $arguments)
          (call $call_function)
          (local.set $result)
        )
      )

      (struct.get $Promise $then-callback (local.get $promise))
      (local.tee $anyref_function)
      (if (ref.test (ref $Function))
        (then
          (ref.cast (ref $Function) (local.get $anyref_function))
          (ref.null any) ;; this
          (local.get $arguments)
          (call $call_function)
          (local.set $result)
        )
        (else
          (local.set $result (local.get $previous-result))
        )
      )

      ;; reuse $arguments
      (array.set $JSArgs (local.get $arguments) (i32.const 0) (local.get $result))

      (call $Promise-resolve (local.get $scope) (local.get $promise) (local.get $arguments))
      (drop)
    catch $JSException
      (call $create-arguments-1)
      (local.set $arguments)

      (call $Promise-reject (local.get $scope) (local.get $promise) (local.get $arguments))
      (drop)
    end
  )

  (func $evaluate-rejected (param $scope (ref $Scope)) (param $previous-result anyref) (param $promise (ref $Promise))
    (local $arguments (ref $JSArgs))
    (local $anyref_function anyref)
    (local $result anyref)

    try
      (call $create-arguments-1 (local.get $previous-result))
      (local.set $arguments)

      (struct.get $Promise $finally-callback (local.get $promise))
      (local.tee $anyref_function)
      (if (ref.test (ref $Function))
        (then
          (ref.cast (ref $Function) (local.get $anyref_function))
          (ref.null any) ;; this
          (local.get $arguments)
          (call $call_function)
          (local.set $result)
        )
      )

      (struct.get $Promise $catch-callback (local.get $promise))
      (local.tee $anyref_function)
      (if (ref.test (ref $Function))
        (then
          (ref.cast (ref $Function) (local.get $anyref_function))
          (ref.null any) ;; this
          (local.get $arguments)
          (call $call_function)
          (local.set $result)

          ;; reuse $arguments
          (array.set $JSArgs (local.get $arguments) (i32.const 0) (local.get $result))

          ;; if there was no errors we resolve and not reject
          (call $Promise-resolve (local.get $scope) (local.get $promise) (local.get $arguments))
          (drop)
        )
        (else
          (array.set $JSArgs (local.get $arguments) (i32.const 0) (local.get $previous-result))

          ;; there was no onrejected callback, so we pass further
          (call $Promise-reject (local.get $scope) (local.get $promise) (local.get $arguments))
          (drop)
        )
      )
    catch $JSException
      (call $create-arguments-1)
      (local.set $arguments)

      (call $Promise-reject (local.get $scope) (local.get $promise) (local.get $arguments))
      (drop)
    end
  )

  (func $add-to-promise-chain (param $target-promise (ref $Promise)) (param $promise (ref $Promise))
    (local $promises (ref null $PromisesArray))
    (local $len i32)
    (local $i i32)
    (local $new-promises (ref null $PromisesArray))

    (local.set $promises (struct.get $Promise $chained-promises (local.get $target-promise)))
    (local.set $len (array.len (local.get $promises)))
    (local.set $i (i32.const 0))

    (block $break (loop $find
      (br_if $break (i32.ge_u (local.get $i) (local.get $len)))
      
      (array.get $PromisesArray (local.get $promises) (local.get $i))
      (if (ref.test nullref)
        (then
          ;; we found a null value, let's insert here
          (array.set $PromisesArray
            (local.get $promises)
            (local.get $i)
            (local.get $promise))

          (return)
        ))
      
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $find)
    ))
  
    ;; we haven't found any free space, let's double the array and try again 
    (local.set $new-promises
      (array.new $PromisesArray (ref.null $Promise) (i32.mul (local.get $len) (i32.const 2))))

    (array.copy
      $PromisesArray             ;; dest type
      $PromisesArray             ;; source type
      (local.get $new-promises)  ;; dest array
      (i32.const 0)              ;; dest offset
      (local.get $promises)      ;; source array
      (i32.const 0)              ;; source data offest
      (local.get $len)           ;; source data length
    )

    ;; set new promieses on the $promise object
    (struct.set $Promise $chained-promises (local.get $target-promise) (ref.cast (ref $PromisesArray) (local.get $new-promises)))

    ;; try again
    (call $add-to-promise-chain (local.get $target-promise) (local.get $promise))
  )

  (func $Promise-resolve
    (type $JSFunc)
    (param $scope (ref $Scope))
    (param $this anyref)
    (param $arguments (ref $JSArgs))
    (result anyref)

    (local $resolve-result anyref)
    (local $promise (ref $Promise))
    (local $current-promise (ref $Promise))
    (local $promises (ref $PromisesArray))
    (local $i i32)
    (local $function (ref $Function))
    (local $callback-arguments (ref $JSArgs))
    (local $len i32)

    (local.set $i (i32.const 0))

    (ref.cast (ref $Promise) (local.get $this))
    (local.set $promise)

    (struct.get $Promise $chained-promises (local.get $promise))
    (local.set $promises)

    (if (i32.eqz (array.len (local.get $arguments)))
      (then
        ;; no arguments, so result is undefined
        (local.set $resolve-result (ref.null any))
      )
      (else
        (array.get $JSArgs (local.get $arguments) (i32.const 0))
        (local.set $resolve-result)
      )
    )

    ;; TODO: handle errors
    (local.get $promise)
    (local.get $resolve-result)
    (struct.set $Promise $success-result)

    (local.get $promise)
    (i32.const 1)
    (struct.set $Promise $resolved)

    (local.set $len (array.len (local.get $promises)))
    (block $break (loop $find
      (br_if $break (i32.ge_u (local.get $i) (local.get $len)))

      (if (i32.eqz (ref.test nullref (array.get $PromisesArray (local.get $promises) (local.get $i))))
        (then
          (array.get $PromisesArray (local.get $promises) (local.get $i))
          (ref.cast (ref $Promise))
          (local.set $promise)

          (call $evaluate-resolved (local.get $scope) (local.get $resolve-result) (local.get $promise))
        ))

      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $find)
    ))
 
    ;; return undefined
    (ref.null any)
  )

  (func $Promise-reject
    (type $JSFunc)
    (param $scope (ref $Scope))
    (param $this anyref)
    (param $arguments (ref $JSArgs))
    (result anyref)

    (local $reject-result anyref)
    (local $promise (ref $Promise))
    (local $current-promise (ref $Promise))
    (local $promises (ref $PromisesArray))
    (local $i i32)
    (local $function (ref $Function))
    (local $callback-arguments (ref $JSArgs))
    (local $len i32)

    (local.set $i (i32.const 0))

    (ref.cast (ref $Promise) (local.get $this))
    (local.set $promise)

    (struct.get $Promise $chained-promises (local.get $promise))
    (local.set $promises)

    (if (i32.eqz (array.len (local.get $arguments)))
      (then
        ;; no arguments, so result is undefined
        (local.set $reject-result (ref.null any))
      )
      (else
        (array.get $JSArgs (local.get $arguments) (i32.const 0))
        (local.set $reject-result)
      )
    )

    (local.get $promise)
    (local.get $reject-result)
    (struct.set $Promise $error-result)

    (local.get $promise)
    (i32.const 1)
    (struct.set $Promise $errored)

    (local.set $len (array.len (local.get $promises)))
    (block $break (loop $find
      (br_if $break (i32.ge_u (local.get $i) (local.get $len)))

      (if (i32.eqz (ref.test nullref (array.get $PromisesArray (local.get $promises) (local.get $i))))
        (then
          (array.get $PromisesArray (local.get $promises) (local.get $i))
          (ref.cast (ref $Promise))
          (local.set $promise)

          (call $evaluate-rejected (local.get $scope) (local.get $reject-result) (local.get $promise))
        ))
      
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $find)
    ))
 
    ;; return undefined
    (ref.null any)

  )

  (func $Promise-constructor
    (type $JSFunc)
    (param $scope (ref $Scope))
    (param $this anyref)
    (param $arguments (ref $JSArgs))
    (result anyref)

    (local $resolver (ref $Function))
    (local $arg1 anyref)
    (local $callback-arguments (ref $JSArgs))
    (local $promise (ref $Promise))
    (local $error anyref)

    (local.set $promise (call $new-promise))

    (array.len (local.get $arguments))
    (i32.eqz)
    (if (then
      (call $new_static_string (i32.const {{ data(str="Promise resolver undefined is not a function") }}) (i32.const {{ data_length() }}))
      (throw $JSException)
    ))

    (array.get $JSArgs (local.get $arguments) (i32.const 0))
    (local.tee $arg1)
    (if (ref.test (ref $Function))
      (then
        (call $new_function (ref.cast (ref $Scope) (global.get $scope)) (ref.func $Promise-resolve) (local.get $promise))
        (call $new_function (ref.cast (ref $Scope) (global.get $scope)) (ref.func $Promise-reject) (local.get $promise))
        (call $create-arguments-2)
        (local.set $callback-arguments)

        try
          (ref.cast (ref $Function) (local.get $arg1))
          (ref.null any) ;; this
          (local.get $callback-arguments)
          (call $call_function)
          (drop)
        catch $JSException
          (local.tee $error)
          (call $create-arguments-1)
          (local.set $arguments)

          (call $Promise-reject (local.get $scope) (local.get $promise) (local.get $arguments))
          (drop)
        end

        (return (local.get $promise))
      )
      (else
        ;; TODO: this need to create a dynamic string and convert the argument to string
        (call $new_static_string (i32.const {{ data(str="Promise resolver is not a function") }}) (i32.const {{ data_length() }}))
        (throw $JSException)
      )
    )

    (return (ref.null any))
  )

  (func $create-arguments-1 (param $arg1 anyref) (result (ref $JSArgs))
    (local $arguments (ref $JSArgs))
    (array.new $JSArgs (ref.null any) (i32.const 1))
    (local.set $arguments)

    (array.set $JSArgs (local.get $arguments) (i32.const 0) (local.get $arg1))

    (local.get $arguments)
  )

  (func $create-arguments-2 (param $arg1 anyref) (param $arg2 anyref) (result (ref $JSArgs))
    (local $arguments (ref $JSArgs))
    (array.new $JSArgs (ref.null any) (i32.const 2))
    (local.set $arguments)

    (array.set $JSArgs (local.get $arguments) (i32.const 0) (local.get $arg1))
    (array.set $JSArgs (local.get $arguments) (i32.const 1) (local.get $arg2))

    (local.get $arguments)
  )

  ;; this function returns the first argument if it's an object, otherwise it returns the
  ;; second argument. it's used when deciding what to return from a new statement
  (func $return_object_or (param $first anyref) (param $second anyref) (result anyref)
    (if (i32.or 
          (ref.test (ref $Object) (local.get $first))
          ;; Promise is also an object
          (ref.test (ref $Promise) (local.get $first)))
      (then
        (return (local.get $first))
      )
      (else
        (return (local.get $second))
      ))

    (ref.null any)
  )

  {{additional_functions}}

  ;; TODO: we could use data from (data) entries for creating strings, but in order
  ;; to do that there would have to be a function with mapping between data labels
  ;; and offsets, cause it's not possible to pass a data label to a function
  ;; Function to create a String from two parts
  (func $add_static_strings (param $ptr1 i32) (param $len1 i32) (param $ptr2 i32) (param $len2 i32) (result (ref $String))
    (local $total_length i32)
    (local $string_data (ref $CharArray))
    (local $i i32)
    
    ;; Calculate total length
    (local.set $total_length 
      (i32.add
        (local.get $len1)
        (local.get $len2)
      )
    )
    
    ;; Create new byte array for string data
    (local.set $string_data (array.new_default $CharArray (local.get $total_length)))
    
    ;; Copy first part
    (local.set $i (i32.const 0))
    (block $break1
      (loop $copy1
        (br_if $break1 (i32.ge_u (local.get $i) (local.get $len1)))
        
        (array.set $CharArray (local.get $string_data)
          (local.get $i)
          (i32.load8_u (i32.add (local.get $ptr1) (local.get $i)))
        )
        
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $copy1)
      )
    )
    ;; Copy second part
    (local.set $i (i32.const 0))
    (block $break2
      (loop $copy2
        (br_if $break2 (i32.ge_u (local.get $i) (local.get $len2)))
        
        (array.set $CharArray (local.get $string_data) 
          (i32.add (local.get $len1) (local.get $i))
          (i32.load8_u (i32.add (local.get $ptr2) (local.get $i)))
        )
        
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $copy2)
      )
    )
    
    ;; Create and return new String struct
    (struct.new $String
      (local.get $string_data)    ;; data field
      (local.get $total_length)   ;; length field
    )
  )

  (func $add_static_string_to_string (param $str (ref $String)) (param $ptr i32) (param $len i32) (result (ref $String))
    (local $total_length i32)
    (local $new_string_data (ref $CharArray))
    (local $string_data (ref $CharArray))
    (local $i i32)
    (local $str_len i32)
    
    (local.set $str_len
      (struct.get $String $length (local.get $str)))
    (local.set $string_data
      (struct.get $String $data (local.get $str)))

    ;; Calculate total length
    (local.set $total_length 
      (i32.add
        (local.get $str_len)
        (local.get $len)
      )
    )
    
    ;; Create new byte array for string data
    (local.set $new_string_data (array.new_default $CharArray (local.get $total_length)))
    
    ;; Copy data from string
    (array.copy
      $CharArray               ;; dest type
      $CharArray               ;; source type
      (local.get $new_string_data) ;; dest array
      (i32.const 0)            ;; dest offset
      (local.get $string_data) ;; source array
      (i32.const 0)            ;; source data offest
      (local.get $str_len)     ;; source data length
    )
    
    ;; Copy from $StaticString
    (local.set $i (i32.const 0))
    (block $break2
      (loop $copy2
        (br_if $break2 (i32.ge_u (local.get $i) (local.get $len)))
        
        (array.set $CharArray (local.get $new_string_data) 
          (i32.add (local.get $str_len) (local.get $i))
          (i32.load8_u (i32.add (local.get $ptr) (local.get $i)))
        )
        
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $copy2)
      )
    )
    
    ;; Create and return new String struct
    (struct.new $String
      (local.get $new_string_data)    ;; data field
      (local.get $total_length)   ;; length field
    )
  )


  (func $new_hashmap (result (ref $HashMap))
    (struct.new $HashMap
      (array.new $EntriesArray (ref.null $HashMapEntry) (i32.const 10))
      (i32.const 0)
    )
  )

  (func $new_hashmap_i32 (result (ref $HashMapI32))
    (struct.new $HashMapI32
      (array.new $EntriesArrayI32 (ref.null $HashMapEntryI32) (i32.const 10))
      (i32.const 0)
    )
  )

  (func $new_object (result (ref $Object))
    (struct.new $Object
      (call $new_hashmap)
      (ref.null any)
    )
  )

  (func $new_array (param $size i32) (result (ref $Array))
    (struct.new $Array
      (array.new $AnyrefArray (ref.null any) (local.get $size))
    )
  )

  (func $new_function
    (param $scope (ref $Scope))
    (param $function (ref $JSFunc))
    (param $this anyref)
    (result (ref $Function))

    (struct.new $Function
      (local.get $scope)
      (local.get $function)
      (local.get $this)
      (call $new_hashmap)
    )
  )

  (func $new_number (param $number f64) (result (ref $Number))
    (struct.new $Number
      (local.get $number)
    )
  )

  (func $new_boolean (param $bool i32) (result i31ref)
    (ref.i31 (local.get $bool))
    return
  )

  (func $cast_ref_to_i32_bool (param $arg anyref) (result i32)
    (local $res i32)
    (if (ref.test nullref (local.get $arg))
      (then
        (return (i32.const 0))
      )
    )
    (if (ref.test i31ref (local.get $arg))
      (then
        (i31.get_s (ref.cast (ref null i31) (local.get $arg)))
        (local.set $res)
        (if (i32.eq (local.get $res) (i32.const 1))
          ;; boolean true
          (then
            (return (i32.const 1))
          )
        )
        ;; anything else is false
        (return (i32.const 0))
      )
    )
    (if (ref.test (ref $Number) (local.get $arg))
      (then
        (return (i32.eqz (f64.eq
            (struct.get $Number 0 (ref.cast (ref $Number) (local.get $arg)))
            (f64.const 0)
          ))
        )
      )
    )
    (if (ref.test (ref $StaticString) (local.get $arg))
      (then
        (return (i32.const 1))
      )
    )
 
    i32.const 0
  )

  (func $new_static_string (param $offset i32) (param $length i32) (result (ref $StaticString))
    (struct.new $StaticString
      (local.get $offset)
      (local.get $length)
    )
  )

  (func $new_scope (param $parent (ref null $Scope)) (result (ref $Scope))
    (struct.new $Scope
      (local.get $parent)
      (call $new_hashmap)
      (call $new_hashmap_i32)
    )
  )

  ;; it's similar to set_variable as it writes to a variable on the scope, but
  ;; it checks if a variable is available in the scope first
  (func $assign_variable (param $scope (ref $Scope)) (param $name i32) (param $value anyref)
    (;(local $existing_type i32);)
    (;;)
    (;;; this needs to also check parents;)
    (;(call $hashmap_get_i32;)
    (;  (struct.get $Scope $var_types (local.get $scope));)
    (;  (local.get $name);)
    (;);)
    (;(local.set $existing_type);)
    (;;)
    (;(if (i32.eq (local.get $existing_type) (i32.const 0));)
    (;  (then;)
    (;    ;; 0 means it's a const, we have to throw an error;)
    (;    ;; TODO: throw proper eception type;)
    (;    (throw $JSException (ref.i31 (i32.const 12)));)
    (;  );)
    (;);)
    (;;)
    (;(call $hashmap_set;)
    (;  (struct.get $Scope $variables (local.get $scope));)
    (;  (local.get $name);)
    (;  (local.get $value);)
    (;);)

    (local $current_scope (ref null $Scope))
    (local $found-value anyref)

    (local.set $current_scope (local.get $scope))
    (loop $search_loop
      (local.set $found-value
        (call $hashmap_get
          (struct.get $Scope $variables (local.get $current_scope))
          (local.get $name)
        )
      )
      (if (call $is_no_value_found (local.get $found-value))
        (then
          (local.set $current_scope (struct.get $Scope $parent (local.get $current_scope)))
          (if (ref.is_null (local.get $current_scope))
            (then
              (throw $JSException (ref.i31 (local.get $name)))
            )
          )
 
          (br $search_loop)
        )
        (else
          ;; we found the variable declared on the $current_scope, so
          ;; set the variable on the $current_scope
          (call $set_variable (ref.cast (ref $Scope) (local.get $current_scope)) (local.get $name) (local.get $value))
        )
      )
    )

    ;; TODO: if there's no variable found JS still lets you assign, but
    ;; it saves it on the global scope
    (call $set_variable (ref.cast (ref $Scope) (global.get $scope)) (local.get $name) (local.get $value))
  )

  ;; TODO: for let and const we need to check if the values already exist
  (func $set_variable (param $scope (ref $Scope)) (param $name i32) (param $value anyref)
    (local $existing_type i32)

    (call $hashmap_get_i32
      (struct.get $Scope $var_types (local.get $scope))
      (local.get $name)
    )
    (local.set $existing_type)

    (if (i32.eq (local.get $existing_type) (i32.const 0))
      (then
        ;; 0 means it's a const, we have to throw an error
        ;; TODO: throw proper eception type
        (throw $JSException (ref.i31 (i32.const 12)))
      )
    )

    (call $hashmap_set
      (struct.get $Scope $variables (local.get $scope))
      (local.get $name)
      (local.get $value)
    )
;;    (call $hashmap_set_i32
;;      (struct.get $Scope $var_types (local.get $scope))
;;      (local.get $name)
;;      (local.get $var_type)
;;    )
  )

  (func $declare_variable (param $scope (ref $Scope)) (param $name i32) (param $value anyref) (param $var_type i32)
    (local $existing_type i32)

    (call $hashmap_get_i32
      (struct.get $Scope $var_types (local.get $scope))
      (local.get $name)
    )
    (local.set $existing_type)

    (if (i32.or
          (i32.eq (local.get $existing_type) (i32.const 3))
          (i32.or
            (i32.eq (local.get $existing_type) (i32.const -1))
            (i32.eq (local.get $existing_type) (i32.const  2))))
      (then
        ;; -1 means there is no such var in the hashmap, we can declare no matter what
        ;; 2 means var and 3 means param, which are also valid to overwrite
        (call $hashmap_set
          (struct.get $Scope $variables (local.get $scope))
          (local.get $name)
          (local.get $value)
        )
        (call $hashmap_set_i32
          (struct.get $Scope $var_types (local.get $scope))
          (local.get $name)
          (local.get $var_type)
        )
        (return)
      )
    )

    (if (i32.eq (local.get $existing_type) (i32.const 0))
      (then
        ;; 0 means it's a const, we have to throw an error
        ;; TODO: throw proper eception type
        (throw $JSException (ref.i31 (i32.const 10)))
      )
    )

    (if (i32.eq (local.get $existing_type) (i32.const 1))
      (then
        ;; 1 means it's a let, we have to throw an error
        ;; TODO: throw proper eception type
        (throw $JSException (ref.i31 (i32.const 11)))
      )
    )
  )

  (func $is_no_value_found (param $arg anyref) (result i32)
    (if (ref.test nullref (local.get $arg))
      (then
        (return (i32.const 0))))

    (if (ref.test i31ref (local.get $arg))
      (then
        (i32.const -1)
        (i31.get_s (ref.cast i31ref (local.get $arg)))
        (if (i32.eq)
          (then
            (return (i32.const 1))
          ))))

    (return (i32.const 0))
  )

  (func $get_variable (param $scope (ref $Scope)) (param $name i32) (result anyref)
    (local $current_scope (ref null $Scope))
    (local $value anyref)

    (local.set $current_scope (local.get $scope))
    (loop $search_loop
      (local.set $value
        (call $hashmap_get
          (struct.get $Scope $variables (local.get $current_scope))
          (local.get $name)
        )
      )
      (if (call $is_no_value_found (local.get $value))
        (then
          (local.set $current_scope (struct.get $Scope $parent (local.get $current_scope)))
          (if (ref.is_null (local.get $current_scope))
            (then
              (throw $JSException (ref.i31 (local.get $name)))
            )
          )
 
          (br $search_loop)
        )
        (else
          (return (local.get $value))
        )
      )
    )
    (throw $JSException (ref.i31 (i32.const 103)))
  )

  ;; TODO: this should also handle prototypes
  (func $get_property (param $target anyref) (param $name i32) (result anyref)
    (local $result anyref)
    (if (ref.test (ref $Object) (local.get $target))
      (then
        (call $hashmap_get
          (struct.get $Object $properties (ref.cast (ref $Object) (local.get $target)))
          (local.get $name)
        )
        (local.set $result)
      )
      (else
        ;; TODO: as long as objects like Function and String are just another objects
        ;; we will have to reimplement a lot of stuff like this. It would be great
        ;; to research parent and child types
        (if (ref.test (ref $Function) (local.get $target))
          (then
            (call $hashmap_get
              (struct.get $Function $properties (ref.cast (ref $Function) (local.get $target)))
              (local.get $name)
            )
            (local.set $result)
          )
          (else
            (if (ref.test (ref $Promise) (local.get $target))
              (then
                (call $hashmap_get
                  (struct.get $Promise $properties (ref.cast (ref $Promise) (local.get $target)))
                  (local.get $name)
                )
                (local.tee $result)
                (if (ref.test i31ref)
                  (then
                    (if (i32.eq 
                          (i31.get_s (ref.cast i31ref (local.get $result)))
                          (i32.const -1))
                      (then
                        ;; if the result is i31ref -1, it means nothing was found, so
                        ;; we need to search the $prototype
                        (ref.cast (ref $Promise) (local.get $target))
                        (struct.get $Promise $prototype)
                        (local.get $name)
                        (call $get_property)
                        (local.set $result)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    (if (ref.test i31ref (local.get $result))
      (then
        (if (i32.eq
              (i31.get_s (ref.cast i31ref (local.get $result)))
              (i32.const -1))
          (then
            (throw $JSException (ref.i31 (i32.const 100)))
          )
        )
      )
    )

    (if (ref.test (ref $Function) (local.get $result))
      (then
        (ref.cast (ref $Function) (local.get $result))
        (local.get $target)
        (struct.set $Function $this)
      )
    )

    (local.get $result)
    (return)
  )

  (func $set_property (param $target anyref) (param $name i32) (param $value anyref)
    (if (ref.test (ref $Object) (local.get $target))
      (then
        (call $hashmap_set
          (struct.get $Object $properties (ref.cast (ref $Object) (local.get $target)))
          (local.get $name)
          (local.get $value)
        )
        (return)
      )
    )

    (if (ref.test (ref $Function) (local.get $target))
      (then
        (call $hashmap_set
          (struct.get $Function $properties (ref.cast (ref $Function) (local.get $target)))
          (local.get $name)
          (local.get $value)
        )
        (return)
      )
    )

    (if (ref.test (ref $Promise) (local.get $target))
      (then
        (call $hashmap_set
          (struct.get $Promise $properties (ref.cast (ref $Promise) (local.get $target)))
          (local.get $name)
          (local.get $value)
        )
        (return)
      )
    )

    (throw $JSException (ref.i31 (i32.const 101)))
  )

  (func $hashmap_set (param $map (ref $HashMap)) (param $key i32) (param $value anyref)
    (local $entries (ref $EntriesArray))
    (local $new_entry (ref $HashMapEntry))
    (local $new_size i32)
    (local $new_entries (ref $EntriesArray))
    (local $i i32)
    (local $found i32)  ;; New local to track if we found the key

    (local.set $entries (struct.get $HashMap $entries (local.get $map)))
    (local.set $new_entry (struct.new $HashMapEntry (local.get $key) (local.get $value)))
    (local.set $found (i32.const 0))  ;; Initialize found flag to false

    ;; First, search for existing key
    (local.set $i (i32.const 0))
    (loop $search_loop
      (if (i32.lt_u (local.get $i) (struct.get $HashMap $size (local.get $map)))
        (then
          (if (i32.eq
                (struct.get $HashMapEntry $key (array.get $EntriesArray (local.get $entries) (local.get $i)))
                (local.get $key))
            (then
              ;; Key found - update the value
              (array.set $EntriesArray (local.get $entries) (local.get $i) (local.get $new_entry))
              (local.set $found (i32.const 1))  ;; Set found flag to true
            )
          )
          (if (i32.eqz (local.get $found))  ;; Only continue searching if not found
            (then
              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              (br $search_loop)
            )
          )
        )
      )
    )

    ;; If key wasn't found, proceed with insertion
    (if (i32.eqz (local.get $found))
      (then
        ;; Check if we need to resize
        (if (i32.ge_u (struct.get $HashMap $size (local.get $map)) (array.len (local.get $entries)))
          (then
            (local.set $new_size (i32.mul (array.len (local.get $entries)) (i32.const 2)))
            (local.set $new_entries (array.new $EntriesArray (ref.null $HashMapEntry) (local.get $new_size)))

            (local.set $i (i32.const 0))
            (loop $copy_loop
              (if (i32.lt_u (local.get $i) (array.len (local.get $entries)))
                (then
                  (array.set $EntriesArray (local.get $new_entries) (local.get $i)
                    (array.get $EntriesArray (local.get $entries) (local.get $i)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $copy_loop)
                )
              )
            )

            (struct.set $HashMap $entries (local.get $map) (local.get $new_entries))
            (local.set $entries (local.get $new_entries))
          )
        )

        ;; Add new entry and increment size
        (array.set $EntriesArray (local.get $entries) (struct.get $HashMap $size (local.get $map)) (local.get $new_entry))
        (struct.set $HashMap $size (local.get $map) (i32.add (struct.get $HashMap $size (local.get $map)) (i32.const 1)))
      )
    )
  )

  (func $hashmap_set_i32 (param $map (ref $HashMapI32)) (param $key i32) (param $value i32)
    (local $entries (ref $EntriesArrayI32))
    (local $new_entry (ref $HashMapEntryI32))
    (local $new_size i32)
    (local $new_entries (ref $EntriesArrayI32))
    (local $i i32)
    (local $found i32)  ;; New local to track if we found the key

    (local.set $entries (struct.get $HashMapI32 $entries (local.get $map)))
    (local.set $new_entry (struct.new $HashMapEntryI32 (local.get $key) (local.get $value)))
    (local.set $found (i32.const 0))  ;; Initialize found flag to false

    ;; First, search for existing key
    (local.set $i (i32.const 0))
    (loop $search_loop
      (if (i32.lt_u (local.get $i) (struct.get $HashMapI32 $size (local.get $map)))
        (then
          (if (i32.eq
                (struct.get $HashMapEntryI32 $key (array.get $EntriesArrayI32 (local.get $entries) (local.get $i)))
                (local.get $key))
            (then
              ;; Key found - update the value
              (array.set $EntriesArrayI32 (local.get $entries) (local.get $i) (local.get $new_entry))
              (local.set $found (i32.const 1))  ;; Set found flag to true
            )
          )
          (if (i32.eqz (local.get $found))  ;; Only continue searching if not found
            (then
              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              (br $search_loop)
            )
          )
        )
      )
    )

    ;; If key wasn't found, proceed with insertion
    (if (i32.eqz (local.get $found))
      (then
        ;; Check if we need to resize
        (if (i32.ge_u (struct.get $HashMapI32 $size (local.get $map)) (array.len (local.get $entries)))
          (then
            (local.set $new_size (i32.mul (array.len (local.get $entries)) (i32.const 2)))
            (local.set $new_entries (array.new $EntriesArrayI32 (ref.null $HashMapEntryI32) (local.get $new_size)))

            (local.set $i (i32.const 0))
            (loop $copy_loop
              (if (i32.lt_u (local.get $i) (array.len (local.get $entries)))
                (then
                  (array.set $EntriesArrayI32 (local.get $new_entries) (local.get $i)
                    (array.get $EntriesArrayI32 (local.get $entries) (local.get $i)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $copy_loop)
                )
              )
            )

            (struct.set $HashMapI32 $entries (local.get $map) (local.get $new_entries))
            (local.set $entries (local.get $new_entries))
          )
        )

        ;; Add new entry and increment size
        (array.set $EntriesArrayI32 (local.get $entries) (struct.get $HashMapI32 $size (local.get $map)) (local.get $new_entry))
        (struct.set $HashMapI32 $size (local.get $map) (i32.add (struct.get $HashMapI32 $size (local.get $map)) (i32.const 1)))
      )
    )
  )

  (func $hashmap_get (param $map (ref $HashMap)) (param $key i32) (result anyref)
    (local $i i32)
    (local $entries (ref $EntriesArray))
    (local.set $entries (struct.get $HashMap $entries (local.get $map)))
    (local.set $i (i32.const 0))
    (loop $search_loop
      (if (i32.lt_u (local.get $i) (struct.get $HashMap $size (local.get $map)))
        (then
          (if (i32.eq
                (struct.get $HashMapEntry $key (array.get $EntriesArray (local.get $entries) (local.get $i)))
                (local.get $key))
            (then
              (return (struct.get $HashMapEntry $value (array.get $EntriesArray (local.get $entries) (local.get $i))))
            )
          )
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $search_loop)
        )
      )
    )

    ;; we need to somehow differentiate between value not being found and the found
    ;; value being null
    (ref.i31 (i32.const -1))
  )

  (func $hashmap_get_i32 (param $map (ref $HashMapI32)) (param $key i32) (result i32)
    (local $i i32)
    (local $entries (ref $EntriesArrayI32))
    (local.set $entries (struct.get $HashMapI32 $entries (local.get $map)))
    (local.set $i (i32.const 0))
    (loop $search_loop
      (if (i32.lt_u (local.get $i) (struct.get $HashMapI32 $size (local.get $map)))
        (then
          (if (i32.eq
                (struct.get $HashMapEntryI32 $key (array.get $EntriesArrayI32 (local.get $entries) (local.get $i)))
                (local.get $key))
            (then
              (return (struct.get $HashMapEntryI32 $value (array.get $EntriesArrayI32 (local.get $entries) (local.get $i))))
            )
          )
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $search_loop)
        )
      )
    )

    ;; not sure if there is a better way to handle this, but since the result is i32,
    ;; we have to return something on not found key
    ;; if it was used for anything else than values greater than zero, we should probably
    ;; make a function like hashmap_exists_i32 and then if a key exists, assume hashmap_get returns
    ;; a proper key
    (i32.const -1)
  )

  (func $call_function (param $func anyref) (param $this anyref) (param $arguments (ref $JSArgs)) (result anyref)
    (local $function (ref $Function))
    (local $js_func (ref $JSFunc))
    (local $current_this anyref)

    (local.set $function (ref.cast (ref $Function) (local.get $func)))
    (local.set $js_func (struct.get $Function $func (local.get $function)))

    (local.get $this)
    (ref.test nullref)
    (if
      (then
        ;; if this is not passed let's use whatever is in the Function
        (struct.get $Function $this (local.get $function))
        (local.set $current_this)
      )
      (else
        (local.get $this)
        (local.set $current_this)
      )
    )

    (call_ref $JSFunc
      (struct.get $Function $scope (local.get $function))
      (local.get $current_this)
      (local.get $arguments)
      (local.get $js_func)
    )
  )

  (func $add (param $arg1 anyref) (param $arg2 anyref) (result anyref)
    (local $num1 (ref $Number))
    (local $num2 (ref $Number))
    (local $static_str1 (ref $StaticString))
    (local $static_str2 (ref $StaticString))
    (local $str1 (ref $String))
    (local $str2 (ref $String))
    (local $result f64)

    ;; TODO: this doesn't take into account casting, it can only add two objects
    ;; of the same type (and only numbers and strings for now)
    (if (i32.and
          (ref.test (ref $Number) (local.get $arg1))
          (ref.test (ref $Number) (local.get $arg2)))
      (then
        (local.set $num1 (ref.cast (ref $Number) (local.get $arg1)))
        (local.set $num2 (ref.cast (ref $Number) (local.get $arg2)))
        (local.set $result
          (f64.add
            (struct.get $Number $value (local.get $num1))
            (struct.get $Number $value (local.get $num2))
          )
        )
        (return (call $new_number (local.get $result)))
      )
    )

    (if (i32.and
          (ref.test (ref $StaticString) (local.get $arg1))
          (ref.test (ref $StaticString) (local.get $arg2)))
      (then
        (local.set $static_str1 (ref.cast (ref $StaticString) (local.get $arg1)))
        (local.set $static_str2 (ref.cast (ref $StaticString) (local.get $arg2)))

        (call $add_static_strings
          (struct.get $StaticString $offset (local.get $static_str1))
          (struct.get $StaticString $length (local.get $static_str1))
          (struct.get $StaticString $offset (local.get $static_str2))
          (struct.get $StaticString $length (local.get $static_str2))
        )
        (return)
      )
    )

    (if (i32.and
          (ref.test (ref $String) (local.get $arg1))
          (ref.test (ref $StaticString) (local.get $arg2)))
      (then
        (local.set $str1 (ref.cast (ref $String) (local.get $arg1)))
        (local.set $static_str2 (ref.cast (ref $StaticString) (local.get $arg2)))

        (call $add_static_string_to_string
          (local.get $str1)
          (struct.get $StaticString $offset (local.get $static_str2))
          (struct.get $StaticString $length (local.get $static_str2))
        )
        (return)
      )
    )

    (ref.null any)
  )

  (func $div (param $arg1 anyref) (param $arg2 anyref) (result anyref)
    (local $num1 (ref $Number))
    (local $num2 (ref $Number))
    (local $result f64)

    (if (i32.and
          (ref.test (ref $Number) (local.get $arg1))
          (ref.test (ref $Number) (local.get $arg2)))
      (then
        (local.set $num1 (ref.cast (ref $Number) (local.get $arg1)))
        (local.set $num2 (ref.cast (ref $Number) (local.get $arg2)))
        (local.set $result
          (f64.div
            (struct.get $Number 0 (local.get $num1))
            (struct.get $Number 0 (local.get $num2))
          )
        )
        (return (call $new_number (local.get $result)))
      )
    )
    (ref.null any)
  )

  (func $sub (param $arg1 anyref) (param $arg2 anyref) (result anyref)
    (local $num1 (ref $Number))
    (local $num2 (ref $Number))
    (local $result f64)

    (if (i32.and
          (ref.test (ref $Number) (local.get $arg1))
          (ref.test (ref $Number) (local.get $arg2)))
      (then
        (local.set $num1 (ref.cast (ref $Number) (local.get $arg1)))
        (local.set $num2 (ref.cast (ref $Number) (local.get $arg2)))
        (local.set $result
          (f64.sub
            (struct.get $Number 0 (local.get $num1))
            (struct.get $Number 0 (local.get $num2))
          )
        )
        (return (call $new_number (local.get $result)))
      )
    )
    (ref.null any)
  )

  (func $strict_not_equal (param $arg1 anyref) (param $arg2 anyref) (result i31ref)
    (return 
      (ref.i31
        (i32.eqz
          (i31.get_s
            (ref.cast 
              (ref null i31)
              (call $strict_equal (local.get $arg1) (local.get $arg2))))))
    )
  )

  (func $strict_equal (param $arg1 anyref) (param $arg2 anyref) (result i31ref)
    (local $num1 (ref $Number))
    (local $num2 (ref $Number))
    (local $result f64)

    ;; if both args are undefined, return true
    (if (i32.and
          (ref.test nullref (local.get $arg1))
          (ref.test nullref (local.get $arg2)))
      (then
        (return (ref.i31 (i32.const 1)))
      )
    )

    ;; if one value is undefined, return false
    (if (i32.or
          (ref.test nullref (local.get $arg1))
          (ref.test nullref (local.get $arg2)))
      (then
        (return (ref.i31 (i32.const 0)))
      )
    )

    (if (i32.and
          (ref.test (ref $Number) (local.get $arg1))
          (ref.test (ref $Number) (local.get $arg2)))
      (then
        (local.set $num1 (ref.cast (ref $Number) (local.get $arg1)))
        (local.set $num2 (ref.cast (ref $Number) (local.get $arg2)))
        (return 
          (ref.i31 (f64.eq
            (struct.get $Number 0 (local.get $num1))
            (struct.get $Number 0 (local.get $num2))
          ))
        )
      )
    )

    ;; if both args are bool or null and are equal, return true
    (if (i32.and
          (ref.test i31ref (local.get $arg1))
          (ref.test i31ref (local.get $arg2)))
      (then
        (return (ref.i31 (i32.const 1)))
      )
    )

    (ref.i31 (i32.const 0))
  )

  (func $logical_or (param $arg1 anyref) (param $arg2 anyref) (result anyref)
    (if (i32.eqz (ref.test nullref (local.get $arg1)))
      (then
        ;; if arg1 is not undefined we return arg1
        (return (local.get $arg1))
      )
    )

    ;; if arg1 is not (null or false) we also return arg1
    (if (ref.test i31ref (local.get $arg1))
      (then
        (if (i32.eqz (i32.or
              (i32.eq (i31.get_s (ref.cast (ref null i31) (local.get $arg1))) (i32.const 0))
              (i32.eq (i31.get_s (ref.cast (ref null i31) (local.get $arg1))) (i32.const 2))))
          (then
            (return (local.get $arg1))
          )
        )
      )
    )

    ;; otherwise we return arg2
    (return (local.get $arg2))
  )

  ;; TODO: handle numbers properly - 0 is false
  (func $logical_and (param $arg1 anyref) (param $arg2 anyref) (result anyref)
    (if (ref.test nullref (local.get $arg1))
      (then
        ;; if arg1 is undefined we return arg1
        (return (local.get $arg1))
      )
    )

    ;; if arg1 is null or false, we return arg1 too
    (if (ref.test i31ref (local.get $arg1))
      (then
        (if (i32.or
              (i32.eq (i31.get_s (ref.cast (ref null i31) (local.get $arg1))) (i32.const 0))
              (i32.eq (i31.get_s (ref.cast (ref null i31) (local.get $arg1))) (i32.const 2)))
          (then
            (return (local.get $arg1))
          )
        )
      )
    )

    ;; otherwise we return arg2
    (return (local.get $arg2))
  )

  (func $logical_not (param $arg anyref) (result i31ref)
    (if (ref.test nullref (local.get $arg))
      (then
        ;; if arg is undefined we return true
        (return (ref.i31 (i32.const 1)))
      )
    )

    (if (ref.test i31ref (local.get $arg))
      (then
        (if (i32.or
              (i32.eq (i31.get_s (ref.cast (ref null i31) (local.get $arg))) (i32.const 0))
              (i32.eq (i31.get_s (ref.cast (ref null i31) (local.get $arg))) (i32.const 2)))
          (then
            ;; if arg is null or false we return 1
            (return (ref.i31 (i32.const 1)))
          )
        )
      )
    )

    ;; otherwise we return false
    (return (ref.i31 (i32.const 0)))
  )

  (func $type_of (param $arg anyref) (result (ref $StaticString))
    (if (ref.test nullref (local.get $arg))
      (then
        (return (call $new_static_string (i32.const {{ data(str="undefined") }}) (i32.const {{ data_length() }})))
      )
    )

    (if (ref.test i31ref (local.get $arg))
      (then
        (if (i32.or
              (i32.eq (i31.get_s (ref.cast (ref null i31) (local.get $arg))) (i32.const 0))
              (i32.eq (i31.get_s (ref.cast (ref null i31) (local.get $arg))) (i32.const 1)))
          (then
            (return (call $new_static_string (i32.const {{ data(str="boolean") }}) (i32.const {{ data_length() }})))
          )
          (else
            (return (call $new_static_string (i32.const {{ data(str="undefined") }}) (i32.const {{ data_length() }})))
          )
        )
      )
    )

    (if (ref.test (ref $Number) (local.get $arg))
      (then
        (return (call $new_static_string (i32.const {{ data(str="number") }}) (i32.const {{ data_length() }}))))
    )

    (if (i32.or 
          (ref.test (ref $Object) (local.get $arg))
          (ref.test (ref $Promise) (local.get $arg)))
      (then
        (return (call $new_static_string (i32.const {{ data(str="object") }}) (i32.const {{ data_length() }}))))
    )

    (if (ref.test (ref $StaticString) (local.get $arg))
      (then
        (return (call $new_static_string (i32.const {{ data(str="string") }}) (i32.const {{ data_length() }}))))
    )

    (if (ref.test (ref $Function) (local.get $arg))
      (then
        (return (call $new_static_string (i32.const {{ data(str="function") }}) (i32.const {{ data_length() }}))))
    )

    (return (call $new_static_string (i32.const {{ data(str="undefined") }}) (i32.const {{ data_length() }})))
  )

  (func $less_than (param $arg1 anyref) (param $arg2 anyref) (result i31ref)
    (local $num1 (ref $Number))
    (local $num2 (ref $Number))
    (local $result f64)

    (if (i32.and
          (ref.test (ref $Number) (local.get $arg1))
          (ref.test (ref $Number) (local.get $arg2)))
      (then
        (local.set $num1 (ref.cast (ref $Number) (local.get $arg1)))
        (local.set $num2 (ref.cast (ref $Number) (local.get $arg2)))
        (return 
          (ref.i31 (f64.lt
            (struct.get $Number 0 (local.get $num1))
            (struct.get $Number 0 (local.get $num2))
          ))
        )
      )
    )

    (ref.i31 (i32.const 0))
  )

  (func $greater_than_or_equal (param $arg1 anyref) (param $arg2 anyref) (result i31ref)
    (local $num1 (ref $Number))
    (local $num2 (ref $Number))
    (local $result f64)

    (if (i32.and
          (ref.test (ref $Number) (local.get $arg1))
          (ref.test (ref $Number) (local.get $arg2)))
      (then
        (local.set $num1 (ref.cast (ref $Number) (local.get $arg1)))
        (local.set $num2 (ref.cast (ref $Number) (local.get $arg2)))
        (return 
          (ref.i31 (f64.ge
            (struct.get $Number 0 (local.get $num1))
            (struct.get $Number 0 (local.get $num2))
          ))
        )
      )
    )

    (ref.i31 (i32.const 0))
  )

  (func $increment_number (param $arg1 anyref) (result anyref)
    (local $num (ref $Number))
    (local $result f64)

    (if (ref.test (ref $Number) (local.get $arg1))
      (then
        (local.set $num (ref.cast (ref $Number) (local.get $arg1)))
        (local.set $result
          (f64.add
            (struct.get $Number 0 (local.get $num))
            (f64.const 1)
          )
        )
        (return (call $new_number (local.get $result)))
      )
    )
    (ref.null any)
  )

  ;; TODO: can we update in-place?
  (func $decrement_number (param $arg1 anyref) (result anyref)
    (local $num (ref $Number))
    (local $result f64)

    (if (ref.test (ref $Number) (local.get $arg1))
      (then
        (local.set $num (ref.cast (ref $Number) (local.get $arg1)))
        (local.set $result
          (f64.sub
            (struct.get $Number 0 (local.get $num))
            (f64.const 1)
          )
        )
        (return (call $new_number (local.get $result)))
      )
    )
    (ref.null any)
  )

  ;; TODO: fix this
(func $writeF64AsAscii (param $value f64) (param $offset i32) (result i32)
  (local $isNegative i32)
  (local $intPart i64)
  (local $currentOffset i32)
  (local $digitCount i32)
  (local $fracValue f64)
  
  ;; Initialize current offset
  (local.set $currentOffset (local.get $offset))
  
  ;; Handle negative numbers
  (local.set $isNegative 
    (i32.lt_s 
      (i32.wrap_i64 
        (i64.shr_u 
          (i64.reinterpret_f64 (local.get $value))
          (i64.const 63)
        )
      )
      (i32.const 0)
    )
  )
  
  ;; Write minus sign if negative
  (if (local.get $isNegative)
    (then
      ;; Write '-' (ASCII 45)
      (i32.store8 (local.get $currentOffset) (i32.const 45))
      (local.set $currentOffset (i32.add (local.get $currentOffset) (i32.const 1)))
      ;; Make value positive
      (local.set $value (f64.neg (local.get $value)))
    )
  )
  
  ;; Get integer part
  (local.set $intPart (i64.trunc_f64_s (local.get $value)))
  
  ;; Write integer part digits in reverse
  (local.set $digitCount (i32.const 0))
  (block $break
    (loop $digit_loop
      ;; Write current digit
      (i32.store8 
        (i32.add 
          (local.get $currentOffset) 
          (local.get $digitCount)
        )
        (i32.add
          (i32.wrap_i64 
            (i64.rem_u 
              (local.get $intPart)
              (i64.const 10)
            )
          )
          (i32.const 48)  ;; ASCII '0'
        )
      )
      (local.set $digitCount (i32.add (local.get $digitCount) (i32.const 1)))
      
      ;; Update intPart
      (local.set $intPart (i64.div_u (local.get $intPart) (i64.const 10)))
      
      ;; Continue if intPart > 0
      (br_if $digit_loop (i64.gt_s (local.get $intPart) (i64.const 0)))
    )
  )
  
  ;; Reverse the digits we just wrote
  (local.set $currentOffset 
    (call $reverseBytes 
      (local.get $currentOffset)
      (local.get $digitCount)
    )
  )
  
  ;; Get fractional part
  (local.set $fracValue (f64.sub 
    (local.get $value)
    (f64.convert_i64_s (i64.trunc_f64_s (local.get $value)))
  ))
  
  ;; Only write decimal point and fraction if there is a fractional part
  (if (f64.gt (local.get $fracValue) (f64.const 0))
    (then
      ;; Add decimal point
      (i32.store8 (local.get $currentOffset) (i32.const 46))  ;; ASCII '.'
      (local.set $currentOffset (i32.add (local.get $currentOffset) (i32.const 1)))
      
      ;; Write fractional digits until we hit zero or max precision
      (local.set $digitCount (i32.const 0))
      (block $frac_break
        (loop $frac_loop
          ;; Break if we've written 16 digits (typical f64 precision limit)
          (br_if $frac_break (i32.ge_u (local.get $digitCount) (i32.const 16)))
          
          ;; Multiply by 10 to get next digit
          (local.set $fracValue (f64.mul (local.get $fracValue) (f64.const 10)))
          
          ;; Get the digit
          (local.set $intPart (i64.trunc_f64_s (local.get $fracValue)))
          
          ;; Write the digit
          (i32.store8 
            (local.get $currentOffset)
            (i32.add
              (i32.wrap_i64 (local.get $intPart))
              (i32.const 48)  ;; ASCII '0'
            )
          )
          
          ;; Update fracValue
          (local.set $fracValue 
            (f64.sub 
              (local.get $fracValue)
              (f64.convert_i64_s (local.get $intPart))
            )
          )
          
          ;; Update counters
          (local.set $currentOffset (i32.add (local.get $currentOffset) (i32.const 1)))
          (local.set $digitCount (i32.add (local.get $digitCount) (i32.const 1)))
          
          ;; Continue if we have more significant digits
          (br_if $frac_loop (f64.gt (local.get $fracValue) (f64.const 0)))
        )
      )
    )
  )
  
  ;; Return number of bytes written
  (i32.sub (local.get $currentOffset) (local.get $offset))
)

;; Helper function to reverse bytes (same as before)
(func $reverseBytes (param $start i32) (param $length i32) (result i32)
  (local $left i32)
  (local $right i32)
  (local $temp i32)
  
  (local.set $left (local.get $start))
  (local.set $right (i32.sub 
    (i32.add (local.get $start) (local.get $length))
    (i32.const 1)
  ))
  
  (block $break
    (loop $reverse_loop
      (br_if $break (i32.ge_u (local.get $left) (local.get $right)))
      
      ;; Swap bytes
      (local.set $temp 
        (i32.load8_u (local.get $left))
      )
      (i32.store8 
        (local.get $left)
        (i32.load8_u (local.get $right))
      )
      (i32.store8 
        (local.get $right)
        (local.get $temp)
      )
      
      ;; Update pointers
      (local.set $left (i32.add (local.get $left) (i32.const 1)))
      (local.set $right (i32.sub (local.get $right) (i32.const 1)))
      (br $reverse_loop)
    )
  )
  
  (i32.add (local.get $start) (local.get $length))
)

  (func $log_string (param $str (ref $StaticString))
    (local.get $str)
    (call $create-arguments-1)
    (call $log)
  )

  (func $log (param $arguments (ref $JSArgs))
    (local $i i32)                    ;; loop counter
    (local $j i32)                    ;; loop counter
    (local $len i32)                  ;; length of arguments array
    (local $str_len i32)              ;; length of a processed sring
    (local $current anyref)           ;; current argument being processed
    (local $offset i32)               ;; current memory offset for data
    (local $iovectors_offset i32)     ;; offset for iovectors
    (local $written_length i32)       ;; length of written data
    (local $num_val f64)              ;; temporary storage for number value
    (local $static_str_ref (ref $StaticString))  ;; temporary storage for string reference
    (local $str_ref (ref $String))  ;; temporary storage for string reference
    (local $num_ref (ref $Number))    ;; temporary storage for number reference
    (local $handled i32)
    (local $i31_ref i31ref)
 
    ;; Get length of arguments array
    (local.set $len 
      (array.len (ref.cast (ref $JSArgs) (local.get $arguments)))
    )
 
    ;; Set initial offsets
    (local.set $iovectors_offset 
      (global.get $free_memory_offset)
    )
    ;; set offset for iovectors. we need place for arguments * 2
    ;; vectors, one for each argument, one for a space after each
    ;; arguments and one for a new line at the end
    (local.set $offset 
      (i32.add 
        (global.get $free_memory_offset)
        (i32.mul (local.get $len) (i32.const 16))
      )
    )
    
    ;; Initialize loop counter
    (local.set $i (i32.const 0))
    
    ;; Main loop through arguments
    (loop $process_args
      (block $process_arg
        ;; Get current argument
        (local.set $current 
          (array.get $JSArgs
            (local.get $arguments)
            (local.get $i)
          )
        )

        ;; argument not handled yet
        (local.set $handled (i32.const 0))
       
        ;; check if it's undefined
        (if (ref.test nullref (local.get $current))
          (then
            ;; Store iovector data
            (i32.store 
              (local.get $iovectors_offset)
              (i32.const {{ data(str="undefined") }})
            )
            (i32.store 
              (i32.add (local.get $iovectors_offset) (i32.const 4))
              (i32.const {{ data_length() }})
            )
            
            (local.set $iovectors_offset 
              (i32.add (local.get $iovectors_offset) (i32.const 8))
            )
    
            ;; argument handled
            (local.set $handled (i32.const 1))
          )
        )

        ;; check if it's null, false or a number
        (if (i32.and
              (ref.test i31ref (local.get $current))
              (i32.eqz (ref.test nullref (local.get $current))))
          (then
            (local.set $i31_ref
              (ref.cast i31ref (local.get $current)))

            (if (i32.eq
              (i31.get_s (ref.cast (ref null i31) (local.get $current)))
              (i32.const 0))
              (then
                ;; Store iovector data
                (i32.store
                  (local.get $iovectors_offset)
                  (i32.const {{ data(str="null") }})
                )
                (i32.store 
                  (i32.add (local.get $iovectors_offset) (i32.const 4))
                  (i32.const {{ data_length() }})
                )
                
                (local.set $iovectors_offset 
                  (i32.add (local.get $iovectors_offset) (i32.const 8))
                )
    
                ;; argument handled
                (local.set $handled (i32.const 1))
              )
            )

            (if (i32.eq
              (i31.get_s (ref.cast (ref null i31) (local.get $current)))
              (i32.const 1))
              (then
                ;; Store iovector data
                (i32.store
                  (local.get $iovectors_offset)
                  (i32.const {{ data(str="true") }})
                )
                (i32.store 
                  (i32.add (local.get $iovectors_offset) (i32.const 4))
                  (i32.const {{ data_length() }})
                )
                
                (local.set $iovectors_offset 
                  (i32.add (local.get $iovectors_offset) (i32.const 8))
                )
    
                ;; argument handled
                (local.set $handled (i32.const 1))
              )
            )

            (if (i32.eq
              (i31.get_s (ref.cast (ref null i31) (local.get $current)))
              (i32.const 2))
              (then
                ;; Store iovector data
                (i32.store
                  (local.get $iovectors_offset)
                  (i32.const {{ data(str="false") }})
                )
                (i32.store 
                  (i32.add (local.get $iovectors_offset) (i32.const 4))
                  (i32.const {{ data_length() }})
                )
                
                (local.set $iovectors_offset 
                  (i32.add (local.get $iovectors_offset) (i32.const 8))
                )
    
                ;; argument handled
              )
            )

            (local.set $num_val 
              (f64.convert_i32_s (i31.get_s (ref.cast (ref null i31) (local.get $current))))
            )
            
            ;; Write number to memory and get length
            (local.set $written_length
              (call $writeF64AsAscii 
                (local.get $num_val)
                (local.get $offset)
              )
            )
            
            ;; Store iovector data
            (i32.store (local.get $iovectors_offset) (local.get $offset))
            (i32.store 
              (i32.add (local.get $iovectors_offset) (i32.const 4))
              (local.get $written_length)
            )
            
            ;; Update offsets
            (local.set $offset 
              (i32.add (local.get $offset) (local.get $written_length))
            )
            (local.set $iovectors_offset 
              (i32.add (local.get $iovectors_offset) (i32.const 8))
            )
            (local.set $handled (i32.const 1))
          )
        )

        ;; Check if it's a Number
        (if (ref.test (ref $Number) (local.get $current))
          (then
            ;; Cast to Number and get value
            (local.set $num_ref
              (ref.cast (ref $Number) (local.get $current))
            )
            (local.set $num_val 
              (struct.get $Number 0 (local.get $num_ref))
            )
            
            ;; Write number to memory and get length
            (local.set $written_length
              (call $writeF64AsAscii 
                (local.get $num_val)
                (local.get $offset)
              )
            )
            
            ;; Store iovector data
            (i32.store (local.get $iovectors_offset) (local.get $offset))
            (i32.store 
              (i32.add (local.get $iovectors_offset) (i32.const 4))
              (local.get $written_length)
            )
            
            ;; Update offsets
            (local.set $offset 
              (i32.add (local.get $offset) (local.get $written_length))
            )
            (local.set $iovectors_offset 
              (i32.add (local.get $iovectors_offset) (i32.const 8))
            )
            
            ;; argument handled
            (local.set $handled (i32.const 1))
          )
        )
        
        ;; Check if it's a StaticString
        (if (ref.test (ref $StaticString) (local.get $current))
          (then
            ;; Cast to StaticString
            (local.set $static_str_ref
              (ref.cast (ref $StaticString) (local.get $current))
            )
            
            ;; Store iovector data
            (i32.store 
              (local.get $iovectors_offset)
              (struct.get $StaticString $offset (local.get $static_str_ref))
            )
            (i32.store 
              (i32.add (local.get $iovectors_offset) (i32.const 4))
              (struct.get $StaticString $length (local.get $static_str_ref))
            )
            
            (local.set $iovectors_offset 
              (i32.add (local.get $iovectors_offset) (i32.const 8))
            )
    
            ;; argument handled
            (local.set $handled (i32.const 1))
          )
        )

        ;; Check if it's a String
        (if (ref.test (ref $String) (local.get $current))
          (then
            (local.set $str_ref 
              (ref.cast (ref $String) (local.get $current))
            )
            
            (local.set $str_len (struct.get $String $length (local.get $str_ref)))
            ;; Store iovector data
            (i32.store 
              (local.get $iovectors_offset)
              (local.get $offset)
            )
            (i32.store 
              (i32.add (local.get $iovectors_offset) (i32.const 4))
              (local.get $str_len)
            )
            (local.set $j (i32.const 0))
            (block $break1
              (loop $copy1
                (br_if $break1 (i32.ge_u (local.get $j) (local.get $str_len)))

                (i32.store8 
                  (i32.add (local.get $offset) (local.get $j))
                  (array.get_u $CharArray 
                    (struct.get $String $data (local.get $str_ref))
                    (local.get $j)
                  )
                )
                
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $copy1)
              )
            )
            
            ;; Update offset for next write
            (local.set $offset 
              (i32.add 
                (local.get $offset)
                (struct.get $String $length (local.get $str_ref))
              )
            )
            (local.set $iovectors_offset 
              (i32.add (local.get $iovectors_offset) (i32.const 8))
            )
    
            ;; argument handled
            (local.set $handled (i32.const 1))
          )
        )

        ;; Increment counter
        (local.set $i (i32.add (local.get $i) (i32.const 1)))

        ;; Check if we're done processing arguments
        (br_if $process_arg
          (i32.ge_u (local.get $i) (local.get $len))
        )

        ;; after each argument, but the last, we put in a space
        (i32.store (local.get $iovectors_offset) (i32.const {{ data(str=" ") }}))
        (i32.store 
          (i32.add (local.get $iovectors_offset) (i32.const 4))
          (i32.const {{ data_length() }})
        )
 
        (local.set $iovectors_offset 
          (i32.add (local.get $iovectors_offset) (i32.const 8))
        )
 
        (br $process_args)
      ) 
    )

    ;; put newline at the end
    (i32.store (local.get $iovectors_offset) (i32.const {{ data(str="\n") }}))
    (i32.store 
      (i32.add (local.get $iovectors_offset) (i32.const 4))
      (i32.const {{ data_length() }})
    )
    
    ;; Call write with all accumulated iovectors
    (call $write 
      (i32.const 1)  ;; stdout
      (global.get $free_memory_offset)  ;; iovectors start
      (i32.mul (local.get $len) (i32.const 2))  ;; number of iovectors
      (i32.const 50)  ;; where to write result
    )
    drop
  )

  (func $add-pollable (param $pollable (ref $Pollable)) (result i32)
    (local $len i32)
    (local $i i32)
    (local $old_pollables (ref $PollablesArray))

    (local.set $len (array.len (global.get $pollables)))
    (local.set $i (i32.const 0))

    (block $break (loop $find
      (br_if $break (i32.ge_u (local.get $i) (local.get $len)))
      
      (array.get $PollablesArray (global.get $pollables) (local.get $i))
      (if (ref.test nullref)
        (then
          ;; we found a null value, let's insert here
          (array.set $PollablesArray
            (global.get $pollables)
            (local.get $i)
            (local.get $pollable))

          (struct.get $Pollable $id (local.get $pollable))
          (return)
        ))
      
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $find)
    ))
  
    ;; we haven't found any free space, let's double the array and try again 
    (local.set $old_pollables (global.get $pollables))
    (global.set $pollables
      (array.new $PollablesArray (ref.null $Pollable) (i32.mul (local.get $len) (i32.const 2))))

    (array.copy
      $PollablesArray            ;; dest type
      $PollablesArray            ;; source type
      (global.get $pollables)    ;; dest array
      (i32.const 0)              ;; dest offset
      (local.get $old_pollables) ;; source array
      (i32.const 0)              ;; source data offest
      (local.get $len)           ;; source data length
    )
 
    (call $add-pollable (local.get $pollable))
  )

  ;; (func $pollables-empty (result i32)
  ;;   (local $len i32)
  ;;   (local $i i32)

  ;;   (local.set $len (array.len (global.get $pollables)))
  ;;   (local.set $i (i32.const 0))

  ;;   (block $break (loop $find
  ;;     (br_if $break (i32.ge_s (local.get $i) (local.get $len)))
  ;;     
  ;;     (array.get $PollablesArray (global.get $pollables) (local.get $i))
  ;;     (if (i32.eqz (ref.test nullref))
  ;;       (then
  ;;         ;; non null value, return false
  ;;         (return (i32.const 0))
  ;;       ))
  ;;     
  ;;     (local.set $i (i32.add (local.get $i) (i32.const 1)))
  ;;     (br $find)
  ;;   ))

  ;;   (return (i32.const 1))
  ;; )

  (func $find-pollable (param $id i32) (result i32)
    (local $len i32)
    (local $i i32)
    (local $current (ref null $Pollable))

    (local.set $len (array.len (global.get $pollables)))
    (local.set $i (i32.const 0))

    (block $break (loop $find
      (br_if $break (i32.ge_u (local.get $i) (local.get $len)))
      
      (array.get $PollablesArray (global.get $pollables) (local.get $i))
      (local.set $current)
      (if (i32.eqz (ref.test nullref (local.get $current)))
        (then
          (if (i32.eq
            (local.get $id)
            (struct.get $Pollable $id (local.get $current)))
              (then
            (return (local.get $i))))))
      
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $find)
    ))

    (return (i32.const -1))
  )

  (func $execute-pollables (param $offset i32)
    (local $len i32)
    (local $index i32)
    (local $i i32)
    (local $func anyref)

    (local.set $len (i32.load (local.get $offset)))
    (local.set $offset (i32.add (local.get $offset) (i32.const 4)))
    (local.set $i (i32.const 0))

    (block $break (loop $find
      (br_if $break (i32.ge_u (local.get $i) (local.get $len)))

      (i32.load (local.get $offset))
      (call $find-pollable)
      (local.set $index)

      (if (i32.ne
            (local.get $index)
            (i32.const -1))
        (then
          (array.get $PollablesArray (global.get $pollables) (local.get $index))
          (struct.get $Pollable $func)
          (local.tee $func)
          (if (ref.test (ref $Function))
            (then
              (local.get $func) ;; function to call
              (ref.null any) ;; this, TODO: we need to handle this differently for arrow functions or binding
              (array.new $JSArgs (ref.null any) (i32.const 0))
              (call $call_function)
              (drop)
              (return)
            )
          )
          ;; TODO: handle async variant
          (throw $JSException (ref.i31 (i32.const 20002)))
      ))

      (local.set $offset (i32.add (local.get $offset) (i32.const 4)))

      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $find)
    ))
  )

  (func $clear-pollables (param $offset i32)
    (local $len i32)
    (local $index i32)
    (local $i i32)

    (local.set $len (i32.load (local.get $offset)))
    (local.set $offset (i32.add (local.get $offset) (i32.const 4)))
    (local.set $i (i32.const 0))

    (block $break (loop $find
      (br_if $break (i32.ge_u (local.get $i) (local.get $len)))

      (i32.load (local.get $offset))
      (call $find-pollable)
      (local.set $index)

      (if (i32.ne
            (local.get $index)
            (i32.const -1))
        (then
          (array.set $PollablesArray (global.get $pollables) (local.get $index) (ref.null $Pollable)))
      )

      (local.set $offset (i32.add (local.get $offset) (i32.const 4)))

      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $find)
    ))
  )

  (func $store-pollables (param $offset i32) (result i32)
    (local $len i32)
    (local $i i32)
    (local $current (ref null $Pollable))
    (local $stored_length i32)

    (local.set $len (array.len (global.get $pollables)))
    (local.set $i (i32.const 0))
    (local.set $stored_length (i32.const 0))

    (block $break (loop $find
      (br_if $break (i32.ge_u (local.get $i) (local.get $len)))
      
      (array.get $PollablesArray (global.get $pollables) (local.get $i))
      (local.set $current)
      (if (i32.eqz (ref.test nullref (local.get $current)))
        (then
          (i32.store (local.get $offset) (struct.get $Pollable $id (local.get $current)))
          (local.set $offset (i32.add (local.get $offset) (i32.const 4)))
          (local.set $stored_length (i32.add (local.get $stored_length) (i32.const 1)))
        )
      )
      
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $find)
    ))

    (local.get $stored_length)
  )

  (func $set-timeout (param $func anyref) (param $duration_arg anyref) (result (ref $Number))
    (local $duration i64)

    (if (ref.test (ref $Function) (local.get $func))
      (then

        (if (ref.test (ref $Number) (local.get $duration_arg))
          (then
            (ref.cast (ref $Number) (local.get $duration_arg))
            (struct.get $Number $value)
            (i64.trunc_f64_s)
            (i64.const 1000000) ;; javascript uses milliseconds, in here we have nanoseconds
            (i64.mul)
            (local.set $duration)
            ;; TODO: handle strings by converting to $Number
          )
          (else
            (local.set $duration (i64.const 0))
          ))

        (call $subscribe-duration (local.get $duration))
        (local.get $func)
        (call $new-pollable)
        (call $add-pollable)
        (f64.convert_i32_s)
        (call $new_number)
        ;; return the pollable id
        (return)
      )
    )

    ;; for now I'm just throwing numbers, so I can roughly find out what happened
    ;; but eventually this should throw actual JS errors
    (throw $JSException (ref.i31 (i32.const 20001)))
  )

  {{init_code}}

  ;; This is not how the run loop will run in the future. `poll-many`
  ;; is supposed to wait for the next pollable to resolve, thus blocking
  ;; the execution. In order to do that on the host there has to be a way
  ;; to call async functions. The problem is that passing an async function
  ;; as an WebAssembly import doesn't await it. There are ways around it,
  ;; for example to use asyncify, but `wasm-opt` with asyncify crashes
  ;; when trying to optimize the project (I'm guessing due to exception handling
  ;; or GC proposals). So instead of running in a loop (like the commented code would suggest)
  ;; we're calling poll-many and the poll-many polyfill will call main_loop again
  ;;
  ;; This should be fixed once th code can be executed on a runtime with proper
  ;; WASI preview2 support
  (func $main_loop
    (local $length i32)
    (local $offset i32)
    (;(block $break (loop $main_loop;)
      (local.set $offset (global.get $free_memory_offset))
      ;; we don't want the polables memory overwritten, so let's shift the offset by the length of pollables
      (global.set $free_memory_offset 
        (i32.add
          (local.get $offset)
          (i32.add
            (i32.mul
              (i32.load (local.get $offset))
              (i32.const 4))
            (i32.const 4)))) ;; we have N 32bit numbers + length

      (call $execute-pollables (local.get $offset))
      (call $clear-pollables (local.get $offset))

      ;; bring back the original free_memory_offset, so we don't waste memory
      (global.set $free_memory_offset (local.get $offset))

      (call $store-pollables (global.get $free_memory_offset))
      (local.set $length)

      (call $poll-many (global.get $free_memory_offset) (local.get $length) (global.get $free_memory_offset))

      ;;(call $clear-pollables (global.get $free_memory_offset))
    (;  (br_if $break (call $pollables-empty));)
    (;  (br $main_loop);)
    (;));)
  )

  (elem declare func $Promise-constructor)
  (elem declare func $Promise-then)
  (elem declare func $Promise-catch)
  (elem declare func $Promise-reject)
  (elem declare func $Promise-resolve)
  (elem declare func $Promise-finally)

  (func $install-globals
    (local $scope (ref $Scope))
    (local $promise-constructor (ref $Function))

    (local.tee $scope (call $new_scope (ref.null $Scope)))
    (global.set $scope)

    (global.set $promise-prototype (call $create-promise-prototype))

    ;; Promise
    (call $new_function (local.get $scope) (ref.func $Promise-constructor) (ref.null any))
    (local.set $promise-constructor)
    (call $set_variable (local.get $scope) (i32.const {{ data(str="Promise") }}) (local.get $promise-constructor))
  )

  (func $outer_init (result i32)
    (local $error anyref)
    (local $temp_arg anyref)
    (local $length i32)
    try
      (call $install-globals)
      (call $init)

      (call $store-pollables (global.get $free_memory_offset))
      (local.set $length)

      ;; if there are any pollables, this will call $main_loop, otherwise it will exit
      (call $poll-many (global.get $free_memory_offset) (local.get $length) (global.get $free_memory_offset))

      (return (i32.const 0))
    catch $JSException
      (local.set $error)

      (call $new_static_string (i32.const {{ data(str="error encountered") }}) (i32.const {{ data_length() }}))
      (local.get $error)
      (call $create-arguments-2)
      (call $log)
      (return (i32.const 1))
    end

    (i32.const 0)
  )

  (func $start
    call $outer_init
    call $proc_exit
  )

  (export "wasi:cli/run@0.2.1#run" (func $outer_init))
  (export "_start" (func $start))
  (export "main_loop" (func $main_loop))
)
