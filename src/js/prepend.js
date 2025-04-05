// Add toString method
Error.prototype.toString = function () {
  return this.name + ": " + this.message;
};

(function () {
  const iteratorFunction = function() {
    let i = 0;
    let arr = this;

    const iterator = {
      next() {
        let result;
        if (i < arr.length) {
          result = { value: arr[i], done: false };
          i += 1;
          return result;
        } else if (i == arr.length) {
          return { done: true };
        }

        result = { value: arr[i], done: true };
        i += 1;
        return result;
      },
    };

    return iterator;
  };

  Object.defineProperty(Array.prototype, Symbol.iterator, {
    value: iteratorFunction
  });

  Object.defineProperty(String.prototype, Symbol.iterator, {
    value: iteratorFunction
  });

  const wrapArray = function(maybeArray) {
    if (Array.isArray(maybeArray)) {
      return maybeArray;
    } else {
      return [maybeArray];
    }
  };

  const toObject = JAWSM.ToObject;
  // TODO: in core-js they split string, but I'm not sure why would that be needed
  const IndexedObject = function(arg) { return arg; };
  const lengthOfArrayLike = JAWSM.LengthOfArrayLike;
  const bind = function(callback, that) { return callback.bind(that); };
  // TODO: this will differ once we implement more array types
  const arraySpeciesCreate = function(that, length) { return Array(length); };
  const push = function(target, value) {
    target.push(value);
  };
  const doesNotExceedSafeInteger = JAWSM.DoesNotExceedSafeInteger;
  const setArrayLength = function (O, length) {
    if (Array.isArray(O) && !Object.getOwnPropertyDescriptor(O, 'length').writable) {
      throw new TypeError('Cannot set read only .length');
    }
    O.length = length;
    return length;
  };
  const isConcatSpreadable = function (O) {
    if (!isObject(O)) return false;
    var spreadable = O[Symbol.isConcatSpreadable];
    return spreadable !== undefined ? !!spreadable : Array.isArray(O);
  };
  const isCallable = function(it) {
    return typeof it == 'function';
  };
  const isObject = function(it) {
    return typeof it == 'object' ? it !== null : isCallable(it);
  };
  const createProperty = function(object, key, value) {
    object[key] = value;
  };
  const toAbsoluteIndex = JAWSM.ToAbsoluteIndex;
  const deletePropertyOrThrow = function(O, P) {
    if (!delete O[P]) throw new TypeError('Cannot delete property ' + tryToString(P) + ' of ' + tryToString(O));
  };
  const tryToString = function(argument) {
    try {
      return String(argument);
    } catch (error) {
      return 'Object';
    }
  };
  const min = function(arg1, arg2) {
    return arg1 <= arg2 ? arg1 : arg2;
  };
  const aCallable = function (argument) {
    if (isCallable(argument)) return argument;
    throw new $TypeError(tryToString(argument) + ' is not a function');
  };
  const requireObjectCoercible = function(it) {
    if (isNullOrUndefined(it)) throw new $TypeError("Can't call method on " + it);
    return it;
  };
  const isNullOrUndefined = function(it) { return it === null || it === undefined; };
  const toIndexedObject = function(it) { return IndexedObject(requireObjectCoercible(it)); };

  // Copyright (c) 2014-2025 Denis Pushkarev
  // MIT license
  // From https://github.com/zloirock/core-js
  var createIterationMethod = function (TYPE) {
    var IS_MAP = TYPE === 1;
    var IS_FILTER = TYPE === 2;
    var IS_SOME = TYPE === 3;
    var IS_EVERY = TYPE === 4;
    var IS_FIND_INDEX = TYPE === 6;
    var IS_FILTER_REJECT = TYPE === 7;
    var NO_HOLES = TYPE === 5 || IS_FIND_INDEX;
    return function ($this, callbackfn, that, specificCreate) {
      var O = toObject($this);
      var self = IndexedObject(O);
      var length = lengthOfArrayLike(self);
      var boundFunction = bind(callbackfn, that);
      var index = 0;
      var create = specificCreate || arraySpeciesCreate;
      var target = IS_MAP ? create($this, length) : IS_FILTER || IS_FILTER_REJECT ? create($this, 0) : undefined;
      var value, result;
      for (;length > index; index++) {
        if (NO_HOLES || index in self) {
          value = self[index];
          result = boundFunction(value, index, O);
          if (TYPE) {
            if (IS_MAP) target[index] = result; // map
            else if (result) switch (TYPE) {
              case 3: return true;              // some
              case 5: return value;             // find
              case 6: return index;             // findIndex
              case 2: push(target, value);      // filter
            } else switch (TYPE) {
              case 4: return false;             // every
              case 7: push(target, value);      // filterReject
            }
          }
        }
      }
      return IS_FIND_INDEX ? -1 : IS_SOME || IS_EVERY ? IS_EVERY : target;
    };
  };

  const createIterationFromLastMethod = function (TYPE) {
    var IS_FIND_LAST_INDEX = TYPE === 1;
    return function ($this, callbackfn, that) {
      var O = toObject($this);
      var self = IndexedObject(O);
      var index = lengthOfArrayLike(self);
      var boundFunction = bind(callbackfn, that);
      var value, result;
      while (index-- > 0) {
        value = self[index];
        result = boundFunction(value, index, O);
        if (result) switch (TYPE) {
          case 0: return value; // findLast
          case 1: return index; // findLastIndex
        }
      }
      return IS_FIND_LAST_INDEX ? -1 : undefined;
    };
  };

  var createIncludesMethod = function (IS_INCLUDES) {
    return function ($this, el, fromIndex) {
      var O = toIndexedObject($this);
      var length = lengthOfArrayLike(O);
      if (length === 0) return !IS_INCLUDES && -1;
      var index = toAbsoluteIndex(fromIndex, length);
      var value;
      // Array#includes uses SameValueZero equality algorithm
      // eslint-disable-next-line no-self-compare -- NaN check
      if (IS_INCLUDES && el !== el) while (length > index) {
        value = O[index++];
        // eslint-disable-next-line no-self-compare -- NaN check
        if (value !== value) return true;
      // Array#indexOf ignores holes, Array#includes - not
      } else for (;length > index; index++) {
        if ((IS_INCLUDES || index in O) && O[index] === el) return IS_INCLUDES || index || 0;
      } return !IS_INCLUDES && -1;
    };
  };

  // `FlattenIntoArray` abstract operation
  // https://tc39.github.io/proposal-flatMap/#sec-FlattenIntoArray
  var flattenIntoArray = function (target, original, source, sourceLen, start, depth, mapper, thisArg) {
    var targetIndex = start;
    var sourceIndex = 0;
    var mapFn = mapper ? bind(mapper, thisArg) : false;
    var element, elementLen;

    while (sourceIndex < sourceLen) {
      if (sourceIndex in source) {
        element = mapFn ? mapFn(source[sourceIndex], sourceIndex, original) : source[sourceIndex];

        if (depth > 0 && Array.isArray(element)) {
          elementLen = lengthOfArrayLike(element);
          targetIndex = flattenIntoArray(target, original, element, elementLen, targetIndex, depth - 1) - 1;
        } else {
          doesNotExceedSafeInteger(targetIndex + 1);
          target[targetIndex] = element;
        }

        targetIndex++;
      }
      sourceIndex++;
    }
    return targetIndex;
  };

  let includes = createIncludesMethod(true);
  Object.defineProperty(Array.prototype, "includes", {
    value: function(el /* , fromIndex = 0 */) {
      return includes(this, el, arguments.length > 1 ? arguments[1] : undefined);
    }
  });

  let indexOf = createIncludesMethod(false);
  Object.defineProperty(Array.prototype, "indexOf", {
    value: function(searchElement /* , fromIndex = 0 */) {
      var fromIndex = arguments.length > 1 ? arguments[1] : undefined;
      return indexOf(this, searchElement, fromIndex);
    }
  });

  let findLast = createIterationFromLastMethod(0);
  Object.defineProperty(Array.prototype, "findLast", {
    value: function(callbackfn /* , that = undefined */) {
      return findLast(this, callbackfn, arguments.length > 1 ? arguments[1] : undefined);
    }
  });

  let findLastIndex = createIterationFromLastMethod(1);
  Object.defineProperty(Array.prototype, "findLastIndex", {
    value: function (callbackfn /* , that = undefined */) {
      return findLastIndex(this, callbackfn, arguments.length > 1 ? arguments[1] : undefined);
    }
  });

  let forEach = createIterationMethod(0);
  Object.defineProperty(Array.prototype, "forEach", {
    value: function(callbackFn, thisArg) {
      return forEach(this, callbackFn, thisArg);
    }
  });

  let map = createIterationMethod(1);
  Object.defineProperty(Array.prototype, "map", {
    value: function(callbackFn, thisArg) {
      return map(this, callbackFn, thisArg);
    }
  });

  let filter = createIterationMethod(2);
  Object.defineProperty(Array.prototype, "filter", {
    value: function(callbackFn, thisArg) {
      return filter(this, callbackFn, thisArg);
    }
  });

  let some = createIterationMethod(3);
  Object.defineProperty(Array.prototype, "some", {
    value: function(callbackFn, thisArg) {
      return some(this, callbackFn, thisArg);
    }
  });

  let every = createIterationMethod(4);
  Object.defineProperty(Array.prototype, "every", {
    value: function(callbackFn, thisArg) {
      return every(this, callbackFn, thisArg);
    }
  });

  let find = createIterationMethod(5);
  Object.defineProperty(Array.prototype, "find", {
    value: function(callbackFn, thisArg) {
      return find(this, callbackFn, thisArg);
    }
  });

  let findIndex = createIterationMethod(6);
  Object.defineProperty(Array.prototype, "findIndex", {
    value: function(callbackFn, thisArg) {
      return findIndex(this, callbackFn, thisArg);
    }
  });

  let filterReject = createIterationMethod(7);
  Object.defineProperty(Array.prototype, "filterReject", {
    value: function(callbackFn, thisArg) {
      return filterReject(this, callbackFn, thisArg);
    }
  });

  Object.defineProperty(Array.prototype, "push", {
    value: function() {
      var O = toObject(this);
      var len = lengthOfArrayLike(O);
      var argCount = arguments.length;
      doesNotExceedSafeInteger(len + argCount);
      setArrayLength(O, len + argCount);
      for (var i = 0; i < argCount; i++) {
        O[len] = arguments[i];
        len++;
      }
      return len;
    }
  });

  Object.defineProperty(Array.prototype, "pop", {
    value: function() {
      var O = this;
      var len = lengthOfArrayLike(O);
      if (len == 0) {
        return undefined;
      } else {
        const value = O[len - 1];
        setArrayLength(O, len - 1);
        return value;
      }
    }
  });

  Object.defineProperty(Array.prototype, "concat", {
    value: function(arg) {
      var O = toObject(this);
      var A = arraySpeciesCreate(O, 0);
      var n = 0;
      var i, k, length, len, E;
      for (i = -1, length = arguments.length; i < length; i++) {
        E = i === -1 ? O : arguments[i];
        if (isConcatSpreadable(E)) {
          len = lengthOfArrayLike(E);
          doesNotExceedSafeInteger(n + len);
          for (k = 0; k < len; k++, n++) if (k in E) createProperty(A, n, E[k]);
        } else {
          doesNotExceedSafeInteger(n + 1);
          // TODO: using n++ as an argument results in a WASM error
          createProperty(A, n++, E);
        }
      }
      A.length = n;
      return A;
    }
  });

  Object.defineProperty(Array.prototype, "copyWithin", {
    value: function(target /* = 0 */, start /* = 0, end = @length */) {
      var O = toObject(this);
      var len = lengthOfArrayLike(O);
      var to = toAbsoluteIndex(target, len);
      var from = toAbsoluteIndex(start, len);
      var end = arguments.length > 2 ? arguments[2] : undefined;
      var count = min((end === undefined ? len : toAbsoluteIndex(end, len)) - from, len - to);
      var inc = 1;
      if (from < to && to < from + count) {
        inc = -1;
        from += count - 1;
        to += count - 1;
      }
      while (count-- > 0) {
        if (from in O) O[to] = O[from];
        else deletePropertyOrThrow(O, to);
        to += inc;
        from += inc;
      }
      return O;
    }
  });

  Object.defineProperty(Array.prototype, "entries", {
    value: function() {
      return this[Symbol.iterator];
    }
  });

  Object.defineProperty(Array.prototype, "fill", {
    value: function(value /* , start = 0, end = @length */) {
      var O = toObject(this);
      var length = lengthOfArrayLike(O);
      var argumentsLength = arguments.length;
      var index = toAbsoluteIndex(argumentsLength > 1 ? arguments[1] : undefined, length);
      var end = argumentsLength > 2 ? arguments[2] : undefined;
      var endPos = end === undefined ? length : toAbsoluteIndex(end, length);
      while (endPos > index) O[index++] = value;
      return O;
    }
  });

  // `Array.prototype.flat` method
  // https://tc39.es/ecma262/#sec-array.prototype.flat
  Object.defineProperty(Array.prototype, "flat", {
    value: function(/* depthArg = 1 */) {
      var depthArg = arguments.length ? arguments[0] : undefined;
      var O = toObject(this);
      var sourceLen = lengthOfArrayLike(O);
      var A = arraySpeciesCreate(O, 0);
      A.length = flattenIntoArray(A, O, O, sourceLen, 0, depthArg === undefined ? 1 : toIntegerOrInfinity(depthArg));
      return A;
    }
  });

  // `Array.prototype.flatMap` method
  // https://tc39.es/ecma262/#sec-array.prototype.flatmap
  Object.defineProperty(Array.prototype, "flatMap", {
    value: function flatMap(callbackfn /* , thisArg */) {
      var O = toObject(this);
      var sourceLen = lengthOfArrayLike(O);
      var A;
      aCallable(callbackfn);
      A = arraySpeciesCreate(O, 0);
      A.length = flattenIntoArray(A, O, O, sourceLen, 0, 1, callbackfn, arguments.length > 1 ? arguments[1] : undefined);
      return A;
    }
  });
})();
