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
  const IndexedObject = function(arg) { return arg; };
  const lengthOfArrayLike = JAWSM.LengthOfArrayLike;
  const bind = function(callback, that) { return callback.bind(that); };
  // TODO: this will differ once we implement more array types
  const arraySpeciesCreate = function(that, length) { return Array(length); };
  const push = function(target, value) {
    target.push(value);
  }
  const doesNotExceedSafeInteger = JAWSM.DoesNotExceedSafeInteger;
  const setArrayLength = function (O, length) {
    if (Array.isArray(O) && !Object.getOwnPropertyDescriptor(O, 'length').writable) {
      throw new TypeError('Cannot set read only .length');
    }
    O.length = length;
    return length;
  }

  // Copyright (c) 2014-2025 Denis Pushkarev
  // MIT license
  // From https://github.com/zloirock/core-js/blob/master/packages/core-js/internals/array-iteration.js
  var createMethod = function (TYPE) {
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

  let forEach = createMethod(0);
  Object.defineProperty(Array.prototype, "forEach", {
    value: function(callbackFn, thisArg) {
      return forEach(this, callbackFn, thisArg);
    }
  });

  let map = createMethod(1);
  Object.defineProperty(Array.prototype, "map", {
    value: function(callbackFn, thisArg) {
      return map(this, callbackFn, thisArg);
    }
  });

  let filter = createMethod(2);
  Object.defineProperty(Array.prototype, "filter", {
    value: function(callbackFn, thisArg) {
      return filter(this, callbackFn, thisArg);
    }
  });

  let some = createMethod(3);
  Object.defineProperty(Array.prototype, "some", {
    value: function(callbackFn, thisArg) {
      return some(this, callbackFn, thisArg);
    }
  });

  let every = createMethod(4);
  Object.defineProperty(Array.prototype, "every", {
    value: function(callbackFn, thisArg) {
      return every(this, callbackFn, thisArg);
    }
  });

  let find = createMethod(5);
  Object.defineProperty(Array.prototype, "find", {
    value: function(callbackFn, thisArg) {
      return find(this, callbackFn, thisArg);
    }
  });

  let findIndex = createMethod(6);
  Object.defineProperty(Array.prototype, "findIndex", {
    value: function(callbackFn, thisArg) {
      return findIndex(this, callbackFn, thisArg);
    }
  });

  let filterReject = createMethod(7);
  Object.defineProperty(Array.prototype, "filterReject", {
    value: function(callbackFn, thisArg) {
      return filterReject(this, callbackFn, thisArg);
    }
  });

  Object.defineProperty(Array.prototype, "push", {
    value: function push() {
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
    value: function pop() {
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
})();
