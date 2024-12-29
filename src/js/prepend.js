// Add toString method
Error.prototype.toString = function () {
  return this.name + ": " + this.message;
};

Object.defineProperty(Array.prototype, Symbol.iterator, {
  value: function() {
    let i = 0;
    let arr = this;

    const iterator = {
      next() {
        let result;
        if (i < arr.length - 1) {
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
  }
});
