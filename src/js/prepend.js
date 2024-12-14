function Error(message) {
  // Handle calling without 'new'
  // if (!(this instanceof Error)) {
  //   return new Error(message);
  // }

  this.message = message;
  this.name = "Error";
}

// Set up prototype
Error.prototype = Object.create(Object.prototype);
Error.prototype.constructor = Error;
//
// Add toString method
Error.prototype.toString = function () {
  return this.name + ": " + this.message;
};

function ReferenceError(message) {
  Error.call(this, message);
  this.name = "ReferenceError";
}

ReferenceError.prototype = Object.create(Error.prototype);
ReferenceError.prototype.constructor = ReferenceError;

// // Test it
// try {
//   throw new ReferenceError("test");
// } catch (e) {
//   // console.log(e instanceof Error); // true
//   // console.log(e instanceof ReferenceError); // true
//   console.log(e.toString()); // "ReferenceError: test"
//   // console.log(e.stack); // Shows stack trace
// }
