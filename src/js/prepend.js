// Add toString method
Error.prototype.toString = function () {
  return this.name + ": " + this.message;
};

// // Test it
// try {
//   throw new ReferenceError("test");
// } catch (e) {
//   // console.log(e instanceof Error); // true
//   // console.log(e instanceof ReferenceError); // true
//   console.log(e.toString()); // "ReferenceError: test"
//   // console.log(e.stack); // Shows stack trace
// }
