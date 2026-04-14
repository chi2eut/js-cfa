// Test 6: taint flows through a function that returns its argument unchanged
// Expect: result tainted with "user-input"
let identity = (x) => x;
let secret = taintSource("user-input", 99);
let result = identity(secret);
