// Test 2: taint propagates through arithmetic operations
// Expect: x tainted, y tainted (x + 5), z tainted (y * 2)
let x = taintSource("user-input", 10);
let y = x + 5;
let z = y * 2;
