// Test 1: direct taint propagation through assignment
// Expect: x and y both tainted with "user-input"
let x = taintSource("user-input", 42);
let y = x;
