// Test 3: untainted variable should not acquire taint labels
// Expect: x tainted with "user-input", y clean (empty taint set)
let x = taintSource("user-input", 42);
let y = 100;
