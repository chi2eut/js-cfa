// Test 8: taint from one argument but not the other; result carries only that label
// Expect: clean is untainted, tainted is tainted, combined carries only "api-key"
let add = (a, b) => a + b;
let tainted = taintSource("api-key", 10);
let clean = 5;
let combined = add(tainted, clean);
