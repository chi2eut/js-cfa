// Test 4: two independent taint sources converge at one variable
// Expect: c tainted with both "source-a" and "source-b"
let a = taintSource("source-a", 1);
let b = taintSource("source-b", 2);
let c = a + b;
