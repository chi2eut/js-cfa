// Test 5: taint through object property write and read
// Expect: x tainted with "db-result" after reading the tainted property
let obj = { val: taintSource("db-result", 42) };
let x = obj.val;
