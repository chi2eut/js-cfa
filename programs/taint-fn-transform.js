// Test 7: taint survives an arithmetic transformation inside a function
// Expect: out tainted with "sensor-data"
let double = (n) => n * 2;
let raw = taintSource("sensor-data", 5);
let out = double(raw);
