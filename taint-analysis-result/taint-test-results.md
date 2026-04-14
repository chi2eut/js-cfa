# Taint Analysis Test Results

This document summarizes the results of the taint analysis tests conducted using `mcfa.rkt`.

**Command Format:**
```bash
acorn --ecma2025 <program> > /tmp/ast.json && racket mcfa.rkt /tmp/ast.json 0 <sinks...>
```

---

## Summary of Test Cases

| Test | Name | Description | Status |
| :--- | :--- | :--- | :---: |
| 1 | `taint-simple.js` | Direct taint propagation through variable assignment | ✅ Pass |
| 2 | `taint-arith.js` | Taint propagates through chained arithmetic operations | ✅ Pass |
| 3 | `taint-no-flow.js` | Untainted variable should not acquire taint labels | ✅ Pass |
| 4 | `taint-multi-source.js` | Two independent taint sources converge at a single variable | ✅ Pass |
| 5 | `taint-obj.js` | Taint through object property write and read | ✅ Pass |
| 6 | `taint-fn-passthrough.js` | Taint flows through a function (identity) | ✅ Pass |
| 7 | `taint-fn-transform.js` | Taint survives arithmetic transformation inside a function | ✅ Pass |
| 8 | `taint-fn-two-args.js` | Taint separation between multiple function arguments | ✅ Pass |

---

## Detailed Results

### 1. `taint-simple.js`
> **Description:** Direct taint propagation through variable assignment.

**Program:**
```javascript
let x = taintSource("user-input", 42);
let y = x;
```

**Analysis Results:**
| Sink | Taint Set |
| :--- | :--- |
| `x` | `(set "user-input")` |
| `y` | `(set "user-input")` |

**Notes:** Taint flows from `x` into `y` via direct assignment.

---

### 2. `taint-arith.js`
> **Description:** Taint propagates through chained arithmetic operations.

**Program:**
```javascript
let x = taintSource("user-input", 10);
let y = x + 5;
let z = y * 2;
```

**Analysis Results:**
| Sink | Taint Set |
| :--- | :--- |
| `x` | `(set "user-input")` |
| `y` | `(set "user-input")` |
| `z` | `(set "user-input")` |

**Notes:** Taint label carries through both `+` and `*` without being lost.

---

### 3. `taint-no-flow.js`
> **Description:** Untainted variable should not acquire taint labels (no false positives).

**Program:**
```javascript
let x = taintSource("user-input", 42);
let y = 100;
```

**Analysis Results:**
| Sink | Taint Set |
| :--- | :--- |
| `x` | `(set "user-input")` |
| `y` | `(set)` |

**Notes:** `y` is a plain literal with no connection to `x`. Empty taint set confirms no spurious flow.

---

### 4. `taint-multi-source.js`
> **Description:** Two independent taint sources converge at a single variable.

**Program:**
```javascript
let a = taintSource("source-a", 1);
let b = taintSource("source-b", 2);
let c = a + b;
```

**Analysis Results:**
| Sink | Taint Set |
| :--- | :--- |
| `a` | `(set "source-a")` |
| `b` | `(set "source-b")` |
| `c` | `(set "source-a" "source-b")` |

**Notes:** Both labels are unioned into `c`, preserving full provenance.

---

### 5. `taint-obj.js`
> **Description:** Taint through object property write and read.

**Program:**
```javascript
let obj = { val: taintSource("db-result", 42) };
let x = obj.val;
```

**Analysis Results:**
| Sink | Taint Set |
| :--- | :--- |
| `obj` | `(set)` |
| `x` | `(set "db-result")` |

**Notes:** 
* Taint lives on the property's store address, not on the object struct itself.
* Reading `obj.val` retrieves the taint from `tau` and flows it into `x`.
* `obj` shows an empty taint set, confirming taint is tracked per-address.

---

### 6. `taint-fn-passthrough.js`
> **Description:** Taint flows through a function that returns its argument unchanged.

**Program:**
```javascript
let identity = (x) => x;
let secret = taintSource("user-input", 99);
let result = identity(secret);
```

**Analysis Results:**
| Sink | Taint Set |
| :--- | :--- |
| `secret` | `(set "user-input")` |
| `result` | `(set "user-input")` |

**Notes:** 
* The taint label on `secret` is stored at the parameter address inside the call, then read back when `x` is returned, and flows into `result` via `sto-extend`.
* Also required fixing a pre-existing bug: `extend` was passing `args` (the wrapped argument list) instead of `(car args)` (the actual argument expressions), causing a match failure on e.g. `'(42)` as an expression.

---

### 7. `taint-fn-transform.js`
> **Description:** Taint survives an arithmetic transformation inside a function.

**Program:**
```javascript
let double = (n) => n * 2;
let raw = taintSource("sensor-data", 5);
let out = double(raw);
```

**Analysis Results:**
| Sink | Taint Set |
| :--- | :--- |
| `raw` | `(set "sensor-data")` |
| `out` | `(set "sensor-data")` |

**Notes:** Taint on the argument `n` flows through `n * 2` via `proj/ev`, so the return value of `double` carries the label and is stored into `out`.

---

### 8. `taint-fn-two-args.js`
> **Description:** One tainted argument and one clean argument; only the taint label from the tainted argument should appear on the result.

**Program:**
```javascript
let add = (a, b) => a + b;
let tainted = taintSource("api-key", 10);
let clean = 5;
let combined = add(tainted, clean);
```

**Analysis Results:**
| Sink | Taint Set |
| :--- | :--- |
| `tainted` | `(set "api-key")` |
| `clean` | `(set)` |
| `combined` | `(set "api-key")` |

**Notes:** `clean` has no taint. The addition `a + b` unions taint from both parameters; since only `a`'s address carries `"api-key"`, `combined` ends up with just that label.
