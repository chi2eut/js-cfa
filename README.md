# JavaScript m-CFA (Taint Analysis)

Make sure npm is installed.

Run by

```
acorn --ecma2025 <program> > /tmp/ast.json && racket mcfa.rkt /tmp/ast.json 0 <sinks...>
```
