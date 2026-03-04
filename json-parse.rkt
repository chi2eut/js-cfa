#lang racket/base

(require json racket/file racket/port racket/match)

(provide run-parse)

(define (run-parse filename)
  (define file-contents
    (port->string (open-input-file filename) #:close? #t))
  (define res (string->jsexpr file-contents))
  (parse res))

(define (parse h)
  (match (hash-ref h 'type)
    ["Program"
     `(program ,(map parse (hash-ref h 'body)))]
    ["FunctionDeclaration"
     `(define ,(list* (parse (hash-ref h 'id))
                      (map parse (hash-ref h 'params)))
        ,(parse (hash-ref h 'body)))]
    ["VariableDeclaration"
     (match (hash-ref h 'kind)
       [(or "let" "const")
        `(let ,(map parse (hash-ref h 'declarations)))])]
    ["VariableDeclarator"
     `(,(parse (hash-ref h 'id)) ,(parse (hash-ref h 'init)))]
    ["ExpressionStatement"
     (parse (hash-ref h 'expression))]
    ["MemberExpression"
     `(memberE ,(parse (hash-ref h 'object))
               ,(parse (hash-ref h 'property)))]
    ["BlockStatement"
     `(block ,(map parse (hash-ref h 'body)))]
    ["SequenceExpression"
     `(seq ,(map parse (hash-ref h 'expressions)))]
    ["AssignmentExpression"
     `(assgn ,(parse (hash-ref h 'left))
             ,(parse (hash-ref h 'right)))]
    [(or "FunctionExpression" "ArrowFunctionExpression")
     `(λ ,(map parse (hash-ref h 'params)) ,(parse (hash-ref h 'body)))]
    ["IfStatement"
     `(if ,(parse (hash-ref h 'test))
          ,(parse (hash-ref h 'consequent))
          ,(parse (hash-ref h 'alternate)))]
    ["BinaryExpression"
     `(bin ,(string->symbol (hash-ref h 'operator))
           ,(parse (hash-ref h 'left))
           ,(parse (hash-ref h 'right)))]
    ["UnaryExpression"
     `(unary ,(string->symbol (hash-ref h 'operator))
             ,(parse (hash-ref h 'argument)))]
    ["LogicalExpression"
     `(logical ,(string->symbol (hash-ref h 'operator))
               ,(parse (hash-ref h 'left))
               ,(parse (hash-ref h 'right)))]
    ["ReturnStatement"
     `(return ,(parse (hash-ref h 'argument)))]
    ["CallExpression"
     `(app ,(parse (hash-ref h 'callee))
           ,(map parse (hash-ref h 'arguments)))]
    ["ForStatement"
     `(forlp ,(parse (hash-ref h 'init))
             ,(parse (hash-ref h 'test))
             ,(parse (hash-ref h 'update))
             ,(parse (hash-ref h 'body)))]
    ["WhileStatement"
     `(whlp ,(parse (hash-ref h 'test))
            ,(parse (hash-ref h 'body)))]
    ["DoWhileStatement"
     `(dowhlp ,(parse (hash-ref h 'body))
              ,(parse (hash-ref h 'test)))]
    ["UpdateExpression"
     `(update ,(string->symbol (hash-ref h 'operator))
              ,(parse (hash-ref h 'argument)))]
    ["Identifier"
     (string->symbol (hash-ref h 'name))]
    ["Literal"
     (hash-ref h 'value)]
    ["ObjectExpression"
     `(obj ,(map parse (hash-ref h 'properties)))]
    ["Property"
     `(,(parse (hash-ref h 'key)) : ,(parse (hash-ref h 'value)))]
    ["NewExpression"
     `(constructor ,(parse (hash-ref h 'callee))
                   ,(map parse (hash-ref h 'arguments)))]
    ["ArrayExpression"
     `(arr ,(map parse (hash-ref h 'elements)))]
    ["TryStatement"
     `(try/catch ,(parse (hash-ref h 'block))
                 ,(parse (hash-ref h 'handler)))]
    ["CatchClause"
     `(λ (,(parse (hash-ref h 'param))) ,(parse (hash-ref h 'body)))]
    ["ThrowStatement"
     `(throw ,(parse (hash-ref h 'argument)))]
    [_
     "unknown"]))

#;(run-parse "programs/let.txt")
