#lang racket/base

(require racket/match racket/set racket/pretty "json-parse.rkt" racket/list)

(define verbose-mode (make-parameter #f))
(define show-sto (make-parameter #f))
(define current-m (make-parameter 0))

;---------------------------------------------------------------------------------------------------------
; Fixpoint Monad Stuff

(define ((unit . xs) σ κ) ((car κ) xs σ))
(define ((>>= c f) σ κ) (c σ (cons (λ (xs σ) ((apply f xs) σ κ)) (cdr κ))))
(define (>> m . ms) (foldl (λ (m M) (>>= M (λ _ m))) m ms))

(define ((∘ f g) s) (f (g s)))
(define (id s) s)

(define ((each . cs) σ κ) (foldl (λ (c s→s) (∘ s→s (c σ κ))) id cs))
(define (fail σ κ) ((cdr κ) (list bot) σ))

(define (choose xs)
  (if (set-empty? xs)
      fail
      (let ([x (set-first xs)]
            [xs (set-rest xs)])
        (each (unit x)
              (choose xs)))))

(define (for-eachm f xs)
  (match xs
    [(list)
     (unit (void))]
    [(cons x xs)
     (>> (f x)
         (for-eachm f xs))]))

(define (((cache k c) σ κ) s)
  (define key (append k (list σ)))
  (cond
    [(hash-ref s key #f)
     => (match-lambda
          [(cons xss×σs κs)
           (for/fold ([s (hash-set s key (cons xss×σs (cons κ κs)))])
                     ([xs×σ (in-set xss×σs)])
             (match-let ([(list xs σ) xs×σ])
               (((car κ) xs σ) s)))])]
    [else
     ((c σ
       (cons (λ (xs σ)
               (λ (s)
                 (match-let
                     ([(cons xss×σs κs) (hash-ref s key)])
                   (if (set-member? xss×σs (list xs σ))
                       s
                       (for/fold ([s (hash-set s key (cons (set-add xss×σs (list xs σ)) κs))])
                                 ([κ (in-list κs)])
                         (((car κ) xs σ) s))))))
             (cdr κ)))
       (hash-set s key (cons (set) (list κ))))]))

(define ((memo tag f) . xs)
  (cache (cons tag xs) (apply f xs)))
(define (run c)
  ((c (hash) (cons (λ (xs σ) id) (λ (xs σ) id))) (hash)))

;----------------------------------------------------------------------------------------------------------
; Utility - store, env, etc.

(define .... 'REPLACE-ME)
(define (push-back xs x) (append xs (list x)))

(define (inj v)
  (match v
    [(? number? n)
     `(,n ,(set) ⊥ ⊥ ,(set) ,(set) ⊥)]
    [(? boolean? b)
     `(⊥ ,(set b) ⊥ ⊥ ,(set) ,(set) ⊥)]
    [(? string? str)
     `(⊥ ,(set) ,str ⊥ ,(set) ,(set) ⊥)]
    [(? symbol? sym)
     `(⊥ ,(set) ⊥ ,sym ,(set) ,(set) ⊥)]
    [`(clos (λ ,xs ,e) ,ρ)
     `(⊥ ,(set) ⊥ ⊥ ,(set `(clos (λ ,xs ,e) ,ρ)) ,(set) ⊥)]
    [(list* obj ps)
     `(⊥ ,(set) ⊥ ⊥ ,(set) ,(set (list* obj ps)) ⊥)]
    [`null
     `(⊥ ,(set) ⊥ ⊥ ,(set) ,(set) ⊤)]))

(define bot `(⊥ ,(set) ⊥ ⊥ ,(set) ,(set) ⊥))

(define num-top `(⊤ ,(set) ⊥ ⊥ ,(set) ,(set) ⊥))
(define bool-top `(⊥ ,(set #t #f) ⊥ ⊥ ,(set) ,(set) ⊥))
(define str-top `(⊥ ,(set) ⊤ ⊥ ,(set) ,(set) ⊥))
(define sym-top `(⊥ ,(set) ⊥ ⊤ ,(set) ,(set) ⊥))
(define null-top `(⊥ ,(set) ⊥ ⊥ ,(set) ,(set) ⊤))

(define val?
  (match-lambda
    [`(,n ,b ,str ,sym ,clo ,obj ,nl) #t]
    [_ #f]))

(define (⊔ v₀ v₁)
  (match* (v₀ v₁)
    [(`(,n₀ ,b₀ ,str₀ ,sym₀ ,clo₀ ,obj₀ ,nl₀)
      `(,n₁ ,b₁ ,str₁ ,sym₁ ,clo₁ ,obj₁ ,nl₁))
     `(,(join n₀ n₁)
       ,(set-union b₀ b₁)
       ,(join str₀ str₁)
       ,(join sym₀ sym₁)
       ,(set-union clo₀ clo₁)
       ,(set-union obj₀ obj₁)
       ,(join nl₀ nl₁))]))

(define (join v₀ v₁)
  (match* (v₀ v₁)
    [('⊤ _) '⊤]
    [(_ '⊤) '⊤]
    [('⊥ v₁) v₁]
    [(v₀ '⊥) v₀]
    [(v₀ v₁) '⊤]))

(define ((sto-lookup a) σ κ)
  ((unit (car (hash-ref σ a)))
   σ κ))

(define ((sto-extend a v) σ κ)
  ((unit (void))
   (hash-update σ a
                (λ (vs)
                  (match* (v vs)
                    [('undefined _) `(undefined ,0)]
                    [((? val? v) `(undefined 0)) `(,v 1)]
                    [((? val? v) `(,v₁ ,c)) `(,(⊔ v v₁) ,(incr c))]))
                `(,bot ,0))
   κ))

(define incr
  (match-lambda
    [0 1]
    [1 'inf]
    ['inf 'inf]))

(define (env-lookup ρ x)
  (hash-ref ρ x))
(define (env-extend ρ x a)
  (hash-set ρ x a))

(define ((error/exception . xs) σ κ)
  ((cdr κ) xs σ))

(define ((inj-exception c f) σ κ)
  (c σ
     (cons (car κ)
           (λ (xs σ)
             ((apply f xs)
              σ κ)))))

(define (proj v ty)
  (match v
    [`(,n ,b ,str ,sym ,clo ,obj ,nl)
     (match ty
       ['NUM (unit n)]
       ['BOOL (choose b)]
       ['STR (unit str)]
       ['SYM (unit sym)]
       ['CLO (choose clo)]
       ['OBJ (choose obj)])]))

(define (proj/ev m ty f)
  (>>= m (λ (v) (>>= (proj v ty) f))))

(define (alloc x t) (list x t))

(define (obj-alloc key e t) (list key e t))

(define (tick app t)
  (if (< (length t) (current-m))
      (cons app t)
      (take (cons app t) (current-m))))

; Analysis data types: Ρ(Int × Boolean × String × Symbol × Closure × Object × Null) + undefined

;----------------------------------------------------------------------------------------------------------
; CFA stuff

(define interp/top
  (memo 'interp/top
        (λ (es ρ t)
          (>>= (env/top es ρ t)
               (λ (ρ)
                 (define itr
                   (match-lambda
                     [(cons e es)
                      (match e
                        [`(let ,(? list? bindings))
                         (define (itr′ bs)
                           (match bs
                             [(cons `(,x ,e) bs)
                              (define a (env-lookup ρ x))
                              (>>= (interp e ρ t)
                                   (λ (v) (>> (sto-extend a v)
                                              (itr′ bs))))]
                             [null
                              (unit (void))]))
                         (>> (itr′ bindings)
                             (itr es))]
                        [`(define ,(list* f args) ,e)
                         (define a (env-lookup f))
                         (>> (sto-extend a (inj `(clos `(λ ,args ,e) ,ρ)))
                             (itr es))]
                        [e
                         (>>= (interp e ρ t)
                              (match-lambda
                                [`(returnV ,v)
                                 (unit `(returnV ,v))]
                                [_
                                 (itr es)]))])]
                     [null
                      (unit (void))]))
                 (itr es))))))

(define (env/top es ρ t)
  (match es
    [(cons e es)
     (>>= (match e
            [`(let ,(? list? bindings))
             (define (itr bs ρ)
               (match bs
                 [(cons `(,x ,e) bs)
                  (define a (alloc x t))
                  (>> (sto-extend a 'undefined)
                      (itr bs (env-extend ρ x a)))]
                 [null
                  (unit ρ)]))
             (itr bindings ρ)]
            [`(define ,(list* f args) ,e)
             (define a (alloc f t))
             (sto-extend a 'undefined)]
            [_
             (unit ρ)])
          (λ (ρ′) (env/top es ρ′ t)))]
    [null
     (unit ρ)]))

(define interp
  (memo `interp
        (λ (e ρ t)
          (pretty-print e)
          (match e
            [`(program ,es)
             (interp/top es ρ t)]
            [`(block ,es)
             (interp/top es ρ t)]
            [`(memberE ,obj ,property)
             (eval/member e ρ t #f)]
            [`(seq ,es)
             (for-eachm (λ (e) (interp e ρ t)) es)]
            [`(assgn ,x ,e)
             (>>= (interp e ρ t)
                  (λ (v)
                    (>>= (match x
                           [(? symbol? x)
                            (unit (env-lookup ρ x))]
                           [`(memberE ,obj ,property)
                            (eval/member x ρ t #t)])
                         (λ (a) (sto-extend a v)))))]
            [`(λ ,args ,e)
             (unit (inj `(clos (λ ,args ,e) ,ρ)))]
            [`(if ,tst ,thn ,els)
             (proj/ev (interp tst ρ t) 'BOOL
                      (λ (b) (interp (if b thn els) ρ t)))]
            [`(bin ,op ,lhs ,rhs)
             (proj/ev (interp lhs ρ t) 'NUM
                      (λ (lv)
                        (proj/ev (interp rhs ρ t) 'NUM
                                 (λ (rv) ((lattice-op op) lv rv)))))]
            [`(unary ,op ,e)
             (if (equal? 'delete)
                 ....
                 (proj/ev (interp e ρ t) (unop->argtp op)
                          (λ (v) ((lattice-unop op) v))))]
            [`(logical ,op ,lhs ,rhs)
             (proj/ev (interp lhs ρ t) 'BOOL
                      (λ (b1)
                        (proj/ev (interp rhs ρ t) 'BOOL
                                 (λ (b2)
                                   (unit (inj ((logic/op op) b1 b2)))))))]
            [`(return ,e)
             (>>= (interp e ρ t)
                  (λ (v) (unit `(returnV ,v))))]
            [(list* 'app f args)
             (proj/ev (interp f ρ t) 'CLO
                      (match-lambda
                        [`(clos (λ ,params ,e) ,ρ′)
                         (define t′ (alloc `(app ,f ,args) t))
                         (define (extend xs es ρ′)
                           (match* (xs es)
                             [((cons x xs) (cons e es))
                              (>>= (interp e ρ t)
                                   (λ (v)
                                     (define a (alloc x t′))
                                     (>> (sto-extend a v)
                                         (extend xs es (env-extend ρ′ x a)))))]
                             [(null null)
                              (unit ρ′)]))
                         (>>= (extend params args ρ′)
                              (λ (ρ′) (interp e ρ′ t′)))]))]
            [`(forlp ,init ,tst ,update ,bod)
             (interp `(block (,init
                              (whlp ,tst
                                    ,(add-to-bod bod update))))
                     ρ t)]
            [`(whlp ,tst ,bod)
             (proj/ev (interp tst ρ t) 'BOOL
                      (λ (b) (if b
                                 (interp `(dowhlp ,bod ,tst) ρ t)
                                 (unit (void)))))]
            [`(dowhlp ,bod ,tst)
             (>>= (interp bod ρ t)
                  (match-lambda
                    [`(returnV ,v)
                     (unit `(return ,v))]
                    [_
                     (proj/ev (interp tst ρ t) 'BOOL
                              (λ (b)
                                (if b
                                    (interp `(dowhlp ,bod ,tst) ρ t)
                                    (unit (void)))))]))]
            [`(update ,op ,(? symbol? x))
             (interp `(assgn ,x (bin + ,x 1)) ρ t)]
            [`(arr ,elems)
             (define (itr es i)
               (match es
                 [(cons e es)
                  (cons `(,(string->symbol (number->string i)) : ,e)
                        (itr es (add1 i)))]
                 [null
                  `((length : ,(length elems)))]))
             (interp `(obj ,(itr elems 0))
                     ρ t)]
            [`(obj ,properties)
             (define (itr properties aggr)
               (match properties
                 [(list* `(,key : ,e) properties)
                  (>>= (interp e ρ t)
                       (λ (v)
                         (define a (obj-alloc key e t))
                         (>> (sto-extend a v)
                             (itr properties (push-back aggr `(,key : ,a))))))]
                 [(? null?)
                  (unit aggr)]))
             (>>= (itr properties (list))
                  (λ (aggr)
                    (unit (inj (list* 'obj aggr)))))]
            [`(try/catch ,try ,catch)
             (proj/ev (interp catch ρ t) 'CLO
                      (match-lambda
                        [`(clos (λ ,xs ,e′) ,ρ′)
                         (define (extend xs vs ρ′)
                           (match* (xs vs)
                             [((cons x xs) (cons v vs))
                              (define a (alloc x t))
                              (>> (sto-extend a v)
                                  (extend xs vs (env-extend ρ′ x a)))]
                             [(null null)
                              (unit ρ′)]))
                         (inj-exception (interp try ρ t)
                                        (λ (errv)
                                          (if (null? xs)
                                              (interp e′ ρ t)
                                              (>>= (extend xs (list errv) ρ′)
                                                   (λ (ρ′) (interp e′ ρ′ t))))))]))]
            [`(throw ,e)
             (>>= (interp e ρ t)
                  (λ (v) (error/exception v)))]
            [(? symbol? x)
             (sto-lookup (env-lookup ρ x))]
            [(or (? number?) (? boolean?) (? string?) 'null)
             (unit (inj e))]))))

(define (eval/member e ρ t addr?)
  (match e
    [`(memberE ,obj ,property)
     (proj/ev (interp obj ρ t) 'OBJ
              (match-lambda
                [(list* 'obj props)
                 (>>= (get-property property props)
                      (λ (a)
                        (if addr?
                            (unit a)
                            (sto-lookup a))))]))]))

(define (add-to-bod bod e)
  (match bod
    [`(block ,es)
     `(block ,(push-back es e))]))

(define op->
  (match-lambda
    ['+ +] ['- -] ['* *] ['/ /] ['% modulo]
    ['< <] ['> >] ['<= <=] ['>= >=] ['== =]))

(define op->top
  (match-lambda
    [(or '+ '- '* '/ '%) num-top]
    [(or '< '> '<= '>= '==) bool-top]))

(define unop->
  (match-lambda
    ['! not]))

(define logic/op
  (match-lambda
    ['&& (λ (x y) (and x y))]
    ['|| (λ (x y) (or x y))]))

(define unop->top
  (match-lambda
    [(or '+ '- '* '/ '%) num-top]
    [(or '!) bool-top]))

(define unop->argtp
  (match-lambda
    [(or '!) 'BOOL]))

(define op->type
  (match-lambda
    [(or '+ '- '* '/ '%) 'NUM]
    [(or '< '> '<= '>= '==) 'BOOL]))

(define (get-property key props)
  (match props
    [(cons `(,key′ : ,a) props)
     (if (equal? key key′)
         (unit a)
         (get-property key props))]
    [(? null?)
     fail]))

(define ((lattice-op op) l r)
  (match* (l r)
    [('⊥ _)
     (unit bot)]
    [(_ '⊥)
     (unit bot)]
    [('⊤ _)
     (if (and (equal? op '/) (equal? r 0))
         fail
         (unit (op->top op)))]
    [(_ '⊤)
     (unit (op->top op))]
    [(l r)
     (if (and (equal? op '/) (equal? r 0))
         fail
         (unit (inj ((op-> op) l r))))]))

(define ((lattice-unop op) x)
  (match x
    ['⊤
     (unit (unop->top op))]
    ['⊥
     fail]
    [_
     (unit (inj ((unop-> op) x)))]))


;----------------------------------------------------------------------------------------------------------
; Running analysis

(define (print h)
  (hash-for-each
   h
   (match-lambda**
    [(key
      (cons xs _))
     (pretty-print key)
     (pretty-print xs)
     (display "\n")]))
  (display "\n\n\n"))

(define (analyze filename)
  (define e (run-parse filename))
  (define h (run (interp e (hash) (list))))
  (define key `(interp ,e ,(hash) ,(list) ,(hash)))
  (if (verbose-mode) (print h) (display ""))
  (display "Query Result:\n")
  (pretty-print key)
  (pretty-print (hash-ref h key))
  )

(define (analyze* e)
  (define h (run (interp e (hash) (list))))
  (define key `(interp ,e ,(hash) ,(list) ,(hash)))
  (if (verbose-mode) (print h) (display ""))
  (display "Query Result:\n")
  (pretty-print key)
  (pretty-print (hash-ref h key)))

(require racket/cmdline)

(define filename
  (command-line
   #:program "compiler"
   #:once-each
   [("-v" "--verbose") "Compile with verbose messages"
                       (verbose-mode #t)]
   [("-s" "--show") "Compile with profiling"
                    (show-sto #t)]
   #:args (filename m)
   (current-m (string->number m))
   filename))

(analyze filename)

#;(define pr '(program ((let ((x 42))) (update ++ x) (let ((y x))))))
#;(define pr '(program ((let ((x 42))) (assgn x (bin + x 1)) (let ((y x))))))

#;(analyze* pr)
