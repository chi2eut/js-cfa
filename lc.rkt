#lang racket/base

(require racket/match racket/set racket/pretty racket/list)

         
(define ((unit . xs) σ κ) ((car κ) xs σ))
(define ((>>= c f) σ κ) (c σ (cons (λ (xs σ) ((apply f xs) σ κ)) (cdr κ))))
(define (>> m . ms) (foldl (λ (m M) (>>= M (λ _ m))) m ms))

(define ((∘ f g) s) (f (g s)))
(define (id s) s)

(define ((each . cs) σ κ) (foldl (λ (c s→s) (∘ s→s (c σ κ))) id cs))
(define (fail σ κ) ((cdr κ) (list (set)) σ))

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
  (cond
    [(hash-ref s k #f)
     => (match-lambda
          [(cons xss×σs κs)
           (for/fold ([s (hash-set s k (cons xss×σs (cons κ κs)))])
                     ([(list xs σ) (in-set xss×σs)])
             (((car κ) xs σ) s))])]
    [else
     ((c σ
       (cons (λ (xs σ)
               (λ (s)
                 (match-let
                     ([(cons xss×σs κs) (hash-ref s k)])
                   (if (set-member? xss×σs (list xs σ))
                       s
                       (for/fold ([s (hash-set s k (cons (set-add xss×σs (list xs σ)) κs))])
                                 ([κ (in-list κs)])
                         (((car κ) xs σ) s))))))
             (cdr κ)))
       (hash-set s k (cons (set) (list κ))))]))

(define ((memo tag f) . xs)
  (cache (cons tag xs) (apply f xs)))
(define (run c)
  ((c (hash) (cons (λ (xs σ) id) (λ (xs σ) id))) (hash)))

(define ((sto-lookup a) σ κ)
  ((unit (hash-ref σ a))
   σ κ))

(define (alloc σ κ)
  ((unit (hash-count σ))
   σ κ))

(define ((sto-extend a v) σ κ)
  ((unit (void))
   (hash-set σ a v) κ))

(define (env-lookup ρ x) (hash-ref ρ x))

(define (env-extend ρ x a) (hash-set ρ x a))

(define ((inj-exception c f) σ κ)
  (c σ
     (cons (car κ)
           (λ (xs σ)
             ((apply f xs)
              σ κ)))))

(define ((error/exception . xs) σ κ)
  ((cdr κ) xs σ))

(define interp
  (memo 'interp
        (λ (e ρ)
          (match e
            [(? number? n)
             (unit n)]
            [(? symbol? x)
             (sto-lookup (env-lookup ρ x))]
            [`(λ (,x) ,e)
             (unit `(clos (λ (,x) ,e) ,ρ))]
            [`(app ,f ,e)
             (>>= (interp f ρ)
                  (match-lambda
                    [`(clos (λ (,x) ,e′) ,ρ′)
                     (>>= (interp e ρ)
                          (λ (v)
                            (>>= alloc
                                 (λ (a)
                                   (>> (sto-extend a v)
                                       (interp e′ (env-extend ρ′ x a)))))))]))]
            [`(try ,e₀ ,e₁)
             (>>= (interp e₁ ρ)
                  (match-lambda
                    [`(clos (λ (,x) ,e) ,ρ′)
                     (inj-exception (interp e₀ ρ)
                                    (λ (errv)
                                      (>>= alloc
                                           (λ (a)
                                             (>> (sto-extend a errv)
                                                 (interp e (env-extend ρ′ x a)))))))]))]
            [`(throw ,e)
             (>>= (interp e ρ)
                  (λ (v) (error/exception v)))]))))

(define (run* e)
  (run (interp e (hash))))

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

#;(print (run* `(app (λ (x) x) 42)))

(print (run* `(try (throw 42) (λ (x) x))))
