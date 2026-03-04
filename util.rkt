#lang racket/base

(require racket/set racket/match)
(provide (all-defined-out))

(define .... 'REPLACE-ME)

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
    [`(clos (λ (,xs) ,e) ,ρ)
     `(⊥ ,(set) ⊥ ⊥ ,(set `(clos (λ (,xs) ,e) ,ρ)) ,(set) ⊥)]
    [`(obj ps)
     `(⊥ ,(set) ⊥ ⊥ ,(set) ,(set `(obj ps)) ⊥)]
    [`null
     `(⊥ ,(set) ⊥ ⊥ ,(set) ,(set) ⊤)]))

(define bot `(⊥ ⊥ ⊥ ⊥ ,(set) ,(set) ⊥))

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



