#lang racket

#|
 | local form
 | replaces (define v ...) with (set! v ...) and makes them local variables through lambda
 |
 | transforms                        into
 | (begin                            ((lambda (a b)
 |   (bla)                              (foo)
 |   (define a (lambda () 10))          (set! a (lambda () 10))
 |   (define b (lambda () 20))          (set! b (lambda () 20))
 |   (a))                               (a))
 |                                     #f #f)
 |#

(define (local-form exp)
  (if (begin-exp? exp)
      (let ([vars (defined-vars (cdr exp))])
        `((lambda ,vars
            ,@(replace-define (cdr exp)))
          ,@(map (lambda (v) #f) vars)))
      exp))

(define (defined-vars exps)
  (foldr (lambda (exp vars)
           (if (define-exp? exp)
               (cons (cadr exp) vars)
               vars))
         '()
         exps))

(define (replace-define exps)
  (map (lambda (exp)
         (if (define-exp? exp)
             `(set! ,@(cdr exp))
             exp)) exps))

(define (exp? exp sym)
  (and (pair? exp)
       (eq? (car exp) sym)))
(define (begin-exp? exp)
  (exp? exp 'begin))
(define (define-exp? exp)
  (exp? exp 'define))