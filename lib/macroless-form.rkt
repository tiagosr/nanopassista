#lang racket
(require "local-form.rkt")

(define library
  '(
    ;TODO fill this up
    ))

#|
 | expands macros and applies the base library
 |#

(define (macroless-form exp)
  (if (begin-exp? exp)
      exp
      `(begin ,@(remove define-macro-exp? (expand-top-level (cdr exp))))))

(define (append-library exp) ; shouldn't it be prepend-library?
  (if (begin-exp? exp)
      `(begin ,@library ,@(cdr exp))
      `(begin ,@library ,exp)))

(define (expand-top-level exps)
  (let ([env (make-base-namespace)])
    (map (lambda (e)
           (match e
             [`(define-macro _ _)
              (eval e env)
              e]
             [else (my-expand e env)])) exps)))

(define (my-expand exp env)
  (if (pair? exp)
      (match exp
        [`(define ,var ,e) `(define ,var ,(my-expand e env))]
        [`(quote ,obj) `(quote ,obj)]
        [`(begin . ,exps) `(begin ,@(map (lambda (e) (my-expand e env)) exps))]
        [`(if ,t ,c ,a) `(if ,(my-expand t env) ,(my-expand c env) ,(my-expand a env))]
        [`(set! ,v ,e) `(set! ,v ,(my-expand e env))]
        [`(lambda ,formals . ,bodies) `(lambda ,formals ,@(map (lambda (e) (my-expand e env)) bodies))]
        [else (let ([r (eval `(macroexpand `,exp) env)])
                (if (equal? exp r)
                    (map (lambda (e) (my-expand e env)) r)
                    (my-expand r env)))])
      exp))

(define (define-macro-exp? exp)
  (exp? exp 'define-macro))

(provide (all-defined-out))
