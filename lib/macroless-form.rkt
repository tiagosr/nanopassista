#lang racket
(require "local-form.rkt")

(define library
  '(
    (define-macro let
      (lambda (decls . bodies)
        (if (pair? decls)
            (let ([vars (map car decls)] ; anonymous let
                  [vals (map cadr decls)])
              `((lambda ,vars ,@bodies) vals))
            (let ([vars (map car (car bodies))] ; named let
                  [vals (map cadr (car bodies))])
              `(letrec ([,decls (lambda ,vars ,@(cdr bodies))])
                 (,decls ,@vals))))))
    
    (define-macro let*
      (lambda (decls . bodies)
        (if (null? (cdr decls))
            `(let (,(car decls)) ,@bodies)
            `(let (,(car decls)) (let* ,(cdr decls) ,@bodies)))))
    
    (define-macro letrec
      (lambda (decls . bodies)
        (let ([vars (map car decls)]
              [vals (map cadr decls)])
          (let ([holders (map (lambda (x) #f) vars)]
                [assigns (map (lambda (v e) `(set! ,v e)) vars vals)])
            `((lambda ,vars ,@assigns ,@bodies) ,@holders)))))
    
    (define-macro cond
      (lambda args
        (if (null? args)
            #f
            (if (eq? (caar args) 'else)
                `(begin ,@(cdar args))
                (if (null? (cdar args))
                    `(let ([+value+ ,(caar args)])
                       (if +value+ +value+ (cond ,@(cdr args))))
                    `(if ,(caar args)
                         (begin ,@(cdar args))
                         (cond ,@(cdr args))))))))
    
    (define-macro do
      (lambda (var-form test-form . args)
        (let ([vars (map car var-form)]
              [vals (map cadr var-form)]
              [step (map cddr var-form)])
          `(letrec ([loop (lambda ,vars
                            (if ,(car test-form)
                                (begin ,@(cdr test-form))
                                (begin
                                  ,@args
                                  (loop ,@(map (lambda (x y)
                                                 (if (null? x)
                                                     y
                                                     (car x)))
                                               step vars)))))]) (loop ,@vals)))))
    
    (define caar (lambda (x) (car (car x))))
    (define cadr (lambda (x) (car (cdr x))))
    (define cdar (lambda (x) (cdr (car x))))
    (define cddr (lambda (x) (cdr (cdr x))))
    (define caaar (lambda (x) (car (car (car x)))))
    (define caadr (lambda (x) (car (car (cdr x)))))
    (define cadar (lambda (x) (car (cdr (car x)))))
    (define caddr (lambda (x) (car (cdr (cdr x)))))
    (define cdaar (lambda (x) (cdr (car (car x)))))
    (define cdadr (lambda (x) (cdr (car (cdr x)))))
    (define cddar (lambda (x) (cdr (cdr (car x)))))
    (define cdddr (lambda (x) (cdr (cdr (cdr x)))))
    
    (define list (lambda x x))
    
    

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
        [`(define (,var . ,formals) ,e) `(define ,var (lambda ,formals ,(my-expand e env)))]
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
