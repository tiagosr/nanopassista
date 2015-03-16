#lang racket
(require "frontend.rkt")

#|
 | continuation-passing style
 |#

(define (cps-form exp)
  `((lambda (call/cc primordial-continuation)
      ,(cps exp 'primordial-continuation))
    (lambda (k f)
      (f k (lambda (dummy-k result)
             (k result))))
    (lambda (r) r)))

(define (cps exp cont-exp)
  (if (pair? exp)
      (match exp
        [`(quote ,obj) `(,cont-exp (quote ,obj))]
        [`(begin ,a ,b) (let ([b-exp (cps b cont-exp)]
                              [r (new-var 'r)])
                          (cps a `(lambda (,r) ,b-exp)))]
        [`(if ,t ,c ,a) (let ([r (new-var 'r)]
                              [c-exp (cps c cont-exp)]
                              [a-exp (cps a cont-exp)])
                          (cps t `(lambda (,r)
                                    (if ,r ,c-exp ,a-exp))))]
        [`(set! ,v ,e) (let ([r (new-var 'r)])
                         (cps e `(lambda (,r) (,cont-exp (set! ,v ,r)))))]
        [`(lambda ,formals ,body)
         (let ([k (new-var 'k)])
           `(,cont-exp (lambda ,(cons k formals) ,(cps body k))))]
        [else (let ([operator (car exp)]
                    [operands (cdr exp)])
                (if (and (symbol? operator)
                         (memq operator *primitives*))
                    (cps-list operands (lambda (args)
                                         (if (eq? operator '%apply)
                                             `(,operator ,cont-exp ,@args)
                                             `(,cont-exp (,operator ,@args)))))
                    (cps-list exp (lambda (args)
                                    (cons (car args)
                                          (cons cont-exp (cdr args)))))))])
      `(,cont-exp ,exp)))

(define (cps-list exp inner)
  (cps-list-body exp inner '()))

(define (cps-list-body exp inner args)
  (if (null? exp)
      (inner (reverse args))
      (cps (car exp)
           (let ([r (new-var 'r)])
             `(lambda (,r)
                ,(cps-list-body (cdr exp) inner (cons r args)))))))

(define (new-var id)
  (gensym (symbol->string id)))