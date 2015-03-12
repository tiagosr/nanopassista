#lang racket

#|
 | converts standard forms to simplified ones
 |#

#|
 | substitutes (if cond result-if-true) with (if cond result-if-true (void))
 | this substitution simplifies the treatment of the (if ...) form further on
 |#
(define remove-one-armed-if
  (match-lambda 
    [`(if ,condition
          ,first-arm) `(if ,condition
                           ,first-arm
                           (void))]
    [else else]))

#|
 | expands multiple-statement lambdas/lets/letrecs with implicit (begin ...)
 | to make them explicit
 |#
(define make-begin-explicit
  (match-lambda
    [`(lambda (,a* ...) ,body* ..2)          `(lambda ,a* (begin ,@body*))]
    [`(let [(,e* ,f*) ...] ,body* ..2)       `(let [,(map list e* f*)] (begin ,@body*))]
    [`(let ,name [(,e* ,f*) ...] ,body* ..2) `(let ,name [,(map list e*  f*)] (begin ,@body*))]
    [`(letrec [(,e* ,f*) ...] ,body* ..2)    `(letrec [,(map list e* f*)] (begin ,@body*))]
    [else else]))

#|
 | eliminates (set! ...) assignments to parameters in lambdas
 |#
(define (substitute-refs-with-vector-refs expr sym)
  (let loop ()
    (match expr
      [`(,before* ... ,(? (lambda (x) (eq? sym x)) x) ,after* ...)
       (loop `(,@before*
               (vector-ref! x 0)
               ,@after*) sym)]
      [else else])))

(define eliminate-assignments
  (match-lambda
    [`(lambda (,before-arg* ... ,arg ,after-arg* ...)
        (begin ,body-before* ... 
               (set! ,arg ,after)
               ,body-after* ...))
     (eliminate-assignments `(lambda ,@before-arg*
                               (lambda (,arg ,@after-arg*)
                                 (begin ,@body-before*
                                        (vector-set! ,arg 0 ,after)
                                        ,@body-after*))))]
    [else else]))