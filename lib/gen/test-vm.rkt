#lang racket

(define todo '())
(define (cg-form exp)
  (set! todo '((_scheme_entry (lambda () (fixed) ,exp)) . ,todo))
  (cg-code))

(define (cg-code)
  (if (null? todo)
      (instructions)
      (let ([first (car todo)]
            [rest  (cdr todo)])
        (set! todo rest)
        (let ([label (car first)])
          (match (cadr first)
            [`(lambda ,formals ,arity ,body)
             (instructions `(label ,label)
                           (cg-prologue formals arity)
                           (cg body 'return 'ignored)
                           (cg-code)
                           )])))))

(define (cg exp code next-label)
  (match exp
    [`(quote ,obj) (cg-set-branch obj next-label)]
    ;;; TODO
    ))

(provide (prefix-out test-vm: cg-form))
