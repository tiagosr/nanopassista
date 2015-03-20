#lang racket

(define (m68k-instructions . args)
  (cons 'instructions
        (let loop ([ls args])
          (if (null? ls)
              `()
              (if (eq? (caar ls) 'instructions)
                  (append (cdar ls) (loop (cdr ls)))
                  (cons (car ls) (loop (cdr ls))))))))
(define m68k-todo '())
(define (m68k-cg-form exp)
  (set! m68k-todo `((_scheme_entry (lambda () (fixed) ,exp)) . ,m68k-todo))
  (m68k-cg-code))

(define (m68k-cg-code)
  (if (null? m68k-todo)
      (m68k-instructions)
      (let ([first (car m68k-todo)]
            [rest  (cdr m68k-todo)])
        (set! m68k-todo rest)
        (let ([label (car first)])
          (match (cadr first)
            [`(lambda ,formals ,arity ,body)
             (m68k-instructions `(label ,label)
                                (m68k-cg-prologue formals arity)
                                (m68k-cg body (* (+ (length formals) 1) ws) 'ac 'return 'ignored)
                                (m68k-cg-code))])))))

(define (m68k-cg exp fs dd cd next-label)
  (match exp
    ;[`(bound ,n ,name) (m68k-cg-load-branch 
    [`(quote ,obj) (m68k-cg-set-branch obj dd cd next-label)]
    [`(if t c a)   (let ([true-label (gen-label "iftrue")]
                         [false-label (gen-label "iffalse")])
                     (instructions
                      (m68k-cg t fs 'effect (join-labels true-label false-label) true-label)
                      `(label ,true-label)
                      (m68k-cg c fs dd cd false-label)
                      `(label ,false-label)
                      (m68k-cg a fs dd cd next-label)))]
    ))
                      
