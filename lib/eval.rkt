#lang racket

(require "util.rkt")
(require "test-vm.rkt")
(require "macroless-form.rkt")

(define *builtin-fn-names*
  '(+ - * / < > = <= >= modulo remainder quotient
    undefined ; <- do we need this?
    void
    cons car cdr list vector
    null? pair? zero? boolean? number? symbol? procedure? list?
    set-car! set-cdr! length apply append
    memq memv member assq assv assoc
    not or and
    map list-ref list-tail reverse
    
    caar cadr cddr
    
    caaar caadr cadar caddr
    cdaar cdadr cddar cdddr
    
    caaaar caaadr caadar caaddr
    cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr
    cddaar cddadr cdddar cddddr
    
    eq? eqv? equal?
    char? char=? char<? char>? char<=? char>=? char->integer integer->char
    string? string-length make-string string-ref string-set! string=? string-append substring
    symbol->string
    string->symbol
    number->string
    vector? make-vector vector-length vector-ref vector-set!
    read 
    input-port? output-port? open-input-file open-output-file close-input-port close-output-port
    write display read-char write-char newline
    
    ))

(define *builtin-fns*
  (let* ([procs *builtin-fn-names*]
         [prcs  (make-vector (+ 3 (length procs)))])
    (vector-set! prcs 0 'type)
    (vector-set! prcs 1 'argnum)
    (vector-set! prcs 2 'body)
    (let loop ([i 3]
               [p procs])
      (unless (null? p)
        (vector-set! i (car p))
        (loop (+ i 1) (cdr p)))
      prcs)))

(define (run x)
  (let ([envs (cons '() *builtin-fn-names*)])
    (let ([code (compile (expand-qq (macroexpand x))
                         envs
                         '()
                         '(halt))])
      ;(output-globals)
      (*vm* '() code 0 *builtin-fns* 0 0))))

(define (p1 x) ; phase 1
  (compile (expand-qq (macroexpand x))
           (cons '() *builtin-fn-names*)
           '()
           '(halt)))

(define (p1-no-expand x)
  (compile x
           (cons '() *builtin-fn-names*)
           '()
           '(halt)))

(define (vmrun code)
  (*vm* '() code 0 *builtin-fns* 0 0))
(define (macroexpand x)
  (expand-rec x top-mark *syntaxes* #f))

(define (expand-qq x)
  (cond [(pair? x)
         (cond [(and (eqv? 'quasiquote (car x))
                     (pair? (cdr x)))
                (expand-qq-impl (cadr x))]
               [(eqv? 'quote (car x))
                x]
               [else (cons (expand-qq (car x))
                           (expand-qq (cdr x)))])]
        [else x]))
(define (alt-qsyms x)
  (replace x '((quote            . __Q__  )
               (unquote          . __UNQ__)
               (unquote-splicing . __UQS__)
               (quasiquote       . __QQ__ ))))
(define (back-qsyms x)
  (replace x '((__Q__   . quote)
               (__UNQ__ . unquote)
               (__UQS__ . unquote-splicing)
               (__QQ__  . quasiquote))))

(define (qq x qql uql)
  (cond [(null? x) '()]
        [(and (pair? x)
              (uqs? (car x)))
         (cons 'append (qqi x qql uql))]
        [else (cons 'cons (qqi x qql uql))]))
(define (qqi x qql uql)
  (cond [(and (pair? x)
              (or (unq? x)
                  (uqs? x)))
         (if (= qql (+ 1 uql))
             (cadr x)
             (cons 'cons (cons (list 'quote
                                     (cond [(unq? x) 'unquote]
                                           [(uqs? x) 'unquote-splicing]))
                               (cons (cons 'cons (qqi (cdr x) qql (+ 1 uql)))))))]
        [(and (pair? x)
              (q? x))
         (list 'list ''quote (qqi (cadr x) qql uql))]
        [(and (pair? x)
              (qq? x))
         (cons (qqi 'quasiquote qql uql)
               (list (qq (cdr x) (+ 1 qql) uql)))]
        [(and (pair x)
              (pair? (car x))
              (not (unq? (car x)))
              (not (uqs? (car x)))
              (not (q? (car x))))
         (cons (qq (car x) qql uql)
               (list (qq (cdr x) qql uql)))]
        [(pair? x)
         (cons (qqi (car x) qql uql)
               (if (or (unq? (cdr x))
                       (uqs? (cdr x))
                       (q?   (cdr x))
                       (qq?  (cdr x)))
                   (list (qqi (cdr x) qql uql))
                   (list (qq  (cdr x) qql uql))))]
        [else (list 'quote x)]))

(define (unq? x)
  (cond [(pair? x) (eqv? '__UNQ__ (car x))]
        [else #f]))
                         
(define (uqs? x)
  (cond [(pair? x) (eqv? '__UQS__ (car x))]
        [else #f]))

(define (qq? x)
  (cond [(pair? x) (eqv? '__QQ__  (car x))]
        [else #f]))

(define (q? x)
  (cond [(pair? x) (eqv? '__Q__   (car x))]
        [else #f]))

(define (expand-qq-impl x)
  (back-qsyms (qq (alt-qsyms x) 1 0)))

; GLOBALS

(define *globals-n* '())
(define (add-g* lst)
  (if (pair? lst)
      (begin (add-g (car lst))
             (add-g* (cdr lst)))
      '()))

(define (add-g x)
  (define (push-g x)
    (set! *globals-n* (cons x *globals-n*))
    (- (length *globals-n*) 1))
  (if (memq x *globals-n*)
      (- (length (memq x *globals-n*)) 1)
      (push-g x)))
