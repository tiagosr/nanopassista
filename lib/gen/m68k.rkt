#lang racket

(require "../util.rkt")

(define numtop (expt 2 29))

; tags for encoding values in memory/rom
(define number-tag  #b000)
(define pair-tag    #b010)
(define string-tag  #b011)
(define symbol-tag  #b100)
(define vector-tag  #b101)
(define closure-tag #b110)
(define float-tag   #b111) ;;; should rather try encoding it in fixed-point, instead?

(define mask        #b111)
(define tag-len 3)

(define bool-tag #b00000001)
(define null-tag #b00001001)
(define char-tag #b00010001)

(define imm-mask #b11111111)

(define string4-tag    #b000)
(define bytevector-tag #b001)

(define attr-mask #b1111)
(define attr-len (+ tag-len 1))

(define ws 4)

(define (encode obj)
  (cond
    [(exact? obj) ;; exact number
     (cond
       ; shift integers into the mask
       [(and (<= 0 obj) (< obj numtop)) (* obj (+ mask 1))] ; positive integer
       [(and (<= (- numtop) obj) (< obj 0)) (* (+ numtop obj) (+ mask 1))] ; negative integer
       [else
        (error (format "~s is out of range" obj))])]
    ;[(inexact? obj) (bitwise-ior (get-u32 (f32vector obj) 0) float-tag)] ; won't encode floats or fixnums (yet)
    [(boolean? obj)
     (+ (* (if obj 1 0) (+ imm-mask 1)) bool-tag)]
    [(null? obj) null-tag]
    [(char? obj) (let ([val (char->integer obj)])
                   (+ (* val (+ imm-mask 1)) char-tag))]
    [else (error (format "~s is not encodable" obj))]))
(define (instructions . args)
  (cons 'instructions
        (let loop ([ls args])
          (if (null? ls)
              `()
              (if (eq? (caar ls) 'instructions)
                  (append (cdar ls) (loop (cdr ls)))
                  (cons (car ls) (loop (cdr ls))))))))
(define todo '())

;; compile an expression to assembly
(define (cg-form exp)
  (set! todo `((_scheme_entry (lambda () (fixed) ,exp)) . ,todo))
  (cg-code))


;; partwise expansion of expressions into assembly
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
                                (cg body (* (+ (length formals) 1) ws) 'ac 'return 'ignored)
                                (cg-code))])))))

; generates the assembly for each given form
(define (cg exp fs dd cd next-label)
  (match exp
    ;[`(bound ,n ,name) (cg-load-branch 
    [`(quote ,obj)  (cg-set-branch obj dd cd next-label)]
    [`(if ,t ,c ,a) (let ([true-label (gen-label "iftrue")]
                          [false-label (gen-label "iffalse")])
                      (instructions
                       (cg t fs 'effect (join-labels true-label false-label) true-label)
                       `(label ,true-label)
                       (cg c fs dd cd false-label)
                       `(label ,false-label)
                       (cg a fs dd cd next-label)))]
    
    ))

; generates the assembly prologue for a given function,
; dealing with fixed or variable arguments
(define (cg-prologue formals arity)
  (let ([correct-label (gen-label "correctarg")]
        [is-variable (eq? (caadr arity) 'variable)])
    (let ([least-length (- (length formals) (if is-variable 1 0))])
      (instructions
       ;;;;; TODO
       ))))

;; assemble a set-immediate-and-branch instruction sequence (branch automatically optimized)
(define (cg-set-branch obj dd cd next-label)
  (instructions
   `(movl ,(encode obj) ,dd ,(format "~s" obj))
   (cg-jump cd next-label)))

;; assemble the different variations of jumps
(define (cg-jump label next-label)
  (if (eq? label 'return)
      (instructions
       `(bra.l (fp 0)))
      (if (eq? label next-label)
          (instructions) ;; nothing (label is the same as the next instruction)
          (instructions `(bra.l ,label)))))

(define (join-labels a b)
  (cond [(pair? a) (join-labels (car a) b)]
        [(pair? b) (list a (cadr b))]
        [else (list a b)]))

(define gen-label
  (let ([n 0])
    (lambda (str)
      (set! n (+ n 1))
      (gensym (string-append str (number->string n))))))

(provide (prefix-out m68k: cg-form))