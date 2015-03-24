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

(define word-size 4)

; register definitions
(define accum 'd0)
(define temp1 'd1)
(define temp2 'd2)
(define temp3 'd3)
(define accum-addr  'a0)
(define temp1-addr  'a1)
(define temp2-addr  'a2)
(define temp3-addr  'a2)
(define jump-target 'a4)
(define closure-ptr 'a5)
(define frame-ptr   'a6)

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

(define (header len tag)
  (arithmetic-shift (bitwise-ior (arithmetic-shift len tag-len) tag) 1))
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
                                (cg body (* (+ (length formals) 1) word-size) accum 'return 'ignored)
                                (cg-code))])))))

; generates the assembly for each given form
(define (cg exp frame-size dd cd next-label)
  (match exp
    ;[`(bound ,n ,name) (cg-load-branch 
    [`(quote ,obj)  (cg-set-branch obj dd cd next-label)]
    [`(if ,t ,c ,a) (let ([true-label (gen-label "iftrue")]
                          [false-label (gen-label "iffalse")])
                      (instructions
                       (cg t frame-size 'effect (join-labels true-label false-label) true-label)
                       `(label ,true-label)
                       (cg c frame-size dd cd false-label)
                       `(label ,false-label)
                       (cg a frame-size dd cd next-label)))]
    
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
   `(move.l ,(encode obj) ,dd)
   (cg-jump cd next-label)))

(define (cg-shuffle frame-size num)
  (let loop ([top frame-size]
             [bottom word-size]
             [num num])
    (if (zero? num)
        (instructions)
        (instructions
         `(move.l (,frame-ptr ,top) ,temp1)
         `(move.l ,temp1 (,frame-ptr ,bottom))
         (loop (+ top word-size) (+ bottom word-size) (- num 1))))))

;; assemble the different variations of jumps
(define (cg-jump label next-label)
  (if (eq? label 'return)
      (instructions
       `(bra.l (,frame-ptr 0)))
      (if (eq? label next-label)
          (instructions) ;; nothing (label is the same as the next instruction)
          (instructions `(bra.l ,label)))))

;; assemble a jump that sets up a closure
(define (cg-jump-closure)
  (instructions
   `(move.l ,accum ,closure-ptr)
   `(sub.l  ,closure-tag ,closure-ptr)
   `(move.l (,closure-ptr ,(* 1 word-size)) ,jump-target)
   `(jmp    ,jump-target)))

;; inline translation of builtin operators
(define (cg-inline exp name rands frame-size dd cd next-label)
  (case name
    [(%eq?)     (cg-binary-pred-inline exp rands frame-size dd cd next-label 'beq.l 'bne.l `(cmp.l ,temp1 ,temp2))]
    [(%fixnum?) (cg-type-test exp number-tag mask rands frame-size dd cd next-label)]
    [(%fx+)     (cg-true-inline cg-binary-rands rands frame-size dd cd next-label
                                (instructions
                                 `(move.l ,temp1 ,accum)
                                 `(add.l ,temp2 ,accum)))]
    [(%fx-)     (cg-true-inline cg-binary-rands rands frame-size dd cd next-label
                                (instructions
                                 `(move.l ,temp1 ,accum)
                                 `(sub.l ,temp2 ,accum)))]
    [(%fx*)     (cg-true-inline cg-binary-rands rands frame-size dd cd next-label
                                (instructions
                                 `(move.l ,temp1 ,accum)
                                 `(muls.w ,temp2 ,accum)))]
    [(%car) (cg-ref-inline cg-unary-rand rands frame-size dd cd next-label
                           `(move.l (,(- word-size pair-tag) ,temp1-addr) ,accum))]
    [(%cdr) (cg-ref-inline cg-unary-rand rands frame-size dd cd next-label
                           `(move.l (,(- (* 2 word-size) pair-tag) ,temp1-addr) ,accum))]
    [(%cons) (cg-true-inline cg-binary-rands rands frame-size dd cd next-label
                             (instructions
                              `(move.l ,accum ,temp1-addr)
                              `(move.l ,temp1 (,(* 1 word-size) ,temp1-addr))
                              `(move.l ,temp2 (,(* 2 word-size) ,temp1-addr))))]
    [(%null) (cg-type-test exp null-tag imm-mask rands frame-size dd cd next-label)]
    [(%string->uninterned-symbol)
     (cg-true-inline cg-unary-rand rands frame-size dd cd next-label
                     (instructions
                      (cg-fix-allocate 2 'accum-addr (cg-framesize frame-size) '(,temp1))
                      `(move.l ,(header 1 symbol-tag) ,accum-addr)
                      `(move.l ,temp1 (,word-size ,accum-addr))
                      (cg-type-tag symbol-tag 'accum-addr)))]
    ;;;;;;; TODO other primitives
    ))

(define (cg-branch true-label false-label next-label jump-if-true jump-if-false)
  (instructions
   (cond
     [(eq? true-label next-label)
      `(,jump-if-false ,false-label)]
     [(eq? false-label next-label)
      `(,jump-if-true ,true-label)]
     [else
      (instructions
       `(,jump-if-true ,true-label)
       `(bra.l ,false-label))])))

(define (cg-store src dest)
  (cond
    [(eq? dest 'effect) (instructions)]
    [(pair? dest) `(move.l ,src ,dest)]
    [else
     (if (eq? src dest)
         (instructions)
         `(move.l ,src dest))]))

(define (cg-effect-rands ls fs)
  (if (null? ls)
      (instructions)
      (let ([operand-label (gen-label "operand")])
        (instructions
         (cg (car ls) fs 'effect operand-label operand-label)
         `(label ,operand-label)
         (cg-effect-rands (cdr ls) fs)))))

;; generate assembly for loading a piece of data (and a jump to a continuation if applicable)
(define (cg-load-branch loc dd cd next-label)
  (cond
    [(eq? dd 'effect)
     (cond
       [(pair? cd)
        (let ([true-label (car cd)]
              [false-label (cadr cd)])
          (instructions
           `(move.l ,loc ,temp1)
           `(cmp.l  ,(encode #f) ,temp1)
           (cg-branch true-label false-label next-label 'bne.l 'beq.l)))]
        [else (cg-jump cd next-label)])]
    [(pair? dd)
     (let ([register (car dd)]
           [offset  (cadr dd)])
       (instructions
        `(move.l ,loc ,temp1)
        `(move.l ,temp1 (,offset ,register))
        (cg-jump cd next-label)))]
    [else
     (instructions
      `(move.l ,loc ,dd)
      (cg-jump cd next-label))]))

;; assemble code for unary operations
(define (cg-unary-rand operands frame-size)
  (let ([operand (car operands)])
    (let ([end-label (gen-label "unaryoperand")])
      (instructions
       (cg operand frame-size temp1 end-label end-label)
       `(label ,end-label)))))

;; assemble code for binary operations
(define (cg-binary-rands operands frame-size)
  (let ([op0 (car operands)]
        [op1 (cadr operands)])
    (let ([op0-label (gen-label "binary0")]
          [op1-label (gen-label "binary1")])
      (instructions
       (cg op0 frame-size `(,frame-size ,frame-ptr) op0-label op0-label)
       `(label ,op0-label)
       (cg op1 (+ frame-size (* 1 word-size)) accum op1-label op1-label)
       `(label ,op1-label)
       `(move.l ,accum ,temp2)
       `(move.l (,frame-size ,frame-ptr) ,temp1)))))

(define (cg-ref-inline operator operands frame-size dd cd next-label code)
  (if (eq? dd 'effect)
      (error "error in cg-ref-inline: not implemented")
      (instructions
       (operator operands frame-size)
       code
       (cg-store accum dd)
       (cg-jump cd next-label))))

(define (cg-true-inline operator operands frame-size dd cd next-label code)
  (if (eq? dd 'effect)
      (error "error in cg-true-inline: not implemented")
      (instructions
       (operator operands frame-size)
       code
       (cg-store 'accum dd)
       (cg-jump cd next-label))))

(define (cg-binary-pred-inline exp rands frame-size dd cd next-label true-instruction false-instruction code)
  (if (eq? dd 'effect)
      (if (pair? cd)
          (let ([true-label (car cd)]
                [false-label (cadr cd)])
            (instructions
             (cg-binary-rands rands)
             code
             (cg-branch true-label false-label next-label true-instruction false-instruction)))
          (instructions
           (cg-effect-rands rands frame-size)
           (cg-jump cd next-label)))
      (cg `(if ,exp '#t '#f) frame-size dd cd next-label)))

(define (cg-type-test exp tag mask rands frame-size dd cd next-label)
  (if (eq? dd 'effect)
      (if (pair? cd)
          (let ([true-label (car cd)]
                [false-label (cadr cd)])
            (instructions
             (cg-unary-rand rands frame-size)
             `(and.l ,mask ,temp1)
             `(cmp.l ,tag  ,temp1)
             (cg-branch true-label false-label next-label 'beq.l 'bne.l)))
          (instructions
           (cg-effect-rands rands frame-size)
           (cg-jump cd next-label)))
      (cg `(if ,exp '#t '#f) frame-size dd cd next-label)))

(define (cg-framesize fs)
  `(move.l ,fs (,frame-ptr ,(- word-size))))

(define (cg-allocate sizecode target frameinfocode usedregs)
  (let ([allocate
         (lambda (overflowcode)
           (let ([dont-label (gen-label "dontgc")])
             (instructions
              `(move.l ,accum ,target)
              `(add.l ,accum ,sizecode)
              `(cmp.l _heap_end ,accum)
              `(bge.l ,dont-label)
              overflowcode
              `(label ,dont-label))))])
    (allocate
     (instructions
      `(comment "gc")
      `(sub.l ,sizecode ,accum)
      frameinfocode
      `(movem.l [,temp3 ,temp2 ,temp1 ,accum ,closure-ptr ,frame-ptr] (a7-))
      ;(movem.l ,(encode-regs usedregs))
      `(move.l ,accum (a7+))
      `(call 'gc-collect)
      `(add.l ,(* 7 word-size) a7)
      `(movem.l [,temp3 ,temp2 ,temp1 ,accum ,closure-ptr ,frame-ptr] (+a7))
      `(move.l _gc_free ,accum-addr)
      (allocate faultcode)
      `(comment "end gc")))))


(define faultcode
  (instructions
   `(move.l 0 ,accum-addr)
   `(move.l (0 ,accum-addr) ,accum)))

(define (cg-fix-allocate n target frameinfocode usedregs)
  (let ([aligned (if (even? n) n (+ n 1))])
    (cg-allocate (* aligned word-size) target frameinfocode)))

(define (cg-type-tag tag reg)
  `(or.l ,tag ,reg))
    
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