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
(define closure-addr 'a5)
(define frame-addr   'a6)

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

(define (varref->address exp)
  (match exp
    [`(bound ,n ,name) `(,(* (+ n 1) word-size) ,frame-addr)]
    [`(free  ,n ,name) `(,(* (+ n 2) word-size) ,closure-addr)]))

; generates the assembly for each given form
(define (cg exp frame-size dd cd next-label)
  (match exp
    [`(bound ,n ,name) (cg-load-branch `(,(* (+ n 1) word-size) ,frame-addr) dd cd next-label)]
    [`(free  ,n ,name) (cg-load-branch `(,(* (+ n 1) word-size) ,closure-addr) dd cd next-label)]
    [`(quote ,obj)  (cg-set-branch obj dd cd next-label)]
    [`(if ,t ,c ,a) (let ([true-label (gen-label "iftrue")]
                          [false-label (gen-label "iffalse")])
                      (instructions
                       (cg t frame-size 'effect (join-labels true-label false-label) true-label)
                       `(label ,true-label)
                       (cg c frame-size dd cd false-label)
                       `(label ,false-label)
                       (cg a frame-size dd cd next-label)))]
    [`(build-closure ,code . ,fvars) (if (eq? dd 'effect)
                                         (error "error in build-closure: not implemented")
                                         (let ([code-label (gen-label "code")])
                                           (set! todo (cons (list code-label code) todo))
                                           (instructions
                                            `(comment "build-closure")
                                            (cg-fix-allocate (+ (length fvars) 2) accum-addr (cg-framesize frame-size) '())
                                            `(move.l ,(header (length fvars) closure-tag) (0 ,accum-addr))
                                            `(move.l (imm ,code-label) (,(* 1 word-size) ,accum-addr))
                                            (let loop ([ls fvars]
                                                       [pos 2])
                                              (if (null? ls)
                                                  (instructions)
                                                  (instructions
                                                   `(move.l ,(varref->address (car ls)) ,temp1)
                                                   `(move.l ,temp1 (,(* pos word-size) ,accum-addr))
                                                   (loop (cdr ls) (+ pos 1)))))
                                            (cg-type-tag closure-tag accum-addr)
                                            (cg-store accum-addr dd)
                                            `(comment "end build-closure")
                                            (cg-jump cd next-label))))]
    [else
     (let ([operator (car exp)]
           [operands (cdr exp)]
           [operator-label (gen-label "operator")])
       (cond
         [(and (eq? operator 'apply) (eq? cd 'return))
          (instructions
           `(comment "apply")
           (cg-ternary-rands operands frame-size)
           `(move.l ,temp1-addr (,word-size ,frame-addr))
           `(move.l ,frame-addr ,temp1-addr)
           `(addi.l ,temp1-addr ,(* 2 word-size))
           (let ([loop-label (gen-label "applyloop")]
                 [break-label (gen-label "applybreak")])
             (instructions
              `(label  ,loop-label)
              `(cmp.l  ,(encode '()) ,temp3-addr)
              `(beq.l  ,break-label)
              `(move.l (,(- word-size pair-tag) ,temp3-addr) (0 ,temp1-addr))
              `(addi.l ,temp1-addr ,word-size)
              `(move.l (,(- (* 2 word-size) pair-tag) ,temp3-addr) ,temp3-addr)
              `(jmp ,loop-label)
              `(label ,break-label)))
           `(sub.l ,frame-addr ,temp1-addr)
           `(subi.l ,temp1-addr ,word-size)
           `(asr.l 2 ,temp1-addr)
           `(move.l ,temp2-addr ,accum)
           (cg-jump-closure)
           `(comment "end apply"))]
         [(symbol? operator)
          (cg-inline exp operator operands frame-size dd cd next-label)]
         [(eq? cd 'return)
          (instructions
           (cg-rands operands frame-size)
           (cg operator (+ frame-size (* (length operands) word-size)) accum operator-label operator-label)
           `(label ,operator-label)
           (cg-shuffle frame-size (length operands))
           `(move.l ,(length operands) ,temp1)
           (cg-jump-closure))]
         [else
          (error "error in else: not implemented")]))]))


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
         `(move.l (,frame-addr ,top) ,temp1)
         `(move.l ,temp1 (,frame-addr ,bottom))
         (loop (+ top word-size) (+ bottom word-size) (- num 1))))))

;; assemble the different variations of jumps
(define (cg-jump label next-label)
  (if (eq? label 'return)
      (instructions
       `(bra.l (,frame-addr 0)))
      (if (eq? label next-label)
          (instructions) ;; nothing (label is the same as the next instruction)
          (instructions `(bra.l ,label)))))

;; assemble a jump that sets up a closure
(define (cg-jump-closure)
  (instructions
   `(move.l ,accum ,closure-addr)
   `(sub.l  ,closure-tag ,closure-addr)
   `(move.l (,closure-addr ,(* 1 word-size)) ,jump-target)
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
    [(string)
     (cg-true-inline cg-rands rands frame-size dd cd next-label
                     (instructions
                      `(comment "string")
                      (cg-fix-allocate
                       (+ (quotient (+ (length rands) (- word-size 1)) word-size) 1)
                       accum-addr
                       (cg-framesize (+ frame-size (* (length rands) word-size)))
                       '())
                      `(move.l ,(header (length rands) string-tag) (0 ,accum-addr))
                      (let loop ([fpos frame-size]
                                 [spos word-size]
                                 [num (length rands)])
                        (if (zero? num)
                            (instructions)
                            (instructions
                             `(move.l (,fpos ,frame-addr) ,temp1)
                             `(swap ,temp1) ;; swap low and high byte
                             `(move.b ,temp1 (,spos ,accum-addr))
                             (loop (+ fpos word-size) (+ spos 1) (- num 1)))))
                      (cg-type-tag string-tag accum-addr)
                      `(comment "end string")))]
    [(%string?)
     (cg-type-test exp string-tag mask rands frame-size dd cd next-label)]
    [(%make-vector)
     (cg-make-vector rands frame-size dd cd next-label vector-tag)]
                             
    ;;;;;;; TODO other primitives
    ))

(define (cg-make-vector rands frame-size dd cd next-label tag)
  (cg-true-inline cg-unary-rand rands frame-size dd cd next-label
                  (instructions
                   `(asr.l ,tag-len ,temp1)
                   `(move.l ,temp1 ,temp2)
                   (if (= tag vector-tag)
                       `(asl.l 2 ,temp2)
                       (instructions))
                   `(move.l ,temp2 ,temp3)
                   `(add.l ,(+ word-size mask) ,temp2)
                   `(and.l ,(bitwise-not mask) ,temp2)
                   (cg-allocate temp2 accum-addr (cg-framesize frame-size) '())
                   `(asl.l ,attr-len ,temp1)
                   `(ori.l ,(arithmetic-shift tag 1) ,temp1)
                   `(move.l ,temp1 (0 ,accum-addr))
                   (if (= tag vector-tag)
                       (let ([loop-label (gen-label "fillloop")]
                             [break-label (gen-label "fillbreak")])
                         (instructions
                          `(add.l accum-ptr ,temp3)
                          `(move.l accum-ptr ,temp1-addr)
                          `(add.l ,word-size ,temp1-addr)
                          `(label ,loop-label)
                          `(cmp.l ,temp2 ,temp3)
                          `(ble.s ,break-label)
                          `(move.l 0 (0 ,temp1-addr))
                          `(add.l ,word-size ,temp1-addr)
                          `(bra.s ,loop-label)
                          `(label ,break-label)))
                       (instructions))
                   (cg-type-tag tag accum-addr))))

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

(define (cg-rands operands frame-size)
  (if (null? operands)
      (instructions)
      (let ([operand-label (gen-label "operand")])
        (instructions
         (cg (car operands) frame-size `(,frame-size ,frame-addr) operand-label operand-label)
         `(label ,operand-label)
         (cg-rands (cdr operands) (+ frame-size word-size))))))

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
       (cg op0 frame-size `(,frame-size ,frame-addr) op0-label op0-label)
       `(label ,op0-label)
       (cg op1 (+ frame-size (* 1 word-size)) accum op1-label op1-label)
       `(label ,op1-label)
       `(move.l ,accum ,temp2)
       `(move.l (,frame-size ,frame-addr) ,temp1)))))

;; assemble code for ternary operators
(define (cg-ternary-rands operands frame-size)
  (let ([r0 (car operands)]
        [r1 (cadr operands)]
        [r2 (caddr operands)])
    (let ([r0-label (gen-label "ternary0")]
          [r1-label (gen-label "ternary1")]
          [r2-label (gen-label "ternary2")])
      (instructions
       (cg r0 frame-size `(,frame-size ,frame-addr) r0-label r0-label)
       `(label ,r0-label)
       (cg r1 (+ frame-size (* 1 word-size)) `(,(+ frame-size (* 1 word-size)) ,frame-addr) r1-label r1-label)
       `(label ,r1-label)
       (cg r2 (+ frame-size (* 2 word-size)) accum-addr r2-label r2-label)
       `(label ,r2-label)
       `(move.l ,accum-addr ,temp3-addr)
       `(move.l (,(+ frame-size (* 1 word-size)) ,frame-addr) ,temp2-addr)
       `(move.l (,frame-size ,frame-addr) ,temp1-addr)))))

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
  `(move.l ,fs (,frame-addr ,(- word-size))))

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
      `(movem.l [,temp3 ,temp2 ,temp1 ,accum ,closure-addr ,frame-addr] (a7-))
      ;(movem.l ,(encode-regs usedregs))
      `(move.l ,accum (a7+))
      `(call 'gc-collect)
      `(add.l ,(* 7 word-size) a7)
      `(movem.l [,temp3 ,temp2 ,temp1 ,accum ,closure-addr ,frame-addr] (+a7))
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