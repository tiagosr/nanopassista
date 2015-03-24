#lang racket

(require "util.rkt")

#|
 | Tiny virtual machine for testing
 |#

(define (*vm* a ;; accumulator
              x ;; current code "word" pointer
              f ;; frame pointer
              c ;; current closure
              s ;; argument stack
              pc ;; program counter
              )
  (define (list-args s n)
    (let loop ([n (- n 1)]
               [a '()])
      (if (< n 0)
          a
          (loop (- n 1)
                (cons (index s n) a)))))
  (define (list-args2 s x n)
    (let loop ([n (- n 1)]
               [x (- x 1)]
               [a '()])
      (if (< n 0)
          a
          (loop (- n 1) (- x 1) (cons (index s x) a)))))
  (define (shift-args n m s)
    (let nextarg ([i (- n 1)])
      (unless (< i 0)
        (index-set! s (+ i m) (index s i))
        (nextarg (- i 1))))
    (- s m))
  
  (let ([key (nth 0 x)])
    (case key
      ; stops the virtual machine returning the accumulator register
      ['halt   a]
      ; dereferences a variable from the locals stack
      ['lref   (*vm* (index f (nth 1 x))         (cddr x) f c s (+ 1 pc))]
      ; dereferences a variable from the current frame
      ['fref   (*vm* (index-closure c (nth 1 x)) (cddr x) f c s (+ 1 pc))]
      ; dereferences a variable from the globals vector
      ['gref   (*vm* (index-global (nth 1 x))    (cddr x) f c s (+ 1 pc))]
      ; unboxes the current accumulator value
      ['unbox  (*vm* (unbox a)                   (cdr  x) f c s (+ 1 pc))]
      ; dereferences a value from the constant pool
      ['const  (*vm* (nth 1 x)                   (cddr x) f c s (+ 1 pc))]
      ; pushes a value to the argument stack
      ['push   (*vm* a                           (cdr  x) f c (push a s) (+ 1 pc))]
      ; pushes a frame to the argument stack
      ['frame  (*vm* a                           (cddr x) f c (push (ncdr (+ 2 (nth 1 x)) x)
                                                                   (push f (push c s))) (+ 1 pc))]
      ; returns from a scheme subroutine
      ['return (let ([s (- s (nth 1 x))])
                 (*vm* a (index s 0) (index s 1) (index s 2) (- s 3) (+ 1 pc)))]
      ; calls a subroutine or primitive procedure
      ['call
       (if (procedure? a)
           (*vm* (let loop ([i (- (nth 1 x) 1)]
                            [lst '()])
                   (if (< i 0)
                       (apply a lst)
                       (loop (- i 1) (cons (index s i) lst))))
                 (ncdr 2 x)
                 f c s (+ 1 pc))
           (let ([type (closure-type a)])
             (case type
               ['close1
                (let ([diff (- (nth 1 x) 1)])
                  (index-set! s diff (list-args s (nth 1 x)))
                  (*vm* a (closure-body a) (- s diff) a (- s diff) (+ 1 pc)))]
               ['close2
                (let ([diff (- (nth 1 x) (closure-argnum a))])
                  (index-set! s (- (nth 1 x) 1) (list-args2 s (nth 1 x) (+ diff 1)))
                  (let ([st (shift-args (- (closure-argnum a) 1)
                                        diff s)])
                    (*vm* a (closure-body a) s a s (+ 1 pc))))]
               ['close0 (*vm* a (closure-body a) s a s (+ 1 pc))])))]
      ; tests a value for truth
      ['test   (*vm* a (if a (ncdr 2 x) (ncdr (+ 2 (nth 1 x)) x)) f c s (+ 1 pc))]
      ; jumps straight to a given position in code
      ['jump   (*vm* a (ncdr (+ 2 (nth 1 x)) x) f c s (+ 1 pc))]
      ['shift  (*vm* a (ncdr 3 x) f c (shift-args (nth 1 x) (nth 2 x) s) (+ 1 pc))]
      
      ;; make closure:        type    args#     body       n         s
      ['close0 (*vm* (closure 'close0 (nth 2 x) (ncdr 4 x) (nth 1 x) s) (ncdr (+ 4 (nth 3 x)) x) f c (- s (nth 1 x)) (+ 1 pc))]
      ['close1 (*vm* (closure 'close1 (nth 2 x) (ncdr 4 x) (nth 1 x) s) (ncdr (+ 4 (nth 3 x)) x) f c (- s (nth 1 x)) (+ 1 pc))]
      ['close2 (*vm* (closure 'close2 (nth 2 x) (ncdr 4 x) (nth 1 x) s) (ncdr (+ 4 (nth 3 x)) x) f c (- s (nth 1 x)) (+ 1 pc))]
      
      ; boxes a variable
      ['box    (index-set! s (nth 1 x) (box (index s (nth 1 x))))
               (*vm* a (ncdr 2 x) f c s (+ 1 pc))]
      ; sets a local variable
      ['lset   (set-box! (index f (nth 1 x)) a)
               (*vm* a (ncdr 2 x) f c s (+ 1 pc))]
      ; sets a global variable
      ['gset   (*vm* (assign-global! (nth 1 x) a) (ncdr 2 x) f c s (+ 1 pc))]
      
      ; sets up a continuation
      ['conti  (*vm* (continuation s) (ncdr 1 x) f c s (+ 1 pc))]
      ; runs a continuation
      ['nuate  (*vm* a (ncdr 2 x) f c (restore-stack (nth 1 x)) (+ 1 pc))]
      
      ['fset   (set-box! (index-closure c (nth 1 x)) a)
               (*vm* a (ncdr 2 x) f c s (+ 1 pc))]
      )))

(define stack (make-vector 2000))
(define (push x s)
  (vector-set! stack s x)
  (+ s 1))
(define (index s i)
  (vector-ref stack (- s i 1)))
(define (index-set! s i v)
  (vector-set! stack (- s i 1) v))

(define (closure type argnum body n s)
  (let ([v (make-vector (+ n 3))])
    (vector-set! v 0 type)
    (vector-set! v 1 argnum)
    (vector-set! v 2 body)
    (let f ([i 0])
      (unless (= i n)
        (vector-set! v (+ i 3) (index s i))
        (f (+ i 1))))
    v))

(define (closure-body c)
  (vector-ref c 2))
(define (closure-type c)
  (vector-ref c 0))
(define (closure-argnum c)
  (vector-ref c 1))
(define (index-closure c n)
  (vector-ref c (+ n 3)))

(define *heap-ref* 99900)
(define *heap* (make-vector 150000))
(define *heap-pnt* *heap-ref*)
(define (box x)
  (vector-set! *heap* (- *heap-pnt* *heap-ref*) x)
  (set! *heap-pnt* (+ 1 *heap-pnt*))
  (- *heap-pnt* 1))
(define (unbox i)
  (vector-ref *heap* (- i *heap-ref*)))
(define (set-box! p x)
  (vector-set! *heap* (- p *heap-ref*) x))

(define (continuation s)
  (define (save-stack s)
    (let ([v (make-vector s)])
      (let copy ([i 0])
        (unless (= i s)
          (vector-set! v i (vector-ref stack i))
          (copy (+ i 1))))
      v))
  (closure 'close0
           -1
           (list 'lref 0 'nuate (save-stack s) 'return 0)
           0 0))

(define (restore-stack v)
  (let ([s (vector-length v)])
    (let copy ([i 0])
      (unless (= i s)
        (vector-set! stack i (vector-ref v i))
        (copy (+ i 1))))
    s))

(define *globals-v* (make-vector 200))
(define (assign-global! i x)
  (vector-set! *globals-v* i x))
(define (index-global i)
  (vector-ref *globals-v* i))

(define (show-globals)
  (trace #t "~% [g] --- ~A~%" *globals-v*))

; debugging

(define *debug-flag*     #f)
(define *output-globals* #f)
(define *debug-out-port* #t)
(define *show-stack*     #t)
(define *show-reg-c*     #t)


(define (show-reg a x f c s pc)
  (trace #t "~% [a] | ~A~% [x] |  ~A~% [pc] |  ~A~% [f] | ~A~% [c] | ~A~% [s]"
         a x pc f (if *show-reg-c* c ""))
  (if *show-stack*
      (ins-st s)
      #f))

(define (ins-st p)
  (trace #t "=> ~3@A [           ]~%" p)
  (let loop ([i p])
    (unless (or (< i 1)
                (< (+ i 1) p))
      (trace #t "               ~A [ ~10@A ]~%" (- i 1) (index p (- p i)))
      (loop (- i 1)))))

(define (trace opt frmt . args)
  (if *debug-flag*
      (apply format *debug-out-port* frmt args)
      (void)))

(provide (all-defined-out))