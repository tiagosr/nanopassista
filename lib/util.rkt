#lang racket

(define *void* (void)) ; kind of poetic

#|
 | sets
 |#
(define (set-member? x s)
  (cond [(null? s) #f]
        [(eq? x (car s)) #t]
        [else (set-member? x (cdr s))]))

(define (set-cons x s)
  (if (set-member? x s)
      s
      (cons x s)))

(define (set-union s1 s2)
  (if (null? s1)
      s2
      (set-union (cdr s1) (set-cons (car s1) s2))))

(define (set-minus s1 s2)
  (if (null? s1)
      '()
      (if (set-member? (car s1) s2)
          (set-minus (cdr s1) s2)
          (cons (car s1) (set-minus (cdr s1) s2)))))

(define (set-intersect s1 s2)
  (if (null? s1)
      '()
      (if (set-member? (car s1) s2)
          (cons (car s1) (set-intersect (cdr s1) s2))
          (set-intersect (cdr s1) s2))))

#|
 | lists
 |#

(define (flat-length l)
  (if (null? l)
      0
      (if (pair? l)
          (+ (flat-length (car l)) (flat-length (cdr l)))
          1)))

(define (flatten lst)
  (cond [(pair? lst) (append (flatten (car lst)) (flatten (cdr lst)))]
        [(null? lst) '()]
        [else (cons lst '())]))

(define (replace lst al)
  (cond [(pair? lst) (cons (replace (car lst) al) (replace (cdr lst) al))]
        [(and (symbol? lst) (assq lst al)) (cdr (assq lst al))]
        [else lst]))

(define (nth n lst)
  (if (= n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))

(define (rnth n lst)
  (nth (- (length lst) n 1) lst))

(define (ncdr n lst)
  (if (= n 0)
      lst
      (ncdr (- n 1)
            (cdr lst))))

(define (join lst x)
  (define (join0 lst x)
    (if (null? (cdr lst))
        (cons x lst)
        (cons x (cons (car lst) (join0 (cdr lst) x)))))
  (if (and (pair? lst)
           (pair? (cdr lst)))
      (cons (car lst)
            (join0 (cdr lst) x))
      lst))

(define (caddddr lst)
  (car (cddddr lst)))

(define (replace-patch-rec lst ptr thing)
  (let ([ptr (reverse (string->list (symbol->string ptr)))])
    (let loop ([lst lst]
               [p (car ptr)]
               [rem (cdr ptr)])
      (if (<= (length rem) 0)
          (cond [(eq? p #\a) (cons thing (cdr lst))]
                [(eq? p #\d) (cons (car lst) thing)]
                [else (error "bad direction ~A" p)]) 
          (cond [(eq? p #\a) (cons (loop (car lst) (car rem) (cdr rem)) (cdr lst))]
                [(eq? p #\d) (cons (car lst) (loop (cdr lst) (car rem) (cdr rem)))]
                [else (error "bad direction ~A" p)])))))

; string utility functions

(define (replace-str str old new)
  (define (replace-str-impl before after old new)
    (if (< 0 (string-length after))
        (replace-str-impl (string-append before
                                         (make-string 1
                                                      (if (char=? (string-ref after 0)
                                                                  old)
                                                          new
                                                          (string-ref after 0))))
                          (substring after 1 (string-length after))
                          old
                          new)
        before))
  (replace-str-impl "" str old new))

(provide (all-defined-out))