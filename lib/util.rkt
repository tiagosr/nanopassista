#lang racket

(define (nth n lst)
  (if (= n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))

(define (ncdr n lst)
  (if (= n 0)
      lst
      (ncdr (- n 1)
            (cdr lst))))

(provide (all-defined-out))