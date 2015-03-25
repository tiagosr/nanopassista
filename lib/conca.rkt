#lang racket

#|
 | Concatenative VM for quick interpreter prototyping
 |#

(define (*vm* accum
              stack
              code
              rstack
              env)
  (if (pair? code)
      (let ([word (car code)])
        (match word
          ['halt! accum]
          ['+            (*vm* (+ (car stack) accum) (cdr stack) (cdr code) rstack env)] 
          ['-            (*vm* (- (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['*            (*vm* (* (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['/            (*vm* (/ (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['=            (*vm* (eq? accum (car stack)) (cdr stack) (cdr code) rstack env)]
          ['dup          (*vm* accum (cons accum stack) (cdr code) rstack env)]
          ['drop         (*vm* (car stack) (cdr stack) (cdr code) rstack env)]
          [':
           (let loop ([name (cadr code)]
                      [words '()]
                      [next (cddr code)])
             (if (eq? (car next) '\;)
                 (*vm* accum stack (cdr next) rstack (append env (list (cons name (append words (list '\;))))))
                 (loop name (append words (list (car next))) (cdr next))))]
          ['\;           (*vm* accum stack (car rstack) (cdr rstack) env)]
          ['r>           (*vm* (car rstack) (cons accum stack) (cdr code) (cdr rstack) env)]
          ['>r           (*vm* (car stack) (cdr stack) (cdr code) (cons accum rstack) env)]
          
          ['if
           (if accum
               (*vm* (car stack) (cdr stack) (cdr code) rstack env)
               (let loop ([next (cdr code)])
                 (if (or (eq? (car next) 'else) (eq? (car next) 'endif))
                     (*vm* (car stack) (cdr stack) (cdr next) rstack env)
                     (loop (cdr next)))))]
          ['else
           (let loop ([next (cdr code)])
             (if (eq? (car next) 'endif)
                 (*vm* accum stack (cdr next) rstack env)
                 (loop (cdr next))))]
          ['endif        (*vm* accum stack (cdr code) rstack env)]
          ['\.           (display accum)
                         (*vm* (car stack) (cdr stack) (cdr code) rstack env)]
          
          [(? number? x) (*vm* x (cons accum stack) (cdr code) rstack env)]
          [(? string? x) (*vm* x (cons accum stack) (cdr code) rstack env)]
          ['print-stack! (displayln (cons accum stack))
                         (*vm* accum stack (cdr code) rstack env)]
          [(? symbol? r) (*vm* accum stack (cdr (assoc r env)) (cons (cdr code) rstack) env)]))
      accum))