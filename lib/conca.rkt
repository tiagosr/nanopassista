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
          ; arithmetic primitives
          ['+            (*vm* (+ (car stack) accum) (cdr stack) (cdr code) rstack env)] 
          ['-            (*vm* (- (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['*            (*vm* (* (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['/            (*vm* (/ (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['=            (*vm* (= accum (car stack)) (cdr stack) (cdr code) rstack env)]
          ['<            (*vm* (< (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['>            (*vm* (> (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['<=           (*vm* (<= (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['>=           (*vm* (>= (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['pow          (*vm* (expt (car stack) accum) (cdr stack) (cdr code) rstack env)]
          ['log          (*vm* (log accum) stack (cdr code) rstack env)]
          ['sqrt         (*vm* (sqrt accum) stack (cdr code) rstack env)]
          ['floor        (*vm* (floor accum) stack (cdr code) rstack env)]
          ['ceil         (*vm* (ceiling accum) stack (cdr code) rstack env)]
          ['++           (*vm* (+ accum 1) stack (cdr code) rstack env)]
          ['--           (*vm* (- accum 1) stack (cdr code) rstack env)]
          ['neg          (*vm* (- accum) stack (cdr code) rstack env)]
          ['%            (*vm* (modulo accum (car stack)) (cdr stack) (cdr code) rstack env)]
          ['b-and        (*vm* (bitwise-and accum (car stack)) (cdr stack) (cdr code) rstack env)]
          ['b-or         (*vm* (bitwise-ior accum (car stack)) (cdr stack) (cdr code) rstack env)]
          ['b-not        (*vm* (bitwise-not accum) stack (cdr code) rstack env)]
          ['b-xor        (*vm* (bitwise-xor accum (car stack)) (cdr stack) (cdr code) rstack env)]
          ['min          (*vm* (min accum (car stack)) (cdr stack) (cdr code) rstack env)]
          ['max          (*vm* (max accum (car stack)) (cdr stack) (cdr code) rstack env)]
          ; stack primitives
          ['dup          (*vm* accum (cons accum stack) (cdr code) rstack env)]
          ['drop         (*vm* (car stack) (cdr stack) (cdr code) rstack env)]
          ['swap         (*vm* (car stack) (cons accum (cdr stack)) (cdr code) rstack env)]
          ['rot          (*vm* (car stack) (cons (cadr stack) (cons accum (cddr stack))) (cdr code) rstack env)]
          ['over         (*vm* (cadr stack) (cons accum stack) (cdr code) rstack env)]
          ['tuck         (*vm* accum (cons (car stack) (cons accum (cdr stack))) (cdr code) rstack env)]
          ['pick         (*vm* (list-ref stack accum) stack (cdr code) rstack env)]
          ['roll         (*vm* (list-ref stack accum) (let ([i 0])
                                                        (filter (lambda (v)
                                                                  (set! i (+ i 1))
                                                                  (not (= i accum))) stack)) (cdr code) rstack env)]
                                                                
          ; word definition primitives
          [':
           (let loop ([name (cadr code)]
                      [words '()]
                      [next (cddr code)])
             (if (eq? (car next) '\;)
                 (*vm* accum stack (cdr next) rstack (append env (list (cons name (append words (list '\;))))))
                 (loop name (append words (list (car next))) (cdr next))))]
          ['\;           (*vm* accum stack (car rstack) (cdr rstack) env)]
          ; return stack primitives
          ['r>           (*vm* (car rstack) (cons accum stack) (cdr code) (cdr rstack) env)]
          ['>r           (*vm* (car stack) (cdr stack) (cdr code) (cons accum rstack) env)]
          ; conditionals
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
          ; I/O
          ['\.           (display accum)
                         (*vm* (car stack) (cdr stack) (cdr code) rstack env)]
          ; other types
          [(? number? x) (*vm* x (cons accum stack) (cdr code) rstack env)]
          [(? string? x) (*vm* x (cons accum stack) (cdr code) rstack env)]
          ['print-stack! (displayln (cons accum stack))
                         (*vm* accum stack (cdr code) rstack env)]
          [(? symbol? r) (*vm* accum stack (cdr (assoc r env)) (cons (cdr code) rstack) env)]))
      accum))

(provide (rename-out [*vm* conca:vm]))