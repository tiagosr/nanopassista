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
      (let ([word (car code)]
            [sequence (cdr code)])
        (match word
          ['halt! accum]
          ; arithmetic primitives
          ['+            (*vm* (+ (car stack) accum) (cdr stack) sequence rstack env)] 
          ['-            (*vm* (- (car stack) accum) (cdr stack) sequence rstack env)]
          ['*            (*vm* (* (car stack) accum) (cdr stack) sequence rstack env)]
          ['/            (*vm* (/ (car stack) accum) (cdr stack) sequence rstack env)]
          ['=            (*vm* (= accum (car stack)) (cdr stack) sequence rstack env)]
          ['<            (*vm* (< (car stack) accum) (cdr stack) sequence rstack env)]
          ['>            (*vm* (> (car stack) accum) (cdr stack) sequence rstack env)]
          ['<=           (*vm* (<= (car stack) accum) (cdr stack) sequence rstack env)]
          ['>=           (*vm* (>= (car stack) accum) (cdr stack) sequence rstack env)]
          ['pow          (*vm* (expt (car stack) accum) (cdr stack) sequence rstack env)]
          ['log          (*vm* (log accum) stack sequence rstack env)]
          ['sqrt         (*vm* (sqrt accum) stack sequence rstack env)]
          ['floor        (*vm* (exact-floor accum) stack sequence rstack env)]
          ['ceil         (*vm* (exact-ceiling accum) stack sequence rstack env)]
          ['++           (*vm* (+ accum 1) stack sequence rstack env)]
          ['--           (*vm* (- accum 1) stack sequence rstack env)]
          ['neg          (*vm* (- accum) stack sequence rstack env)]
          ['%            (*vm* (modulo accum (car stack)) (cdr stack) sequence rstack env)]
          ['b-and        (*vm* (bitwise-and accum (car stack)) (cdr stack) sequence rstack env)]
          ['b-or         (*vm* (bitwise-ior accum (car stack)) (cdr stack) sequence rstack env)]
          ['b-not        (*vm* (bitwise-not accum) stack sequence rstack env)]
          ['b-xor        (*vm* (bitwise-xor accum (car stack)) (cdr stack) sequence rstack env)]
          ['bit-set?     (*vm* (bitwise-bit-set? (car stack) accum) (cdr stack) sequence rstack env)]
          ['asl.b        (*vm* (bitwise-and #xff (arithmetic-shift (car stack) accum)) (cdr stack) sequence rstack env)]
          ['asl.w        (*vm* (bitwise-and #xffff (arithmetic-shift (car stack) accum)) (cdr stack) sequence rstack env)]
          ['asl.l        (*vm* (bitwise-and #xffffffff (arithmetic-shift (car stack) accum)) (cdr stack) sequence rstack env)]
          ['rol.b    (let ([to-shift (bitwise-and #xff (car stack))])
                       (let ([shifted-1 (arithmetic-shift to-shift accum)]
                             [shifted-2 (arithmetic-shift to-shift (- 8 accum))])
                         (*vm* (bitwise-and #xff (bitwise-ior shifted-1 shifted-2)) (cdr stack) sequence rstack env)))]
          ['rol.w    (let ([to-shift (bitwise-and #xffff (car stack))])
                       (let ([shifted-1 (arithmetic-shift to-shift accum)]
                             [shifted-2 (arithmetic-shift to-shift (- 16 accum))])
                         (*vm* (bitwise-and #xffffffff (bitwise-ior shifted-1 shifted-2)) (cdr stack) sequence rstack env)))]
          ['rol.l    (let ([to-shift (bitwise-and #xffffffff (car stack))])
                       (let ([shifted-1 (arithmetic-shift to-shift accum)]
                             [shifted-2 (arithmetic-shift to-shift (- 32 accum))])
                         (*vm* (bitwise-and #xffffffff (bitwise-ior shifted-1 shifted-2)) (cdr stack) sequence rstack env)))]
          
          ['min          (*vm* (min accum (car stack)) (cdr stack) sequence rstack env)]
          ['max          (*vm* (max accum (car stack)) (cdr stack) sequence rstack env)]
          ['sin          (*vm* (sin accum) stack sequence rstack env)]
          ['cos          (*vm* (cos accum) stack sequence rstack env)]
          ['tan          (*vm* (tan accum) stack sequence rstack env)]
          ['asin         (*vm* (asin accum) stack sequence rstack env)]
          ['acos         (*vm* (acos accum) stack sequence rstack env)]
          ['atan         (*vm* (atan accum) stack sequence rstack env)]
          ['atan2        (*vm* (acos (car stack) accum) (cdr stack) sequence rstack env)]
          ['pi           (*vm* pi (cons accum stack) sequence rstack env)]
          ['+inf         (*vm* +inf.0 (cons accum stack) sequence stack env)]
          ['-inf         (*vm* -inf.0 (cons accum stack) sequence stack env)]
          ['inf?         (*vm* (infinite? accum) stack sequence stack env)]
          
          ; stack primitives
          ['dup          (*vm* accum (cons accum stack) sequence rstack env)]
          ['drop         (*vm* (car stack) (cdr stack) sequence rstack env)]
          ['swap         (*vm* (car stack) (cons accum (cdr stack)) sequence rstack env)]
          ['rot          (*vm* (car stack) (cons (cadr stack) (cons accum (cddr stack))) sequence rstack env)]
          ['over         (*vm* (cadr stack) (cons accum stack) sequence rstack env)]
          ['tuck         (*vm* accum (cons (car stack) (cons accum (cdr stack))) sequence rstack env)]
          ['nip          (*vm* accum (cdr stack) sequence rstack env)]
          ['pick         (*vm* (list-ref stack accum) stack sequence rstack env)]
          ['roll         (*vm* (list-ref stack accum) (let ([i 0])
                                                        (filter (lambda (v)
                                                                  (set! i (+ i 1))
                                                                  (not (= i accum))) stack)) sequence rstack env)]
          ['depth        (*vm* (+ 1 (length stack)) (cons accum stack) sequence rstack env)]
          ['?dup         (*vm* (car stack) (if accum
                                               stack
                                               (cdr stack)) sequence rstack env)]
          ; list operation primitives
          ['cons         (*vm* (cons (car stack) accum) (cdr stack) sequence rstack env)]
          ['car          (*vm* (car accum) stack sequence rstack env)]
          ['cdr          (*vm* (cdr accum) stack sequence rstack env)]
          ['length       (*vm* (length accum) stack sequence rstack env)]
          
          ; data conversion primitives
          ['num->str     (*vm* (number->string accum) stack sequence rstack env)]
          ['str->num     (*vm* (string->number accum) stack sequence rstack env)]
          ['hexstr->num  (*vm* (string->number accum 16) stack sequence rstack env)]
          ['radixstr->num (*vm* (string->number (car stack) accum) (cdr stack) sequence rstack env)]
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
          ['r>           (*vm* (car rstack) (cons accum stack) sequence (cdr rstack) env)]
          ['>r           (*vm* (car stack) (cdr stack) sequence (cons accum rstack) env)]
          ['rdrop        (*vm* accum stack sequence (cdr rstack) env)]
          ; conditionals
          ['if
           (if accum
               (*vm* (car stack) (cdr stack) sequence rstack env)
               (let loop ([next sequence])
                 (if (or (eq? (car next) 'else) (eq? (car next) 'then))
                     (*vm* (car stack) (cdr stack) (cdr next) rstack env)
                     (loop (cdr next)))))]
          ['else
           (let loop ([next sequence])
             (if (eq? (car next) 'then)
                 (*vm* accum stack (cdr next) rstack env)
                 (loop (cdr next))))]
          ['then         (*vm* accum stack sequence rstack env)]
          ['begin        (*vm* accum stack sequence (cons sequence rstack) env)]
          ['until    (if accum
                         (*vm* (car stack) (cdr stack) sequence (cdr rstack) env)
                         (*vm* (car stack) (cdr stack) (car rstack) rstack env))]
          
          ; I/O
          ['\.           (printf " ~a" accum)
                         (*vm* (car stack) (cdr stack) sequence rstack env)]
          ['dec.         (printf " ~d" accum)
                         (*vm* (car stack) (cdr stack) sequence rstack env)]
          ['hex.         (printf " ~x" accum)
                         (*vm* (car stack) (cdr stack) sequence rstack env)]
          ['HEX.         (printf " ~X" accum)
                         (*vm* (car stack) (cdr stack) sequence rstack env)]
          ['emit         (printf " ~c" accum)
                         (*vm* (car stack) (cdr stack) sequence rstack env)]
          ['cr           (write #\n)
                         (*vm* accum stack sequence rstack env)]
          
          ; other types
          [(? number? x) (*vm* x (cons accum stack) sequence rstack env)]
          [(? string? x) (*vm* x (cons accum stack) sequence rstack env)]
          ['print-stack! (displayln (cons accum stack))
                         (*vm* accum stack sequence rstack env)]
          [(? symbol? r) (*vm* accum stack (cdr (assoc r env)) (cons sequence rstack) env)]))
      accum))

(provide (rename-out [*vm* conca:vm]))