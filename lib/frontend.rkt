#lang racket

#|
 | converts standard forms to simplified ones
 |#

(define *primitives*
  '(%eq? %fixnum? %car %cdr %cons
    %fx+ %fx- %fx* %fx/
    %fl+ %fl- %fl* %fl/
    %null?
    %string->uninterned-symbol
    string
    %string?
    %make-vector vector %vector-ref %vector-set!
    %make-byte-string %string-size %string-byte-ref %string-byte-set
    %string-fx-ref %string-fx-set!
    %object-tag-set! %object-tag-ref
    %dlsym %foreign-call %set-global-refs %global-refs
    %apply))
(define *keywords*
  '(quote begin set! lambda if))
    
(define (core-form expr)
  (if (pair? expr)
      (match expr
        [`(quote ,obj) `(quote ,obj)] ; quote objects continue unaltered
        [`(begin ,e0 . ,exps)
         (if (null? exps)
             (core-form e0)         ; begin with two or more expressions is iterated
             (let ([new-e0 (core-form e0)]
                   [new-e* (core-form `(begin ,exps))])
               `(begin ,new-e0 ,new-e*)))]
        [`(if ,t ,c ,a)               ; if is iterated
         (let ([new-t (core-form t)]
               [new-c (core-form c)]
               [new-a (core-form a)])
           `(if ,new-t ,new-c ,new-a))]
        [`(set! ,v ,e)                ; set! is checked for a symbol and an expression,
         (if (symbol? v)              ; then iterated over the expression
             `(set! ,v ,(core-form e))
             (error "bad expression" expr))]
        [`(lambda ,formals ,bodies ...) ; lambda has formals list fixed, checked and body iterated over 
         (let ([pformals (proper-list formals)]) ;; TODO check if list fix allows for varargs (probably not)
           (if (and (andmap symbol? pformals)
                    (andmap (lambda (x) (not (memq x *keywords*))) pformals)
                    (is-set? pformals))
               (let ([new-body (core-form `(begin ,@bodies))])
                 `(lambda ,formals ,new-body))
               (error (format "bad formals ~s in ~s" formals exp))))]
        [`(,val) (core-form val)]
        [else (if (or (null? expr)
                      (not (list? expr))
                      (memq (car expr) *keywords*))
                  (error "bad expression" expr)
                  (cons (core-form (car expr)) (core-form (cdr expr))))])
      (match expr
        [(? symbol? e) e]             ; symbol is passed over straight
        [(or (? number? e)            ; numbers, booleans, strings and chars are quoted
             (? boolean? e)
             (? string? e)
             (? char? e)) `(quote ,e)]
        [else (error "bad expression" expr)]))) ; else expression is invalid

(define (core-convert-list lst)
  (map core-form lst))

#|
 | substitutes (if cond result-if-true) with (if cond result-if-true (void))
 | this substitution simplifies the treatment of the (if ...) form further on
 |#
(define remove-one-armed-if
  (match-lambda 
    [`(if ,condition
          ,first-arm) `(if ,condition
                           ,first-arm
                           (void))]
    [else else]))

#|
 | expands multiple-statement lambdas/lets/letrecs with implicit (begin ...)
 | to make them explicit
 |#
(define make-begin-explicit
  (match-lambda
    [`(lambda (,a* ...) ,body* ..2)          `(lambda ,a* (begin ,@body*))]
    [`(let [(,e* ,f*) ...] ,body* ..2)       `(let [,(map list e* f*)] (begin ,@body*))]
    [`(let ,name [(,e* ,f*) ...] ,body* ..2) `(let ,name [,(map list e*  f*)] (begin ,@body*))]
    [`(letrec [(,e* ,f*) ...] ,body* ..2)    `(letrec [,(map list e* f*)] (begin ,@body*))]
    [else else]))

#|
 | eliminates (set! ...) assignments to parameters in lambdas
 | essentially "curries" a lambda at the referenced value to make it become a reference to a vector variable
 |#
(define (substitute-refs-with-vector-refs expr sym)
  (let loop ()
    (match expr
      [`(,before* ... ,(? (lambda (x) (eq? sym x)) x) ,after* ...)
       (loop `(,@before*
               (vector-ref! x 0)
               ,@after*) sym)]
      [else else])))

(define eliminate-assignments
  (match-lambda
    [`(lambda (,before-arg* ... ,arg ,after-arg* ...)
        (begin ,body-before* ... 
               (set! ,arg ,after)
               ,body-after* ...))
     (eliminate-assignments `(lambda ,@before-arg*
                               (lambda (,arg ,@after-arg*)
                                 (begin ,@body-before*
                                        (vector-set! ,arg 0 ,after)
                                        ,@body-after*))))]
    [else else]))

#|
 | Lifts literals from the heap (symbols and strings turn into quoted data)
 |#

(define s-table '())

(define (immediate-literal-form exp)
  (let ([quoted (cadr exp)]
        [exp (caddr exp)])
    (if (null? quoted)
        exp
        (let ([q-exps (map heap-literal-destruct (map cadadr quoted))]
              [q-vars (map car quoted)])
          (let ([exp `((lambda ,q-vars '(free) '(fixed) ,exp) ,@q-exps)])
            (if (null? s-table)
                exp
                (let ([s-exps (map symbol-destruct (map car s-table))]
                      [s-vars (map cadr s-table)])
                  `((lambda ,s-vars '(free) '(fixed) ,exp) ,@s-exps))))))))

(define (heap-literal-destruct obj)
  (cond
    [(symbol? obj) (let ([entry (assq obj s-table)])
                     (if (pair? entry)
                         (cadr entry)
                         (let ([v (gen-ssym)])
                           (set! s-table (cons (list obj v) s-table))
                           v)))]
    [(or (boolean? obj)
         (number? obj)
         (char? obj)
         (null? obj)) `(quote ,obj)]
    [(string? obj) (let ([char-exps (map (lambda (c) `(quote ,c)) (string->list obj))])
                     `(string ,@char-exps))]
    [(pair? obj) (let ([car-exp (heap-literal-destruct (car obj))]
                       [cdr-exp (heap-literal-destruct (cdr obj))])
                   `(%cons ,car-exp ,cdr-exp))]
    [(vector? obj) (let ([contents-exps (map heap-literal-destruct (vector->list obj))])
                     `(vector ,@contents-exps))]))


(define (symbol-destruct sym)
  (let ([char-exps (map (lambda (c) `(quote ,c)) (string->list (symbol->string sym)))])
    `(%string->uninterned-symbol (string ,@char-exps))))

#|
 | Free and bound variables separation
 | bound variable: local variable, declared in current scope
 | free variable: non-local variable, declared in an exterior scope
 |#

(define (variable-separation exp bounds frees)
  (if (not (pair? exp))
      (let ([i (pass-list-index exp bounds)])
        (if i
            `(bound ,i ,exp)
            (let ([i (pass-list-index exp frees)])
              (if i
                  `(free ,i exp)
                  exp))))
      (match exp
        [`(quote ,obj) `(quote ,obj)]
        [`(begin ,a ,b)
         (let ([a-exp (variable-separation a bounds frees)]
               [b-exp (variable-separation b bounds frees)])
           `(begin ,a-exp ,b-exp))]
        [`(if ,t ,a ,b)
         (let ([t-exp (variable-separation t bounds frees)]
               [a-exp (variable-separation a bounds frees)]
               [b-exp (variable-separation b bounds frees)])
           `(if ,t-exp ,a-exp ,b-exp))]
        [`(lambda ,formals ,quoted-frees ,arity ,body)
         (let ([free (cdadr quoted-frees)])
           (let ([free-exps (variable-separation-list free bounds frees)]
                 [body-exp (variable-separation body formals free)])
             `(build-closure (lambda ,formals ,arity ,body-exp) ,@free-exps)))]
        [else (let ([operator (car exp)]
                    [operands (cdr exp)])
                (let ([operator-exp (variable-separation operator bounds frees)]
                      [operands-exps (variable-separation-list operands bounds frees)])
                  `(,operator-exp ,@operands-exps)))])))

(define (variable-separation-list ls bounds frees)
  (map (lambda (e) (variable-separation e bounds frees)) ls))

#|
 | code generation form
 |#

(define (code-generation-form exp)
  (variable-separation exp '() '()))

#|
 | Utility functions for everything
 |#

(define (fix-list l)
  (if (pair? l)
      (cons (car l) (fix-list (cdr l)))
      (cons l '())))
(define (proper-list l)
  (if (list? l)
      l
      (fix-list l)))
(define (pass-list-index v ls)
  (for/or ([y ls] [i (in-naturals)] #:when (eq? v y)) i))
(define (is-set? ls)
  (or (null? ls)
      (and (not (memq (car ls) (cdr ls)))
           (is-set? (cdr ls)))))
(define gen-qsym gensym)
(define gen-ssym gensym)