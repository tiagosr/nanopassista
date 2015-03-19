#lang racket
(require "util.rkt")
(require "local-form.rkt")

(define library
  '(
    (define-macro let
      (lambda (decls . bodies)
        (if (pair? decls)
            (let ([vars (map car decls)] ; anonymous let
                  [vals (map cadr decls)])
              `((lambda ,vars ,@bodies) vals))
            (let ([vars (map car (car bodies))] ; named let
                  [vals (map cadr (car bodies))])
              `(letrec ([,decls (lambda ,vars ,@(cdr bodies))])
                 (,decls ,@vals))))))
    
    (define-macro let*
      (lambda (decls . bodies)
        (if (null? (cdr decls))
            `(let (,(car decls)) ,@bodies)
            `(let (,(car decls)) (let* ,(cdr decls) ,@bodies)))))
    
    (define-macro letrec
      (lambda (decls . bodies)
        (let ([vars (map car decls)]
              [vals (map cadr decls)])
          (let ([holders (map (lambda (x) #f) vars)]
                [assigns (map (lambda (v e) `(set! ,v e)) vars vals)])
            `((lambda ,vars ,@assigns ,@bodies) ,@holders)))))
    
    (define-macro cond
      (lambda args
        (if (null? args)
            #f
            (if (eq? (caar args) 'else)
                `(begin ,@(cdar args))
                (if (null? (cdar args))
                    `(let ([+value+ ,(caar args)])
                       (if +value+ +value+ (cond ,@(cdr args))))
                    `(if ,(caar args)
                         (begin ,@(cdar args))
                         (cond ,@(cdr args))))))))
    
    (define-macro do
      (lambda (var-form test-form . args)
        (let ([vars (map car var-form)]
              [vals (map cadr var-form)]
              [step (map cddr var-form)])
          `(letrec ([loop (lambda ,vars
                            (if ,(car test-form)
                                (begin ,@(cdr test-form))
                                (begin
                                  ,@args
                                  (loop ,@(map (lambda (x y)
                                                 (if (null? x)
                                                     y
                                                     (car x)))
                                               step vars)))))]) (loop ,@vals)))))
    
    (define caar (lambda (x) (car (car x))))
    (define cadr (lambda (x) (car (cdr x))))
    (define cdar (lambda (x) (cdr (car x))))
    (define cddr (lambda (x) (cdr (cdr x))))
    (define caaar (lambda (x) (car (car (car x)))))
    (define caadr (lambda (x) (car (car (cdr x)))))
    (define cadar (lambda (x) (car (cdr (car x)))))
    (define caddr (lambda (x) (car (cdr (cdr x)))))
    (define cdaar (lambda (x) (cdr (car (car x)))))
    (define cdadr (lambda (x) (cdr (car (cdr x)))))
    (define cddar (lambda (x) (cdr (cdr (car x)))))
    (define cdddr (lambda (x) (cdr (cdr (cdr x)))))
    
    (define list (lambda x x))
    
    

    ;TODO fill this up
    ))

#|
 | expands macros and applies the base library
 |#

(define (macroless-form exp)
  (if (begin-exp? exp)
      exp
      `(begin ,@(remove define-macro-exp? (expand-top-level (cdr exp))))))

(define (append-library exp) ; shouldn't it be prepend-library?
  (if (begin-exp? exp)
      `(begin ,@library ,@(cdr exp))
      `(begin ,@library ,exp)))

(define (expand-top-level exps)
  (let ([env (make-base-namespace)])
    (map (lambda (e)
           (match e
             [`(define-macro _ _)
              (eval e env)
              e]
             [else (my-expand e env)])) exps)))

(define (my-expand exp env)
  (if (pair? exp)
      (match exp
        [`(define (,var . ,formals) ,e) `(define ,var (lambda ,formals ,(my-expand e env)))]
        [`(define ,var ,e) `(define ,var ,(my-expand e env))]
        [`(quote ,obj) `(quote ,obj)]
        [`(begin . ,exps) `(begin ,@(map (lambda (e) (my-expand e env)) exps))]
        [`(if ,t ,c ,a) `(if ,(my-expand t env) ,(my-expand c env) ,(my-expand a env))]
        [`(set! ,v ,e) `(set! ,v ,(my-expand e env))]
        [`(lambda ,formals . ,bodies) `(lambda ,formals ,@(map (lambda (e) (my-expand e env)) bodies))]
        [else (let ([r (eval `(macroexpand `,exp) env)])
                (if (equal? exp r)
                    (map (lambda (e) (my-expand e env)) r)
                    (my-expand r env)))])
      exp))

(define (my-define-macro def . body)
  (if (pair? def)
      `(my-define-macro ,(car def) (lambda ,(cdr def) ,@body))
      `(hashtable/put! all-macros ',def ,@body)))
(define (macroexpand-1 exp)
  (void))


(define (set-global-syntax name x env)
  (let ([defpair (make-syntax-def name x '())]
        [r       (assq name *syntaxes*)])
    (if r
        (set-cdr! r (cdr defpair))
        (set! *syntaxes* (cons defpair *syntaxes*)))
    (if #f #t)))

(define (let-syntax-imp x env)
  (let loop ([bindings (cadr x)]
             [newenv '()])
    (if (null? bindings)
        (append newenv env)
        (let ([keyword (caar bindings)]
              [trnsfrm (cadar bindings)])
          (loop (cdr bindings) (cons (make-syntax-def keyword trnsfrm env) env))))))

(define (letrec-syntax-imp x env)
  (define (backpatch-env! x env)
    (if (not (null? x))
        (begin
          (if (eqv? '%PLACEHOLDER% (car (cddddr (car x))))
              (set-cdr! (cddddr (car x)) env)
              *undefined*)
          (backpatch-env! (cdr x)
                          env))
        *undefined*))
  (let loop ([bindings (cadr x)]
             [newenv '()])
    (if (null? bindings)
        (let ([x (append newenv env)])
          (backpatch-env! x x)
          x)
        (let ([keyword (caar bindings)]
              [trnsfrm (cadar bindings)])
          (loop (cdr bindings)
                (cons (make-syntax-def keyword trnsfrm '%PLACEHOLDER%) newenv))))))



(define (make-syntax-def name x env)
  (let ([syms (cadr x)]
        [patterns (map car (cddr x))]
        [templates (map cadr (cddr x))])
    (let ([def (list syms patterns templates env)])
      (cons name def))))

(define (expand x mark env rec?) ; r5rs-like macro expand
  (define (ellipsis? x) ; matching ...
    (and (pair? x)
         (pair? (cdr x))
         (eqv? (cadr x) '...)))
  (define (gen-variety x d)
    (cond [(symbol? x)
           (if (eqv? x '...)
               '...
               (string->symbol (string-append (symbol->string x)
                                              "_"
                                              (number->string d))))]
          [(pair? x)
           (if (ellipsis? x)
               (cons (gen-variety (car x) (+ 1 d))
                     (gen-variety (cdr x) d))
               (cons (gen-variety (car x) d)
                     (gen-variety (cdr x) d)))]
          [else x]))
  (define (gen-variety2 x d lst)
    (cond [(symbol? x)
           (if (eqv? x '...)
               '...
               (let ([new (string->symbol (string-append (symbol->string x)
                                                         "_"
                                                         (number->string d)))])
                 (if (and (memq x lst) (memq new lst))
                     new
                     x)))]
          [(pair? x)
           (if (ellipsis? x)
               (cons (gen-variety2 (car x) (+ 1 d) lst)
                     (gen-variety2 (cdr x) d lst))
               (cons (gen-variety2 (car x) d lst)
                     (gen-variety2 (cdr x) d lst)))]
          [else x]))
  (define (add-family x)
    (set! *family* (set-union x *family*))
    *family*)
  (define (pmatch x y lits)
    (let ([family '()])
      (let ([add-family (lambda (x) (set! family (set-union x family)) family)])
        (pm-impl x y lits '() '()))))
  (define (varsym? x lits)
    (and (symbol? x) (not (memq x lits))))
  (define (pm-impl x y lits cand bn)
    (cond [(or (eqv? x y)
               (eqv? x '_)
               (eqv? y '_))
           (begin (add-family cand)
                  (cons #t bn))]
          [(varsym? x lits)
           (begin (add-family cand)
                  (cons #t (cons (cons x y) bn)))]
          [(and (pair? x)
                (pair? y))
           (let ([k (pm-impl (car x) (car y) lits cand bn)])
             (if (car k)
                 (if (ellipsis? x)
                     (pm-impl (cons (gen-variety (car x) 0)
                                    (cdr x))
                              lits
                              (append (flatten (car x)) cand)
                              (cdr x))
                     (pm-impl (cdr x) (cdr y) lits cand (cdr k)))
                     (cons #f #f)))]
          [(and (null? y)
                (ellipsis? x))
           (begin (add-family cand)
                  (cons #t bn))]
          [#t (begin (add-family cand)
                     (cons #f #f))]))
  (define (transform template bind)
    (cond [(symbol? template) (let ([r (assq template bind)])
                                (if r
                                    (cdr r)
                                    template))]
          [(ellipsis? template) (let ([transformed (transform (car template) bind)])
                                  (if (equal? (car template) transformed)
                                      (if (pair? (cdr template))
                                          (transform (cddr template) bind)
                                          (void))
                                      (cons transformed (transform (let ([generated (gen-variety2 (car template) 0 *family*)])
                                                                     (if (equal? generated (car template))
                                                                         (if (pair? (cdr template))
                                                                             (transform (cddr template) bind)
                                                                             (void))
                                                                         (cons generated (cdr template))))))))]
          [(pair? template) (cons (transform (car template) bind)
                                  (transform (cdr template) bind))]
          [else template]))
  (let* ([stript (newstrip (car x))]
         [lits (literals stript x)]
         [isom (synobj-isomorph (normalize-synobj (add-mark mark x)) lits)])
    (let loop ([ptn (patterns stript env)]
               [tmpl (templates stript env)])
      (if (null? ptn)
          x
          (let ([r (pmatch (car ptn)
                           (cdr isom) lits)])
            (if (car r)
                (expand-rec (newstrip (normalize-synobj (add-mark mark (transform (transform (car tmpl)
                                                                                             (cdr r))
                                                                                  (car isom)))))
                            mark
                            (if rec?
                                env
                                (syntax-env stript env))
                            rec?)
                (loop (cdr ptn)
                      (cdr tmpl))))))))

(define (expand-rec x mark env rec?)
  (define (macro? x env)
    (let ([exp (syntax-object-expr x)])
      (and (pair? exp)
           (find-syntax (car exp) env))))
  (define (quote? x)
    (and (pair? x)
         (eqv? 'quote car x)))
  
  (cond [(symbol? x) x]
        [(quote? x) x]
        [(pair? x)
         (cond [(binding-exp? x)
                (expand-binding x mark env rec?)]
               [(macro? x env)
                (let ([newmark (make-mark)])
                  (expand x newmark env rec?))]
               [(or (eqv? 'define-syntax (car x))
                    (eqv? 'syntax-rules (car x))) x]
               [(syn-identifier? x) x]
               [else (cons (expand-rec (car x) mark env rec?)
                           (expand-rec (cdr x) mark env rec?))])]
        [else x]))

(define (newstrip x)
  (define (replace-sym sym wrap)
    (let ([mark* (get-mark* wrap '())]
          [sbst* (get-subst* wrap '())])
      (let search ([sbst* sbst*])
        (if (null? sbst*)
            sym
            (let ([sbst (car sbst*)])
              (if (and (eq? sym (subst-sym sbst))
                       (memq (subst-mark* sbst) mark*))
                  (subst-label sbst)
                  (search (cdr sbst*))))))))
  (define (get-mark* wrap acc)
    (if (null? wrap)
        acc
        (let ([w (car wrap)])
          (if (mark? w)
              (get-mark* (cdr wrap) (cons w acc))
              (get-mark* (cdr wrap) acc)))))
  (define (get-subst* wrap acc)
    (if (null? wrap)
        acc
        (let ([w (car wrap)])
          (if (subst? w)
              (get-subst* (cdr wrap) (cons w acc))
              (get-subst* (cdr wrap) acc)))))
  (cond [(syntax-object? x)
         (if (syn-identifier? x)
             (replace-sym (syntax-object-expr x)
                          (syntax-object-wrap x))
             (newstrip (syntax-object-expr x)))]
        [(pair? x) (cons (newstrip (car x)) (newstrip (cdr x)))]
        [else x]))

(define *family* '())
(define *syntaxes* '())

(define (find-syntax x env)
  (assq x env))

(define (literals x env)
  (cadr (find-syntax x env)))

(define (patterns x env)
  (caddr (find-syntax x env)))

(define (templates x env)
  (cadddr (find-syntax x env)))

(define (syntax-env x env)
  (let ([e (car (cddddr (find-syntax x env)))])
    (if (null? e)
        *syntaxes*
        e)))

(define (make-syntax-object exp wrap)
  (if (self-evaluating? exp)
      exp
      (cons '*syntax-object* (cons exp wrap))))

(define (syntax-object? x)
  (and (pair? x)
       (eq? (car x) '*syntax-object*)
       (pair? (cdr x))))

(define (syntax-object-expr x)
  (if (syntax-object? x)
      (cadr x)
      x))

(define (syntax-object-wrap x)
  (if (syntax-object? x)
      (cddr x)
      x))

(define *make-mark-num* 0)
(define (make-mark)
  (set! *make-mark-num* (+ *make-mark-num* 1))
  (string->symbol (string-append "mark." (number->string *make-mark-num*))))
(define (mark? x)
  (and (symbol? x)
       (let ([str (symbol->string x)])
         (and (<= 5 (string-length str))
              (string=? (substring str 0 5)
                        "mark.")))))

(define top-mark (make-mark))

(define (top-marked? wrap)
  (and (not (null? wrap))
       (or (eq? (car wrap) top-mark)
           (top-marked? (cdr wrap)))))

(define (make-subst id mark* label)
  (cons '*substitute* (cons id (cons mark* label))))

(define (subst? x)
  (and (pair? x)
       (eq? (car x) '*substitute*)))

(define (subst-sym x)
  (if (subst? x)
      (cadr x)
      x))

(define (subst-mark* x)
  (if (subst? x)
      (caddr x)
      x))

(define (subst-label x)
  (if (subst? x)
      (cadddr x)
      x))

(define (add-mark mark x)
  (extend-wrap (list mark) x))

(define (extend-wrap wrap x)
  (define (join-wraps wrap1 wrap2)
    (cond [(null? wrap1) wrap2]
          [(null? wrap2) wrap1]
          [else
           (let f ([w (car wrap1)]
                   [w* (cdr wrap1)])
             (if (null? w*)
                 (if (and (mark? w)
                          (eq? (car wrap2) w))
                     (cdr wrap2)
                     (cons w wrap2))
                 (cons w (f (car w*) (cdr w*)))))]))
  (if (syntax-object? x)
      (make-syntax-object
       (syntax-object-expr x)
       (join-wraps wrap (syntax-object-wrap x)))
      (make-syntax-object x wrap)))

(define (normalize-synobj x)
  (if (syntax-object? x)
      (let ([exp (syntax-object-expr x)]
            [wrp (syntax-object-wrap x)])
        (cond [(null? exp) '()]
              [(pair? exp) (cons (normalize-synobj (extend-wrap wrp (car exp)))
                                 (normalize-synobj (extend-wrap wrp (cdr exp))))]
              [else (extend-wrap wrp exp)]))
      x))

(define (synobj-isomorph x lits)
  (define synobj-id
    (let ([n 0])
      (lambda ()
        (set! n (+ 1 n))
        (string->symbol (string-append "syn." (number->string n))))))
  (define (synobj-isomorph-impl exp lits)
    (let ([x (cdr exp)]
          [b (car exp)])
      (cond [(null? x) (cons 'b '())]
            [(and (pair? x)
                  (syntax-object? x))
             (let ([id  (synobj-id)]
                   [sym (syntax-object-expr x)])
               (if (memq sym lits)
                   (cons (cons (cons sym x) b) sym)
                   (cons (cons (cons id  x) b) id)))]
            [(pair? x) (let ([kar (synobj-isomorph-impl (cons b (car x)) lits)]
                             [kdr (synobj-isomorph-impl (cons b (car x)) lits)])
                         (cons (append b (car kar) (car kdr))
                               (cons (cdr kar) (cdr kdr))))]
            [else (cons b x)])))
  (synobj-isomorph-impl (cons '() x) lits))

(define (self-evaluating? x)
  (or (boolean? x) (number? x) (string? x) (char? x)))

(define (binding-exp? x)
  (cond [(syntax-object? x) (binding-exp? (syntax-object-expr x))]
        [(pair? x) (or (and (syntax-object? (car x))
                            (eq? (syntax-object-expr (car x)) 'lambda))
                       (and (eq? (car x) 'lambda)
                            (pair? (cdr x))))]
        [else #f]))

(define (syn-identifier? x)
  (and (syntax-object? x)
       (symbol? (syntax-object-expr) x)))

(define *gen-var-num* 0)
(define (gen-var id)
  (set! *gen-var-num* (+ *gen-var-num* 0))
  (let ([name (syntax-object-expr id)])
    (string->symbol (string-append (symbol->string name) "." (number->string *gen-var-num*)))))

(define (expand-binding x mark env rec?)
  (define (add-subst id mark label x)
    (extend-wrap (list (make-subst (syntax-object-expr id) mark label)) x))
  (define (add-subst-and-mark bind exp mark)
    (if (null? bind)
        (normalize-synobj (add-mark mark exp))
        (add-subst-and-mark (cdr bind)
                            (add-subst (car bind)
                                       mark
                                       (gen-var (syntax-object-expr (car bind)))
                                       exp)
                            mark)))
  (define (get-bind x)
    (let ([bind (syntax-object-expr (cadr x))])
      (if (symbol? bind)
          (list bind)
          (flatten bind))))
  (let ([newmark (make-mark)]
        [bind    (get-bind x)])
    (let ([expanded (newstrip (add-subst-and-mark (syntax-object-expr bind)
                                                  (newstrip x)
                                                  newmark))])
      (cons (car expanded) (cons (cadr expanded)
                                 (expand-rec (cddr expanded) newmark env rec?))))))

(define (macro-names x)
  (map car x))


(define macroexpand
  (lambda args
    (let loop ([exp (car args)]
               [expanded (macroexpand-1 (car args) (if (null? (cdr args))
                                                       (current-namespace)
                                                       (cdar args)))])
      (if (eq? expanded exp)
          expanded
          (begin
            (set! exp expanded)
            (loop))))))

(define (define-macro-exp? exp)
  (exp? exp 'define-macro))

(provide (all-defined-out))
