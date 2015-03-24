#lang racket

(require "lib/macroless-form.rkt")
(require "lib/frontend.rkt")
(require "lib/backend.rkt")
(require "lib/gen/m68k.rkt")

(display (macroless-form '((lambda (x) (* x x)) 2)))
(macroless-form '(letrec ([square (lambda (x) (* x x))])
                   (square 2)))