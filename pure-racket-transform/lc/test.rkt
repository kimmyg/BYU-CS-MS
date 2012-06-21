#lang racket

(require "emit.rkt")
(require "eval.rkt")
(require "parse.rkt")

(define program '(isnil? (rst (cons 5 nil))))

(print program)
(newline)
(print (eval program))
(newline)