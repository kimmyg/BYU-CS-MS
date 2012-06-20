#lang racket

(require "emit.rkt")
(require "eval.rkt")
(require "parse.rkt")

(define random-var
  (λ ()
    (let ((i (random 3)))
      (cond
        ((= i 0) 'x)
        ((= i 1) 'y)
        (else    'z)))))

(define random-abs
  (λ ()
    `(λ (,(random-var)) ,(random-term))))

(define random-app
  (λ ()
    `(,(random-term) ,(random-term))))

(define (random-num)
  (random 10))

(define (random-term)
  (let ((i (random 4)))
    (cond
      ((= i 0) (random-var))
      ((= i 1) (random-abs))
      ((= i 2) (random-app))
      (else    (random-num)))))

(define (test-emit-parse program)
  (equal? (emit (parse program)) program))

(let ((program (random-term)))
  (print program)
  (newline)
  (print (eval program))
  (newline))