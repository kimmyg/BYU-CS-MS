#lang racket

(require "../lc/eval.rkt")
(require "transform.rkt")

(define (test-transform program)
  (let ((value1 (transform (eval program)))
        (value2 (eval `(,(transform program) (λ (x) x)))))
    (begin
      (display value1)
      (newline)
      (display value2)
      (newline))))

(test-transform '((λ (x) ((λ (x) x) x)) y))
(test-transform '(λ (x) x))
