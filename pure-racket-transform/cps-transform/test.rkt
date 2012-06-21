#lang racket

(require "../lc/eval.rkt")
(require "transform.rkt")

(define (test-transform program)
  (let ((value1 (eval `(,(transform (eval program)) (λ (x) x))))
        (value2 (eval `(,(transform program) (λ (x) x)))))
    (begin
      (display value1)
      (newline)
      (display value2)
      (newline))))

(test-transform '(cons 1 nil))
