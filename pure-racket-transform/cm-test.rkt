#lang racket
(require racket/include)

(include "cm.rkt")

(define k (cm-parse '(λ (x) (λ (y) y))))

(define program '(wcm (λ (x) x) (wcm (λ (y) (y y)) (ccm))))
(define parsed-program (cm-parse program))
(define parsed-value (cm-eval parsed-program k))
(define value (cm-emit parsed-value))

(display value)
(newline)
