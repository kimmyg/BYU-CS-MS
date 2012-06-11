#lang racket
(require racket/include)

(include "fresh-variable.rkt")
(include "lc.rkt")
(include "cm.rkt")
(include "transform.rkt")

; cm-program -> cm-parsed-program -> (cm-parsed-value)

(define test-transform
  (λ (cm-program)
    (let ((lc-value1 (cm-transform (cm-eval cm-program)))  ; E ->  v   -> C(v)
          (lc-value2 (lc-eval (cm-transform cm-program)))) ; E -> C(E) -> C(v)
      (if (equal? lc-value1 lc-value2)
          #t
          (begin
            (display cm-program)
            (newline)
            (display lc-value1)
            (newline)
            (display lc-value2)
            (newline))))))

(test-transform 'x)
