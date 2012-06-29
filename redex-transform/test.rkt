#lang racket
(require redex)

(require "cm.rkt"
         "lc.rkt"
         "transform.rkt"
         "alpha.rkt")

(define (transform-test program)
  (let* ((value (first (apply-reduction-relation* λcm-rr program)))
         (value1 (transform value))
         (value2 (first (apply-reduction-relation* λv-rr (init (transform program))))))
    (if (alpha-eq? value1 value2)
        #t
        (begin
          (display value)
          (newline)
          (display (term->static-distance value1))
          (newline)
          (display (term->static-distance value2))
          (newline)
          #f))))

(transform-test '(ccm))
(transform-test '(wcm 1 (ccm)))

#|
pass in λp.((p λx.λy.y) ,(transform-abs 'λx.λy.y)

\p.((p E) F)

\k.\m.(k \p.T[((p E) F)])






|#