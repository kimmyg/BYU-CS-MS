#lang racket
(require redex)

(require "cm.rkt"
         "lc.rkt"
         "transform.rkt"
         "alpha.rkt")

(define (transform-test program)
  (let* ((value (first (apply-reduction-relation* λcm-rr program)))
         (value1 (first (apply-reduction-relation* λv-rr (init (transform value)))))
         (value2 (first (apply-reduction-relation* λv-rr (init (transform program))))))
    ;(if (alpha-eq? value1 value2)
        ;#t
        (begin
          (display value)
          (newline)
          (display (term->first-alpha value1))
          (newline)
          (display (term->first-alpha value2))
          (newline)
          (alpha-eq? value1 value2))))
          ;#f))))

(define (trace program)
  (traces λv-rr (init (transform program))))

;(transform-test '(λ (x) x))
;(transform-test '(ccm))
(transform-test '(wcm (ccm) (ccm)))
;(transform-test '(wcm (λ (x) (λ (y) y)) (ccm)))
;(transform-test '(wcm (λ (x) x) (ccm)))
;(transform-test '(wcm (λ (x) (λ (y) x)) (ccm)))
;(transform-test '(wcm 0 (wcm 1 (ccm))))

#;(transform-test '(wcm 0
       ((λ (ignored)
          ((λ (x) x)
           (wcm 1
                (ccm))))
        (λ (x) x))))

#;(transform-test '(wcm (λ (f) (λ (z) z))
       ((λ (ignored)
          ((λ (x) x)
           (wcm (λ (f) (λ (z) (f z)))
                (ccm))))
        (λ (x) x))))

;
;(transform-test '(λ (u) u))
;(trace '(λ (u) u))

(define (the-important-property-holds program)
  (begin
    (display program)
    (newline)
    (let* ((value1 (transform (first (apply-reduction-relation* λcm-rr program))))
           (value2 (first (apply-reduction-relation* λv-rr (init (transform program))))))
      (alpha-eq? value1 value2))))



;(redex-check λcm e (the-important-property-holds (term e)))
