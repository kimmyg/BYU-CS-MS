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
          (display (term->first-alpha value1))
          (newline)
          (display (term->first-alpha value2))
          (newline)
          #f))))

(define (trace program)
  (traces λv-rr (init (transform program))))

(transform-test '(ccm))
(transform-test '(wcm 1 (ccm)))
(transform-test '(wcm (λ (x) (λ (y) x)) (ccm)))
(transform-test '(wcm 0 (wcm 1 (ccm))))

;(trace '(wcm 1 (ccm)))

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


(define program (transform '(λ (p) ((p a) b))))
;(traces λv-rr `(((((,program (λ (r) r)) (λ (x) x)) (λ (x) (λ (k) (λ (m) (k (λ (y) (λ (k) (λ (m) (k y))))))))) (λ (x) x)) (λ (p) ((p fst_) snd_))))

;(traces λv-rr (init (transform '(wcm 1 (wcm 2 (ccm))))))
