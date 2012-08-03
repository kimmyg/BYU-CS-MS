#lang racket
(require redex)

(require racket/pretty
         "cm.rkt"
         "lc.rkt"
         "transform.rkt"
         "alpha.rkt")

(define (transform-test program)
  (let* ((value (first (apply-reduction-relation* λcm-rr program)))
         (value1 (transform value))
         (value2 (first (apply-reduction-relation* λv-rr (init (transform program))))))
    (if (alpha-eq? value1 value2)
        (if #f
            (begin
              (display value)
              (newline)
              (display (term->first-alpha value1))
              (newline)
              (display (term->first-alpha value2))
              (newline)
              #t)
            #t)
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


;(transform-test '1)
;(transform-test '(λ (x) x))
;(transform-test '(ccm))
;(transform-test '((λ (x) x) (ccm)))
;(transform-test '(wcm 0 (ccm)))
;(transform-test '(wcm (ccm) (ccm)))
;(transform-test '(wcm (wcm (ccm) (ccm)) (λ (x) x)))
;(transform-test '(wcm (wcm (ccm) (ccm)) b))
;(transform-test '(wcm (λ (a) (λ (b) b)) (ccm)))
;(transform-test '(wcm (λ (x) x) (ccm)))
;(transform-test '(wcm (λ (x) (λ (y) x)) (ccm)))
;(transform-test '(wcm 0 (wcm 1 (ccm))))
;(transform-test 'F)
;(transform-test '(wcm (ccm) 3))
;(transform-test '((ccm) 2))
;(transform-test '(wcm ((ccm) 1) (ccm)))
;(transform-test '(wcm 0 ((ccm) (ccm))))
;(transform-test '(wcm 1 ((ccm) (λ (x) (λ (y) x)))))
;(transform-test '(a b))
;(transform-test '(wcm (ccm) (ccm)))
;(transform-test '((wcm (ccm) (ccm)) 1))
;(transform-test '(Y (wcm (ccm) (ccm))))
;(transform-test '(error (ccm)))

(define p '(wcm 0 (ccm)))

(traces λcm-rr p)
(traces λv-rr (init (transform p)))

(define (apply-reduction-relation/n rr e n [i 0])
  (if (= i n)
      (list e)
      (apply append (map (λ (t) (apply-reduction-relation/n rr t n (+ i 1))) (apply-reduction-relation rr e))))) 

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
;(trace '((ccm) 2))

(define (the-important-property-holds program)
  (let* ((value1 (transform (first (apply-reduction-relation* λcm-rr program))))
           (value2 (first (apply-reduction-relation* λv-rr (init (transform program))))))
      (alpha-eq? value1 value2)))

(define (prepare-cm-term term)
  term)

(redex-check λcm e (the-important-property-holds (term e)) #:attempts 1000 #:prepare prepare-cm-term)
