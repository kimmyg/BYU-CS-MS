#lang racket

(require (prefix-in lc- "../lc/eval.rkt"))
(require (prefix-in cm- "../cm/eval.rkt"))
(require "transform.rkt")

(define (test-transform program)
  (let ((value1 (lc-eval `((,(transform program) (λ (x) x)) (λ (p) ((p ,(transform '(λ (x) (λ (y) y)))) ,(transform '(λ (x) (λ (y) y))))))))  ; E -> C[E] -> C[v]
        (value2 (transform (cm-eval program)))) ; E ->  v   -> C[v]
    (begin
      (display value1)
      (newline)
      (display value2)
      (newline))))

(test-transform '(wcm 1 (ccm)))

;"should be 1:nil"
;(test-transform '(wcm 0 ((λ (ignore) (wcm 1 (ccm))) 0)))
;"should be 1:0:nil"
;(test-transform '(wcm 0 ((λ (ignore) ((λ (x) x) (wcm 1 (ccm)))) (λ (x) x))))

#|((define HEAD '(λ (p) (p (λ (x) (λ (y) x)))))

lc-eval 
 (transform 
  '(wcm (λ (f) (λ (z) z))
        ((λ (ignored)
           (wcm (λ (f) (λ (z) (f z)))
                (ccm)))
         (λ (x) x)))))
"should be 1:nil"
(lc-eval
 (transform
  '(wcm (λ (f) (λ (z) z))
        ((λ (ignored)
           ((λ (x) x)
            (wcm (λ (f) (λ (z) (f z)))
                 (ccm))))
         (λ (x) x)))))
"should be 1:0:nil"
(lc-eval
 (transform
  `(wcm (λ (f) (λ (z) (f z)))
        (((,HEAD (ccm))
          (λ (zero)
            (wcm (λ (f) (λ (z) z))
                 (((,HEAD (ccm)) ;;; we want ccm = 0:nil, now its 1:nil, but don't want 0:1:nil
                   (λ (zero)
                     (λ (f) (λ (z) (f z)))))
                  zero))))
         (λ (f) (λ (z) z))))))
"should be 0"|#