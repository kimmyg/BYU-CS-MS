#lang racket

(require "eval.rkt")

(define program '(wcm (λ (x) x) (ccm)))

(define HEAD '(λ (p) (p (λ (x) (λ (y) x)))))

(eval 
 '(wcm (λ (f) (λ (z) z))
       ((λ (ignored)
          (wcm (λ (f) (λ (z) (f z)))
               (ccm)))
        (λ (x) x))))
"should be 1:nil"
(eval
 '(wcm (λ (f) (λ (z) z))
       ((λ (ignored)
          ((λ (x) x)
           (wcm (λ (f) (λ (z) (f z)))
                (ccm))))
        (λ (x) x))))
"should be 1:0:nil"
(eval
 `(wcm (λ (f) (λ (z) (f z)))
       (((,HEAD (ccm))
         (λ (zero)
           (wcm (λ (f) (λ (z) z))
                (((,HEAD (ccm)) ;;; we want ccm = 0:nil, now its 1:nil, but don't want 0:1:nil
                  (λ (zero)
                    (λ (f) (λ (z) (f z)))))
                 zero))))
        (λ (f) (λ (z) z)))))
"should be 0"
