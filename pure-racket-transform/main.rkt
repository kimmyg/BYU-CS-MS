#lang racket

(require "church.rkt")
(require "cm-eval.rkt")
(require "lc-eval.rkt")
(require "transform.rkt")

; cm-program -> cm-parsed-program -> (cm-parsed-value)

#|(define test-transform
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
            (newline)
            #f)))))|#

(define test-transform
  (λ (cm-program)
    (let ((lc-value1 (cm-eval cm-program))  ; E ->  v
          (lc-value2 (lc-eval (cm-transform cm-program)))) ; E -> C(E) -> v?
      (if (equal? lc-value1 lc-value2)
          #t
          (begin
            (display cm-program)
            (newline)
            (display lc-value1)
            (newline)
            (display lc-value2)
            (newline)
            #f)))))


(require "cm-random.rkt")
(define test-transform-n
  (λ (n)
    (if (= n 0)
        (display "done")
        (begin
          (test-transform (random-cm-term))
          (test-transform-n (- n 1))))))

;(test-transform '(wcm (wcm y (λ (z) (λ (z) (λ (x) (ccm))))) ((λ (z) (λ (y) (wcm (wcm (ccm) (λ (x) x)) (ccm)))) (ccm))))
;(display (lc-parse '(λ (y) (λ (z) ((z (λ (x) x)) (λ (z) ((z (λ (z) (λ (z) (λ (x) (λ (z) ((z y) (λ (x) (λ (y) y)))))))) (λ (x) (λ (y) y)))))))))
;(test-transform-n 100)

;(map test-transform (cm-terms-of-length 4))

(lc-eval 
 (cm-transform 
  '(wcm (λ (f) (λ (z) z))
        ((λ (ignored)
           (wcm (λ (f) (λ (z) (f z)))
                (ccm)))
         (λ (x) x)))))
"should be 1:nil"
(lc-eval
 (cm-transform
  '(wcm (λ (f) (λ (z) z))
        ((λ (ignored)
           ((λ (x) x)
            (wcm (λ (f) (λ (z) (f z)))
                 (ccm))))
         (λ (x) x)))))
"should be 1:0:nil"
(lc-eval
 (cm-transform
  `(wcm (λ (f) (λ (z) (f z)))
        (((,HEAD (ccm))
          (λ (zero)
            (wcm (λ (f) (λ (z) z))
                 (((,HEAD (ccm)) ;;; we want ccm = 0:nil, now its 1:nil, but don't want 0:1:nil
                   (λ (zero)
                     (λ (f) (λ (z) (f z)))))
                  zero))))
         (λ (f) (λ (z) z))))))
"should be 0"