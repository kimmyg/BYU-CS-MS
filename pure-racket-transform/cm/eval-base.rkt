; rename

(define rename-wcm
  (λ (wcm x y)
    `(wcm ,(rename (second wcm) x y) ,(rename (third wcm) x y))))

(define rename-ccm
  (λ (ccm x y)
    ccm))

; occurs free in

(define (occurs-free-in-wcm wcm x)
  (or (occurs-free-in (second wcm) x) (occurs-free-in (third wcm) x)))

(define (occurs-free-in-ccm ccm x)
  #f)

; substitute

(define substitute-wcm
  (λ (wcm x f)
    `(wcm ,(substitute (second wcm) x f) ,(substitute (third wcm) x f))))

(define substitute-ccm
  (λ (ccm x f)
    ccm))

