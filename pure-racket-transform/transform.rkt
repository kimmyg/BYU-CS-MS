(require racket/include)
(include "church.rkt")
;(include "fresh-variable.rkt")

(define cm-transform-var
  (λ (var)
    `(abs ,(fresh-variable) (var ,(second var)))))

(define cm-transform-abs
  (λ (abs)
    abs))

(define cm-transform-app
  (λ (app)
    app))

(define cm-transform-wcm
  (λ (wcm)
    '()))

(define cm-transform-ccm
  (λ (ccm)
    '(abs p (app (var p) (abs x (abs y (var y)))))))

(define cm-transform-inner
  (λ (term)
    (cond
      ((eq? (first term) 'var) (cm-transform-var term))
      ((eq? (first term) 'abs) (cm-transform-abs term))
      ((eq? (first term) 'app) (cm-transform-app term))
      ((eq? (first term) 'wcm) (cm-transform-wcm term))
      ((eq? (first term) 'ccm) (cm-transform-ccm term))
      (else (error "cm-transform-inner unrecognized tag " (first term))))))

(define cm-transform
  (λ (term)
    ;(lc-emit `(app ,(cm-transform-inner (cm-parse term)) (abs z (app (app (var z) (abs x (abs y (var y)))) (abs x (abs y (var y)))))))))
    (lc-emit (cm-transform-inner (cm-parse term)))))
