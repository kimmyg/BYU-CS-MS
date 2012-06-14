(require racket/include)
(include "church.rkt")
;(include "fresh-variable.rkt")

(define cm-transform-var
  (λ (var)
    (let ((k (fresh-variable)))
      `(abs ,k ,var))))

(define cm-transform-abs
  (λ (abs)
    (let ((k (fresh-variable)))
      `(abs ,k (abs ,(second abs) (app ,(cm-transform-inner (third abs)) (var ,k)))))))
      ;`(abs ,k ,(cm-transform-inner (third abs))))))

(define cm-transform-app
  (λ (app)
    (let ((k (fresh-variable)))
      `(abs ,k (app (app ,(cm-transform-inner (second app)) (var ,k)) (app ,(cm-transform-inner (third app)) (var ,k)))))))
                      
(define cm-transform-wcm
  (λ (wcm)
    (let ((k (fresh-variable)))
      `(abs ,k (app ,(cm-transform-inner (third wcm)) (var ,k))))))

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
    (lc-emit `(app ,(cm-transform-inner (cm-parse term)) (abs z (app (app (var z) (abs x (abs y (var y)))) (abs x (abs y (var y)))))))))
    ;(lc-emit (cm-transform-inner (cm-parse term)))))
