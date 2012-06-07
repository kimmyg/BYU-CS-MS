(require racket/include)

(include "church.rkt")

(define cm-transform-var
  (λ (var)
    var))

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
    `(abs y (app ,(lc-parse SND) (var y)))))

(define cm-transform
  (λ (term)
    (cond
      ((eq? (first term) 'var) (cm-transform-var term))
      ((eq? (first term) 'abs) (cm-transform-abs term))
      ((eq? (first term) 'app) (cm-transform-app term))
      ((eq? (first term) 'wcm) (cm-transform-wcm term))
      ((eq? (first term) 'ccm) (cm-transform-ccm term))
      (else (error "unrecognized tag " (first term))))))

