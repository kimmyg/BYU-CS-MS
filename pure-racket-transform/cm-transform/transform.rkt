#lang racket

(require "church.rkt")
(require (prefix-in cm- "cm/parse.rkt"))
(require (prefix-in lc- "lc/emit.rkt"))

(define transform-var
  (λ (var)
    (let ((k (gensym 'x)))
      `(abs ,k ,var))))

(define transform-abs
  (λ (abs)
    (let ((k (gensym 'x)))
      `(abs ,k (abs ,(second abs) (app ,(transform-inner (third abs)) (var ,k)))))))

(define transform-app
  (λ (app)
    (let ((k (gensym 'x)))
      `(abs ,k (app (app ,(transform-inner (second app)) (var ,k))
                    (app ,(transform-inner (third app)) (var ,k)))))))
                      
(define transform-wcm
  (λ (wcm)
    (let ((k (gensym 'x)))
      (cm-parse `(λ (,k) (,(lc-emit (transform-inner (third wcm))) ((,PAIR ,TRUE) ((,PAIR ,(lc-emit `(app ,(transform-inner (second wcm)) (var ,k)))) (,SND (((,IF (,FST ,k)) (,SND ,k)) ,k))))))))))

(define transform-ccm
  (λ (ccm)
    '(abs p (app (var p) (abs x (abs y (var y)))))))

(define transform-inner
  (λ (term)
    (cond
      ((eq? (first term) 'var) (transform-var term))
      ((eq? (first term) 'abs) (transform-abs term))
      ((eq? (first term) 'app) (transform-app term))
      ((eq? (first term) 'wcm) (transform-wcm term))
      ((eq? (first term) 'ccm) (transform-ccm term))
      (else (error "unrecognized tag " (first term))))))

(define transform
  (λ (term)
    (lc-emit `(app ,(transform-inner (cm-parse term)) (abs z (app (app (var z) (abs x (abs y (var y)))) (abs x (abs y (var y)))))))))
    ;(lc-emit (cm-transform-inner (cm-parse term)))))

(provide transform)