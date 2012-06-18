#lang racket

(require "church.rkt")
(require "cm-parse.rkt")
(require "lc-emit.rkt")

(define cm-transform-var
  (λ (var)
    (let ((k (gensym 'x)))
      `(abs ,k ,var))))

(define cm-transform-abs
  (λ (abs)
    (let ((k (gensym 'x)))
      `(abs ,k (abs ,(second abs) (app ,(cm-transform-inner (third abs)) (var ,k)))))))

(define cm-transform-app
  (λ (app)
    (let ((k (gensym 'x)))
      `(abs ,k (app (app ,(cm-transform-inner (second app)) (var ,k))
                    (app ,(cm-transform-inner (third app)) (var ,k)))))))
                      
(define cm-transform-wcm
  (λ (wcm)
    (let ((k (gensym 'x)))
      (cm-parse `(λ (,k) (,(lc-emit (cm-transform-inner (third wcm))) ((,PAIR ,TRUE) ((,PAIR ,(lc-emit `(app ,(cm-transform-inner (second wcm)) (var ,k)))) (,SND (((,IF (,FST ,k)) (,SND ,k)) ,k))))))))))

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
      (else (error "unrecognized tag " (first term))))))

(define cm-transform
  (λ (term)
    (lc-emit `(app ,(cm-transform-inner (cm-parse term)) (abs z (app (app (var z) (abs x (abs y (var y)))) (abs x (abs y (var y)))))))))
    ;(lc-emit (cm-transform-inner (cm-parse term)))))

(provide cm-transform)