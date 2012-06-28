#lang racket

(require "cm-parse.rkt"
         "lc-emit.rkt")

(define (transform-var var)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k (abs ,m (app (var ,k) ,var)))))

(define (transform-abs abs)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k (abs ,m (app (var ,k) (abs ,(second abs) ,(transform-inner (third abs))))))))

(define (transform-app app)
  ())

(define (transform-wcm wcm)
  ())

(define (transform-ccm ccm)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k (abs ,m (app (var ,k) (app (var ,m) (abs x (abs y (var y)))))))))

(define (transform-num num)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k (abs ,m (app (var ,k) ,num)))))
                           
(define (transform-inner e)
  (let ((tag (first e)))
    (cond
      ((eq? tag 'var) (transform-var e))
      ((eq? tag 'abs) (transform-var e))
      ((eq? tag 'app) (transform-var e))
      ((eq? tag 'wcm) (transform-var e))
      ((eq? tag 'ccm) (transform-var e))
      ((eq? tag 'num) (transform-var e))
      (else (error "unrecognized tag" tag)))))
      
(define (transform e)
  (emit `(app (app ,(parse (transform-inner e)) (abs x (var x))) (abs p (app (app (var p) (abs x (abs y (var y)))) (abs x (abs y (var y))))))))

(provide transform)