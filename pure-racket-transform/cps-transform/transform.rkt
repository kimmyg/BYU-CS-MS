#lang racket

(require "../lc/emit.rkt")
(require "../lc/parse.rkt")

(define (transform-var var)
  (let ((k (gensym 'k)))
    `(abs ,k (app (var ,k) ,var))))

(define (transform-abs abs)
  (let ((k (gensym 'k)))
    `(abs ,k (app (var ,k) (abs ,(second abs) ,(transform-inner (third abs)))))))

(define (transform-app app)
  (let ((k (gensym 'k))
        (e (gensym 'e))
        (f (gensym 'f)))
    `(abs ,k (app ,(transform-inner (second app)) (abs ,e (app ,(transform-inner (third app)) (abs ,f (app (app (var ,e) (var ,f)) (var ,k)))))))))

; takes lc terms to lc terms
(define (transform-inner term)
  (if (list? term)
      (let ((tag (first term)))
        (cond
          ((eq? tag 'var) (transform-var term))
          ((eq? tag 'abs) (transform-abs term))
          ((eq? tag 'app) (transform-app term))
          (else (error "unrecognized tag" tag))))
      (error "expected list, got" term)))

(define (transform term)
  (emit (transform-inner (parse term))))

(provide transform)