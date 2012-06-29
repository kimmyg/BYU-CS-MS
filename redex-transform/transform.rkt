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
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (e (gensym 'e))
        (f (gensym 'f))
        (snd_m (gensym 'snd_m)))
    `(abs ,k (abs ,m (app (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var y)))))) (abs ,snd_m (app (app ,(transform-inner (second app)) (abs ,e (app (app ,(transform-inner (third app)) (abs ,f (app (app (app (var ,e) (var ,f)) (var ,k)) (var ,m)))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m)))))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m))))))))))

(define (transform-wcm wcm)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (n (gensym 'n))
        (mark (gensym 'mark))
        (fst_m (gensym 'fst_m))
        (snd_m (gensym 'snd_m))
        (tail_m (gensym 'tail_m))
        (proper_tail_m (gensym 'proper_tail_m)))
    `(abs ,k (abs ,m (app
                      (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var y)))))) ; (k (snd m))
                      (abs ,snd_m (app (app ,(transform-inner (second wcm))
                                            (abs ,mark (app
                                                       (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var x)))))) ; (k (fst m))
                                                       (abs ,fst_m (app
                                                                    (abs ,k (app (var ,k) (app (app (var ,fst_m) (var ,snd_m)) (var ,m))))
                                                                    (abs ,tail_m (app
                                                                                  (abs ,k (app (var ,k) (app (var ,tail_m) (abs x (abs y (var y))))))
                                                                                  (abs ,proper_tail_m (app
                                                                                                       (abs ,k (app (var ,k) (abs p (app (app (var p) (abs x (abs y (var x)))) (abs p (app (app (var p) (var ,mark)) (var ,proper_tail_m)))))))
                                                                                                       (abs ,n (app (app ,(transform-inner (third wcm)) (var ,k)) (var ,n)))))))))))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m))))))))))

  
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
      ((eq? tag 'abs) (transform-abs e))
      ((eq? tag 'app) (transform-app e))
      ((eq? tag 'wcm) (transform-wcm e))
      ((eq? tag 'ccm) (transform-ccm e))
      ((eq? tag 'num) (transform-num e))
      (else (error "unrecognized tag" tag)))))
      
(define (transform e)
  (emit (transform-inner (parse e))))

(define (init e)
  `((,e (λ (x) x)) (λ (p) ((p (λ (x) (λ (y) y))) ,(transform '(λ (x) (λ (y) y)))))))

(provide transform
         init)