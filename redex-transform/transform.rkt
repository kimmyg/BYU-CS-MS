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

(define (substitute-app rator rand)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (e (gensym 'e))
        (f (gensym 'f))
        (snd_m (gensym 'snd_m)))
    `(abs ,k (abs ,m (app (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var y)))))) (abs ,snd_m (app (app ,rator (abs ,e (app (app ,rand (abs ,f (app (app (app (var ,e) (var ,f)) (var ,k)) (var ,m)))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m)))))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m))))))))))


(define (transform-app app)
  (substitute-app (transform-inner (second app)) (transform-inner (third app))))

(define (transform-wcm wcm)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (n (gensym 'n))
        (mark (gensym 'mark))
        (flag (gensym 'flag))
        (marks (gensym 'marks))
        (tail_marks (gensym 'tail_marks))
        (rest_marks (gensym 'rest_marks)))
    `(abs ,k (abs ,m (app
                      (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var y)))))) ; (k (snd m))
                      (abs ,marks (app
                                   (abs ,k (app (app (app (app (app (var ,marks) (abs x (var x))) (abs x (var x))) (abs x (abs ,k (abs ,m (app (var ,k) (abs y (abs ,k (abs ,m (app (var ,k) (var y)))))))))) (abs x (app (var ,k) (abs ,k (abs ,m (app (var ,k) (var x))))c))) (abs p (app (app (var p) (var x)) (var y)))))
                                   ;(abs ,k (app (var ,k) (app (app (app (var ,marks) (abs x (var x))) (abs x (var x))) (abs x (abs y (var y))))))
                                   (abs ,tail_marks (app (app ,(transform-inner (second wcm))
                                                              (abs ,mark (app
                                                                          (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var x)))))) ; (k (fst m))
                                                                          (abs ,flag (app
                                                                                       (abs ,k (app (var ,k) (app (app (var ,flag) (var ,tail_marks)) (var ,marks))))
                                                                                       (abs ,rest_marks (app
                                                                                                         (abs ,k (app (var ,k) (abs p (app (app (var p) (abs x (abs y (var x)))) (abs ,k (abs ,m (app (var ,k) (abs p ,(substitute-app (substitute-app `(abs ,k (abs ,m (app (var ,k) (var p)))) `(abs ,k (abs ,m (app (var ,k) (var ,mark))))) `(var ,rest_marks))))))))))
                                                                                                         (abs ,n (app (app ,(transform-inner (third wcm)) (var ,k)) (var ,n)))))))))) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,marks))))))))))))

  
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
