#lang racket

#;(require "cm-parse.rkt"
         "lc-emit.rkt")

#;(define (transform-var var)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k (abs ,m (app (var ,k) ,var)))))

#;(define (transform-abs abs)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `(abs ,k (abs ,m (app (var ,k) (abs ,(second abs) ,(transform-inner (third abs))))))))

#;(define (substitute-app rator rand)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (e (gensym 'e))
        (f (gensym 'f))
        (snd_m (gensym 'snd_m)))
    `(abs ,k (abs ,m (app
                      (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var y))))))
                      (abs ,snd_m (app
                                   (abs ,k (app (var ,k) (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,snd_m)))))
                                   (abs ,m (app (app ,rator (abs ,e (app (app ,rand (abs ,f (app (app (app (var ,e) (var ,f)) (var ,k)) (var ,m)))) (var ,m)))) (var ,m))))))))))


#;(define (transform-app app)
  (substitute-app (transform-inner (second app)) (transform-inner (third app))))

#;(define (transform-wcm wcm)
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
                                   (abs ,k (app (app (app (app (app (var ,marks) (abs x (var x))) (abs x (var x))) (abs x (abs ,k (abs ,m (app (var ,k) (abs y (abs ,k (abs ,m (app (var ,k) (var y)))))))))) (abs x (app (var ,k) (abs ,k (abs ,m (app (var ,k) (var x))))))) (abs p (app (app (var p) (var x)) (var y)))))
                                   ;(abs ,k (app (var ,k) (app (app (app (var ,marks) (abs x (var x))) (abs x (var x))) (abs x (abs y (var y))))))
                                   (abs ,tail_marks (app (app ,(transform-inner (second wcm))
                                                              (abs ,mark (app
                                                                          (abs ,k (app (var ,k) (app (var ,m) (abs x (abs y (var x)))))) ; (k (fst m))
                                                                          (abs ,flag (app
                                                                                       (abs ,k (app (var ,k) (app (app (var ,flag) (var ,tail_marks)) (var ,marks))))
                                                                                       (abs ,rest_marks (app
                                                                                                         (abs ,k (app (var ,k) (abs p (app (app (var p) (abs x (abs y (var x)))) (abs ,k (abs ,m (app (var ,k) (abs p ,(substitute-app (substitute-app `(abs ,k (abs ,m (app (var ,k) (var p)))) `(abs ,k (abs ,m (app (var ,k) (var ,mark))))) `(var ,rest_marks))))))))))
                                                                                                         (abs ,n (app (app ,(transform-inner (third wcm)) (var ,k)) (var ,n))))))))))
                                                         (abs p (app (app (var p) (abs x (abs y (var y)))) (var ,marks))))))))))))

  

      
(define (transform e)
  (match e
    [(list ccm)
     `(λ (k) (λ (m) (((m (λ (x) (λ (y) y))) k) z)))]
    [(list wcm e1 e2)
     `(λ (k) (λ (m) ((λ (k) (k (m (λ (x) (λ (y) x)))))
                     (λ (f) ((λ (k) (k ((λ (k) (m (λ (x) (λ (y) y)))))))
                             (λ (s) ((λ (k) (k (λ (p) ((p (λ (x) (λ (y) y))) s))))
                                     (λ (u) ((λ (k) (k ((((s (λ (x) x)) z) (λ (x) (λ (k) (λ (m) (k (λ (y) (λ (k) (λ (m) (k y))))))))))))
                                             (λ (t) ((,(transform e1)
                                                      (λ (v) ((λ (k) (k ((f t) s)))
                                                              (λ (s) ((λ (k) (k (λ (p) ((p (λ (x) (λ (y) x))) z)))) ;[z should be v paired with s]
                                                                      (λ (m) ((,(transform e2)
                                                                               k)
                                                                              m)))))))
                                                     u)))))))))))]
    [(list λ (list x1) e1)
     `(λ (k) (λ (m) (k (λ (,x1) ,(transform e1)))))]
    [(list e1 e2)
     `(λ (k) (λ (m) ((λ (k) (k (m (λ (x) (λ (y) y)))))
                     (λ (s) ((λ (k) (k (λ (p) ((p (λ (x) (λ (y) y))) s))))
                             (λ (n) ((,(transform e1)
                                      (λ (e) ((,(transform e2)
                                               (λ (f) (((e f) k) m)))
                                              n)))
                                     n)))))))]
    [x1
     `(λ (k) (λ (m) (k ,x1)))]))

(define (init e)
  `((λ (k) (k ((,e (λ (x) x)) (λ (p) ((p (λ (x) (λ (y) y))) ,(transform '(λ (x) (λ (y) y)))))))) (λ (v) (λ (k) (λ (m) (k v))))))

(provide transform
         init)
