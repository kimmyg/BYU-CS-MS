#lang racket

(define (substitute-app e1 e2)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (s (gensym 's))
        (n (gensym 'n))
        (e (gensym 'e))
        (f (gensym 'f)))
    `(λ (,k) (λ (,m) ((λ (,k) (,k ((λ (p) (p (λ (x) (λ (y) y)))) ,m)))
                      (λ (,s) ((λ (,k) (,k (λ (z) ((z (λ (x) (λ (y) y))) ,s))))
                               (λ (,n) ((,e1
                                         (λ (,e) ((,e2
                                                   (λ (,f) (((,e ,f) ,k) ,m)))
                                                  ,n)))
                                        ,n)))))))))

(define (transform e)
  (let ((k (gensym 'k))
        (m (gensym 'm))
        (s (gensym 's))
        (u (gensym 'u))
        (v (gensym 'v))
        (t (gensym 't))
        (f (gensym 'f))
        (a (gensym 'a)))
    (match e
      [(list 'ccm)
       `(λ (,k) (λ (,m) (((,m (λ (x) (λ (y) y))) ,k) (λ (z) z))))]
      [(list 'wcm e1 e2)
       `(λ (,k) (λ (,m) ((λ (,k) (,k ((λ (p) (p (λ (x) (λ (y) y)))) ,m)))
                         (λ (,s) ((λ (,k) (,k (λ (z) ((z (λ (x) (λ (y) y))) ,s))))
                                  (λ (,u) ((,(transform e1)
                                            (λ (,v) ((λ (,k) (,k ((λ (p) (p (λ (x) (λ (y) x)))) ,m)))
                                                     (λ (,f) ((λ (,k) (,k ((((,f (((,s (λ (x) x)) (λ (z) z)) (λ (x) (λ (k) (λ (m) (k (λ (y) (λ (k) (λ (m) (k y)))))))))) ,s) (λ (v) (λ (,k) (λ (,m) (,k v))))) (λ (z) z))))
                                                              (λ (,a) ((λ (,k) (,k (λ (z) ((z (λ (x) (λ (y) x))) (λ (,k) (λ (,m) (,k (λ (z) ,(substitute-app (substitute-app `(λ (,k) (λ (,m) (,k z))) `(λ (,k) (λ (,m) (,k ,v)))) a)))))))))
                                                                       (λ (,m) ((,(transform e2)
                                                                                 ,k)
                                                                                ,m))))))))) ,u)))))))]
[(list 'λ (list x1) e1)
 `(λ (,k) (λ (,m) (,k (λ (,x1) ,(transform e1)))))]
[(list e1 e2)
 (substitute-app (transform e1) (transform e2))]
['error
 'error]
['ope
 'Ce]
['opf
 'Cf]
[x1
 `(λ (,k) (λ (,m) (,k ,x1)))])))

(define (init e)
  (let ((k (gensym 'k))
        (m (gensym 'm)))
    `((,e (λ (v) (λ (,k) (λ (,m) (,k v))))) (λ (p) ((p (λ (x) (λ (y) y))) ,(transform '(λ (x) (λ (y) y))))))))

(provide transform
         init)
