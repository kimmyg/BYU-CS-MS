#lang racket

;; paired flag and marks
(define (substitute-app e0 e1)
    (let ((k (gensym 'k))
          (m (gensym 'm))
          (s (gensym 's))
          (n (gensym 'n))
          (e (gensym 'e))
          (f (gensym 'f)))
      `(λ (,k) (λ (,m) ((λ (,k) (,k ((λ (z) (z (λ (x) (λ (y) y)))) ,m)))
                        (λ (,s) ((λ (,k) (,k (λ (z) ((z (λ (x) (λ (y) y))) ,s))))
                                 (λ (,n) ((,e0
                                           (λ (,e) ((,e1
                                                     (λ (,f) (((,e ,f) ,k) ,m)))
                                                    ,n)))
                                          ,n)))))))))

;; separate flag and marks
#;(define (substitute-app e0 e1)
    (let ([k (gensym 'k)]
          [f (gensym 'f)]
          [m (gensym 'm)]
          [a (gensym 'a)]
          [b (gensym 'b)])
      `(λ (,k)
         (λ (,f)
           (λ (,m)
             (((,e0
                (λ (,a)
                  (((,e1
                     (λ (,b)
                       ((((,a ,b) ,k) ,f) ,m)))
                    (λ (x) (λ (y) y)))
                   ,m)))
               (λ (x) (λ (y) y)))
              ,m))))))


;; no continuation
#;(define (substitute-app e0 e1)
  (let ([k (gensym 'k)]
        [f (gensym 'f)]
        [m (gensym 'm)]
        [a (gensym 'a)]
        [b (gensym 'b)])
    `(λ (,f)
       (λ (,m)
         (((((,e0 (λ (x) (λ (y) y))) ,m)
            ((,e1 (λ (x) (λ (y) y))) ,m))
           ,f)
          ,m)))))


;; paired flag and marks
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
         `(λ (,k) (λ (,m) ((λ (,k) (,k (,m (λ (x) (λ (y) y)))))
                           (λ (,s) ((λ (,k) (,k (((((,s (λ (x) x)) (λ (z) z)) (λ (x) (λ (k) (λ (m) (k (λ (y) (λ (k) (λ (m) (k y))))))))) (λ (,v) (λ (,k) (λ (,m) (,k ,v))))) (λ (z) z))))
                                    (λ (,t) ((λ (,k) (,k (,m (λ (x) (λ (y) x)))))
                                             (λ (,f) ((λ (,k) (,k ((,f ,t) ,s)))
                                                      (λ (,a) ((λ (,k) (,k (λ (z) ((z (λ (x) (λ (y) y))) ,s))))
                                                               (λ (,u) ((,(transform e1)
                                                                         (λ (,v) ((λ (,k) (,k (λ (z) ((z (λ (x) (λ (y) x))) (λ (,k) (λ (,m) (,k (λ (z) ,(substitute-app (transform `(z ,v)) a)))))))))
                                                                                  (λ (,m) ((,(transform e2)
                                                                                            ,k)
                                                                                           ,m)))))
                                                                        ,u)))))))))))))]
        [(list 'λ (list x1) e1)
         `(λ (,k) (λ (,m) (,k (λ (,x1) ,(transform e1)))))]
        [(list e1 e2)
         (substitute-app (transform e1) (transform e2))]
        ['error
         'error]
        [x1
         `(λ (,k) (λ (,m) (,k ,x1)))])))

;; separate flag and marks
#;(define (transform e)
    (let ((k (gensym 'k))
          (f (gensym 'f))
          (m (gensym 'm))
          (v (gensym 'v))
          (t (gensym 't)))
      (match e
        [(list 'ccm)
         `(λ (,k) (λ (,f) (λ (,m) (((,m ,k) (λ (z) z)) (λ (z) z)))))]
        [(list 'wcm e0 e1)
         `(λ (,k) (λ (,f) (λ (,m) (((,(transform e0)
                                     (λ (,v) ((λ (,k) (,k ((,f (((((((,m (λ (x) x)) (λ (z) z)) (λ (z) z)) (λ (x) (λ (k) (λ (f) (λ (m) (k (λ (y) (λ (k) (λ (f) (λ (m) (k y))))))))))) (λ (,v) (λ (,k) (λ (,f) (λ (,m) (,k ,v)))))) (λ (z) z)) (λ (z) z))) ,m)))
                                              (λ (,t) (((,(transform e1)
                                                         ,k)
                                                        (λ (x) (λ (y) x)))
                                                       (λ (,k) (λ (,f) (λ (,m) (,k (λ (z) ,(substitute-app (transform `(z ,v)) t)))))))))))
                                    (λ (x) (λ (y) y)))
                                   ,m))))]
        [(list 'λ (list x0) e0)
         `(λ (,k) (λ (,f) (λ (,m) (,k (λ (,x0) ,(transform e0))))))]
        [(list e0 e1)
         (substitute-app (transform e0) (transform e1))]
        ['error
         'error]
        [x0
         `(λ (,k) (λ (,f) (λ (,m) (,k ,x0))))])))


;; no continuation
#;(define (transform e)
  (let ([f (gensym 'f)]
        [m (gensym 'm)]
        [a (gensym 'a)]
        [b (gensym 'b)])
    (match e
      [(list 'ccm)
       `(λ (,f) (λ (,m) ((,m (λ (z) z)) (λ (z) z))))]
      [(list 'wcm e0 e1)
       `(λ (,f)
          (λ (,m)
            ((,(transform e1)
              (λ (x) (λ (y) x)))
             (((λ (,a) (λ (,b) ,(transform `(λ (z) ((z ,a) ,b)))))
               ((,(transform e0) (λ (x) (λ (y) y))) ,m))
              ((,f
                (((((,m (λ (z) z)) (λ (z) z)) (λ (x) (λ (,f) (λ (,m) (λ (y) (λ (,f) (λ (,m) y))))))) (λ (q1) q1)) (λ (q2) q2)))
               ((,m (λ (z) z)) (λ (z) z)))))))]
      [(list 'λ (list x0) e0)
       `(λ (,f) (λ (,m) (λ (,x0) ,(transform e0))))]
      [(list e0 e1)
       (substitute-app (transform e0) (transform e1))]
      ['error
       'error]
      [x0
       `(λ (,f) (λ (,m) ,x0))])))

;; paired flag and marks
(define (init e)
    (let ([k (gensym 'k)]
          [m (gensym 'm)])
      `((,e (λ (v) (λ (,k) (λ (,m) (,k v))))) (λ (z) ((z (λ (x) (λ (y) y))) ,(transform '(λ (x) (λ (y) y))))))))


;; separate flag and marks
#;(define (init e)
    (let ([k (gensym 'k)]
          [f (gensym 'f)]
          [m (gensym 'm)])
      `(((,e (λ (v) (λ (,k) (λ (,f) (λ (,m) (,k v)))))) (λ (x) (λ (y) y))) ,(transform '(λ (x) (λ (y) y))))))

;; no continuation
#;(define (init e)
  (let ([f (gensym 'f)]
        [m (gensym 'm)])
    `((λ (v) (λ (,f) (λ (,m) v))) ((,e (λ (x) (λ (y) y))) ,(transform '(λ (x) (λ (y) y)))))))

(provide transform
         init)
