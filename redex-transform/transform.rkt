#lang racket

;; paired flag and marks
#;(define (transform e)
    (let ([k (gensym 'k)]
          [m (gensym 'm)]
          [n (gensym 'n)]
          [s (gensym 's)]
          [t (gensym 't)]
          [r (gensym 'r)]
          [a (gensym 'a)]
          [b (gensym 'b)]
          [f (gensym 'f)])
      (match e
        [(list 'ccm)
         `(λ (,k) (λ (,m) (((,m (λ (x) (λ (y) y))) ,k) (λ (z) z))))]
        [(list 'wcm e0 e1)
         `(λ (,k)
            (λ (,m)
              ((λ (,k) (,k (,m (λ (x) (λ (y) y)))))
               (λ (,s) ((,(transform e0)
                         (λ (,f) ((λ (,k) (,k ((,s (λ (x) x)) (λ (z) z))))
                                  (λ (,t) ((λ (,k) (,k (((,m (λ (x) (λ (y) x))) (((,t (λ (x) (λ (k) (λ (m) (k (λ (y) (λ (k) (λ (m) (k y))))))))) (λ (x) x)) (λ (z) z))) ,t)))
                                           (λ (,r) ((,(transform e1) ,k) (λ (z) ((z (λ (x) (λ (y) x))) ,(transform `(λ (z) ((z ,f) ,r))))))))))))
                        ((λ (b) (λ (z) ((z (λ (x) (λ (y) y))) b))) ,s))))))]
        [(list 'λ (list x1) e1)
         `(λ (,k) (λ (,m) (,k (λ (,x1) ,(transform e1)))))]
        [(list e0 e1)
         `(λ (,k) (λ (,m) ((λ (,k) (,k ((λ (b) (λ (z) ((z (λ (x) (λ (y) y))) b))) (,m (λ (x) (λ (y) y))))))
                           (λ (,n) ((,(transform e0)
                                     (λ (,a) ((,(transform e1)
                                               (λ (,b) (((,a ,b) ,k) ,m)))
                                              ,n)))
                                    ,n)))))]
        ['error
         'error]
        [x1
         `(λ (,k) (λ (,m) (,k ,x1)))])))

;; separate flag and marks
#;(define (transform e)
  (let ([k (gensym 'k)]
        [m (gensym 'm)]
        [n (gensym 'n)]
        [s (gensym 's)]
        [t (gensym 't)]
        [r (gensym 'r)]
        [a (gensym 'a)]
        [b (gensym 'b)]
        [f (gensym 'f)])
    (match e
      [(list 'ccm)
       `(λ (,k) (λ (,f) (λ (,m) (((,m ,k) (λ (z) z)) (λ (z) z)))))]
      [(list 'wcm e0 e1)
       `(λ (,k)
          (λ (,f)
            (λ (,m)
              (((,(transform e0)
                 (λ (,n) ((λ (,k) (,k (((,m (λ (x) x)) (λ (z) z)) (λ (z) z))))
                          (λ (,t) ((λ (,k) (,k ((,f ((((,t (λ (x) (λ (k) (λ (f) (λ (m) (k (λ (y) (λ (k) (λ (f) (λ (m) (k y))))))))))) (λ (x) x)) (λ (z) z)) (λ (z) z))) ,t)))
                                   (λ (,r) 
                                     (((,(transform e1)
                                        ,k)
                                       (λ (x) (λ (y) x)))
                                      ,(transform `(λ (z) ((z ,n) ,r))))))))))
                (λ (x) (λ (y) y)))
               ,m))))]
      [(list 'λ (list x0) e0)
       `(λ (,k) (λ (,f) (λ (,m) (,k (λ (,x0) ,(transform e0))))))]
      [(list e0 e1)
       `(λ (,k)
          (λ (,f)
            (λ (,m)
              (((,(transform e0)
                 (λ (,a)
                   (((,(transform e1)
                      (λ (,b)
                        ((((,a ,b) ,k) ,f) ,m)))
                     (λ (x) (λ (y) y)))
                    ,m)))
                (λ (x) (λ (y) y)))
               ,m))))]
      ['error
       'error]
      [x0
       `(λ (,k) (λ (,f) (λ (,m) (,k ,x0))))])))


;; no continuation
(define (transform e)
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
                  (((((,m (λ (z) z)) (λ (z) z)) (λ (x) (λ (,f) (λ (,m) (λ (y) (λ (,f) (λ (,m) y))))))) (λ (z) z)) (λ (z) z)))
                 ((,m (λ (z) z)) (λ (z) z)))))))]
        [(list 'λ (list x0) e0)
         `(λ (,f) (λ (,m) (λ (,x0) ,(transform e0))))]
        [(list e0 e1)
         `(λ (,f)
            (λ (,m)
              (((((,(transform e0) (λ (x) (λ (y) y))) ,m)
                 ((,(transform e1) (λ (x) (λ (y) y))) ,m))
                ,f)
               ,m)))]
        ['error
         'error]
        [x0
         `(λ (,f) (λ (,m) ,x0))])))

;; paired flag and marks
#;(define (init e)
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
(define (init e)
    (let ([f (gensym 'f)]
          [m (gensym 'm)])
      `((λ (v) (λ (,f) (λ (,m) v))) ((,e (λ (x) (λ (y) y))) ,(transform '(λ (x) (λ (y) y)))))))

(provide transform
         init)
