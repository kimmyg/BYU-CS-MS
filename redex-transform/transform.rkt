#lang racket

(define (substitute-app e1 e2)
  `(λ (k) (λ (m) ((λ (k) (k (m (λ (x) (λ (y) y)))))
                  (λ (s) ((λ (k) (k (λ (p) ((p (λ (x) (λ (y) y))) s))))
                          (λ (n) ((,e1
                                   (λ (e) ((,e2
                                            (λ (f) (((e f) k) m)))
                                           n)))
                                  n))))))))
      
(define (transform e)
  (match e
    [(list 'ccm)
     `(λ (k) (λ (m) (((m (λ (x) (λ (y) y))) k) (λ (z) z))))]
    [(list 'wcm e1 e2)
     `(λ (k) (λ (m) ((λ (k) (k (m (λ (x) (λ (y) y)))))
                     (λ (s) ((λ (k) (k (λ (p) ((p (λ (x) (λ (y) y))) s))))
                             (λ (u) ((,(transform e1)
                                      (λ (v) ((λ (k) (k (((((s (λ (x) x)) (λ (z) z)) (λ (x) (λ (k) (λ (m) (k (λ (y) (λ (k) (λ (m) (k y))))))))) (λ (v) (λ (k) (λ (m) (k v))))) (λ (z) z))))
                                              (λ (t) ((λ (k) (k (m (λ (x) (λ (y) x)))))
                                                      (λ (f) ((λ (k) (k ((f t) s)))
                                                              (λ (a) ((λ (k) (k (λ (p) ((p (λ (x) (λ (y) x))) (λ (k) (λ (m) (k (λ (p) ,(substitute-app (substitute-app '(λ (k) (λ (m) (k p))) '(λ (k) (λ (m) (k v)))) 'a)))))))))
                                                                      (λ (m) ((,(transform e2)
                                                                               k)
                                                                              m)))))))))))
                                     u)))))))]
    [(list 'wcm1 e1 e2)
     `(λ (k) (λ (m) ((λ (k) (k (m (λ (x) (λ (y) y)))))
                     (λ (s) ((λ (k) (k (λ (p) ((p (λ (x) (λ (y) y))) s))))
                             (λ (u) ((,(transform e1)
                                      (λ (v) ((λ (k) (k (((((s (λ (x) x)) z) (λ (x) (λ (k) (λ (m) (k (λ (y) (λ (k) (λ (m) (k y)))))))))))
                                              (λ (t) t)))) (λ (z) z))))))))))]
    [(list 'λ (list x1) e1)
     `(λ (k) (λ (m) (k (λ (,x1) ,(transform e1)))))]
    [(list e1 e2)
     (substitute-app (transform e1) (transform e2))]
    ['error
     'error]
    [x1
     `(λ (k) (λ (m) (k ,x1)))]))

(define (init e)
  `((,e (λ (v) (λ (k) (λ (m) (k v))))) (λ (p) ((p (λ (x) (λ (y) y))) ,(transform '(λ (x) (λ (y) y)))))))

(provide transform
         init)
