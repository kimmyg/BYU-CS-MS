#lang racket

T[(e_0 e_1)]
(lambda (k)
  (lambda (m)
    ((lambda (k) (k (SND m)))
     (lambda (s) ((lambda (k) (k ((PAIR FALSE) s)))
                  (lambda (n) ((e_0
                                (lambda (e) ((e_1
                                              (lambda (f) (((e f) k) m)))
                                             n))) n)))))))

C[(e_0 e_1)]
(lambda (k)
  (lambda (m)
    ((lambda (k) (k (SND m)))
     (lambda (s) ((lambda (k) (k ((PAIR FALSE) s)))
                  (lambda (n) ((Ce_0
                                (lambda (e) ((Ce_1
                                              (lambda (f) (((e f) k) m)))
                                             n))) n)))))))

C[(wcm e_0 e_1)]
(lambda (k)
  (lambda (m)
    ((lambda (k) (k (SND m)))
     (lambda (s) ((lambda (k) (k ((PAIR FALSE) s)))
                  (lambda (u) ((Ce_0
                                (lambda (v) ((lambda (k)
                                               (k (((((s (lambda (z) z))
                                                      (lambda (z) z))
                                                     (lambda (x) Cy.y))
                                                    (lambda (v) Cv))
                                                   (lambda (z) z))))
                                             (lambda (t)
                                               ((lambda (k) (k (FST m))
                                                  (lambda (f) ((lambda (k) (k ((f t) s)))
                                                               (lambda (a)
                                                                 ((lambda (k)
                                                                    (k ((PAIR TRUE)
                                                                        (lambda (k)
                                                                          (lambda (m)
                                                                            (k (lambda (z) T[(C[(z v)] a)])))))))
                                                                  (lambda (m) ((Ce_1 k) m))))))))))) u))))))))

C[(ccm)]
(lambda (k) (lambda (m) (((SND m) k) (lambda (z) z))))

C[(lambda (x) e_0)]
(lambda (k) (lambda (m) (k Ce_0)))

C[x]
(lambda (k) (lambda (m) (k x)))

C[error]
error


C[hole]
(lambda (v) (lambda (k) (lambda (m) (k v))))

C[(hole e_1)]
(lambda (e) ((Ce_1
              (lambda (f) (((e f) CE) ((PAIR xiE) chiE))))
             ((PAIR FALSE) chiE)))

C[(v_0 hole)]
(lambda (f) (((e f) CE) ((PAIR xiE) chiE)))

C[(wcm hole e_1)]
(lambda (v) ((lambda (k)
               (k (((((chiE (lambda (z) z))
                      (lambda (z) z))
                     (lambda (x) Cy.y))
                    (lambda (v) Cv))
                   (lambda (z) z))))
             (lambda (t)
               ((lambda (k) (k (FST ((PAIR xiE) chiE))))
                (lambda (f) ((lambda (k) (k ((f t) s)))
                             (lambda (a)
                               ((lambda (k)
                                  (k ((PAIR TRUE)
                                      (lambda (k)
                                        (lambda (m)
                                          (k (lambda (z) T[(C[(z v)] a)])))))))
                                (lambda (m) ((Ce_1 CE) m))))))))))

C[(wcm v_0 hole)]
CE
