#lang racket
(require redex
         "lc.rkt")

(define-extended-language λcm λv
  (e .... (wcm e e) (ccm))
  (E (wcm v F) F)
  (F (E e) (v E) (wcm E e) hole))

(define λcm-rr
  (extend-reduction-relation
   λv-rr
   λcm
   (--> (in-hole E ((λ (x) e) v))
        (in-hole E (λcm-subst x v e))
        "βv")
   (--> (in-hole E (wcm v_1 (wcm v_2 e)))
        (in-hole E (wcm v_2 e))
        "wcm-collapse")
   (--> (in-hole E (wcm v_1 v_2))
        (in-hole E v_2)
        "wcm")
   (--> (in-hole E (wcm error e))
        (in-hole E error)
        "error in wcm mark expression")
   (--> (in-hole E (wcm v error))
        (in-hole E error)
        "error in wcm body expression")
   (--> (in-hole E (ccm))
        (in-hole E (chi E (λ (x) (λ (y) y))))
        "chi")))

(define-metafunction λcm
  chi : E v -> v
  [(chi hole v_ms)      v_ms]
  [(chi (E e) v_ms)     (chi E v_ms)]
  [(chi (v E) v_ms)     (chi E v_ms)]
  [(chi (wcm E e) v_ms) (chi E v_ms)]
  [(chi (wcm v E) v_ms) (chi E (λ (z) ((z v) v_ms)))])

(define-metafunction/extension λv-subst λcm
  λcm-subst : x v e -> e
  [(λcm-subst x_0 v_0 (wcm e_0 e_1))
   (wcm (λcm-subst x_0 v_0 e_0) (λcm-subst x_0 v_0 e_1))]
  [(λcm-subst x_0 v_0 (ccm))
   (ccm)])

(provide λcm
         λcm-rr)
