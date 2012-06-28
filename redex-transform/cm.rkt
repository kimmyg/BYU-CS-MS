#lang racket
(require redex)

(define-language λcm_l
  (e (e e) (wcm e e) (ccm) x v)
  (x variable-not-otherwise-mentioned)
  (v number (λ (x) e))
  (E (wcm v F) F)
  (F (E e) (v E) (wcm E e) hole))

(define λcm_l-rr
  (reduction-relation
   λcm_l
   (--> (in-hole E ((λ (x) e) v))
        (in-hole E (subst-n (x v) e))
        "βv")
   (--> (in-hole E ((λ (x_1) e) x_2))
        (in-hole E (subst-n (x_1 x_2) e))
        "βv-v")
   (--> (in-hole E (wcm v_1 v_2))
        (in-hole E v_2)
        "wcm")
   (--> (in-hole E (wcm v_1 (wcm v_2 e)))
        (in-hole E (wcm v_2 e))
        "tail")
   (--> (in-hole E (ccm))
        (in-hole E (chi E))
        "chi")))

(define-metafunction λcm_l
  chi : E -> lv
  [(chi hole)          (λ (x) (λ (y) y))]
  [(chi (E_1 e_1))     (chi E_1)]
  [(chi (v_1 E_1))     (chi E_1)]
  [(chi (wcm E_1 e_1)) (chi E_1)]
  [(chi (wcm v_1 E_1)) (λ (p) ((p v_1) (chi E_1)))])

(define-metafunction λcm_l
  subst : x any any -> any
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst x_1 any_1 (λ (x_2 ... x_1 x_3 ...) any_2))
   (λ (x_2 ... x_1 x_3 ...) any_2)
   (side-condition (not (member (term x_1)
                                (term (x_2 ...)))))]
  ;; 2. general purpose capture avoiding case
  [(subst x_1 any_1 (λ (x_2 ...) any_2))
   (λ (x_new ...) 
     (subst x_1 any_1
            (subst-vars (x_2 x_new) ... any_2)))
   (where (x_new ...) ,(variables-not-in (term (x_1 any_1 any_2)) 
                                         (term (x_2 ...))))]
  ;; 3. replace x_1 with e_1
  [(subst x_1 any_1 x_1) any_1]
  ;; 4. x_1 and x_2 are different, so don't replace
  [(subst x_1 any_1 x_2) x_2]
  ;; the last cases cover all other expressions
  [(subst x_1 any_1 (any_2 ...))
   ((subst x_1 any_1 any_2) ...)]
  [(subst x_1 any_1 any_2) any_2])

(define-metafunction λcm_l
  subst-vars : (x any) ... any -> any
  [(subst-vars (x_1 any_1) x_1) any_1]
  [(subst-vars (x_1 any_1) (any_2 ...)) 
   ((subst-vars (x_1 any_1) any_2) ...)]
  [(subst-vars (x_1 any_1) any_2) any_2]
  [(subst-vars (x_1 any_1) (x_2 any_2) ... any_3) 
   (subst-vars (x_1 any_1) 
               (subst-vars (x_2 any_2) ... any_3))]
  [(subst-vars any) any])


#|
(define-metafunction λcm-rr
  substitute x v e -> e
  ;; 1. variable that is the same
  [(substitute x_1 v_1 x_1)
   v_1]
  ;; 2. variable that is different
  [(substitute x_1 v_1 x_2)
   x_2]
  ;; 3. abstraction of same variable
  [(substitute x_1 v_1 (λ (x_1) e_1))
   (λ (x_1) e_1)]
  ;; 4. abstraction of different variable
  [(substitute x_1 v_1 (λ (x_2) e_1))
|#

(provide λcm_l
         λcm_l-rr)