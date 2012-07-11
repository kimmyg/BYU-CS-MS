#lang racket
(require redex)

(define-language λcm
  (e (e f) (wcm e f) (ccm) v)
  (f e x number)
  (x variable-not-otherwise-mentioned)
  (u v x number)
  (v (λ (x) f))
  (E (wcm v F) F)
  (F (E f) (v E) (wcm E f) hole))

(define λcm-rr
  (reduction-relation
   λcm
   (--> (in-hole E ((λ (x) f) u))
        (in-hole E (subst-n (x u) f))
        "βu")
   (--> (in-hole E (wcm v u))
        (in-hole E u)
        "wcm")
   (--> (in-hole E (wcm v_1 (wcm v_2 e)))
        (in-hole E (wcm v_2 e))
        "tail")
   (--> (in-hole E (ccm))
        (in-hole E (chi E (λ (x) (λ (y) y))))
        "chi")))

(define-metafunction λcm
  chi : E v -> v
  [(chi hole v_ms)      v_ms]
  [(chi (E f) v_ms)     (chi E v_ms)]
  [(chi (v E) v_ms)     (chi E v_ms)]
  [(chi (wcm E f) v_ms) (chi E v_ms)]
  [(chi (wcm v E) v_ms) (chi E (λ (p) ((p v) v_ms)))])

(define-metafunction λcm
  subst-n : (x any) ... any -> any
  [(subst-n (x_1 any_1) (x_2 any_2) ... any_3)
   (subst x_1 any_1 (subst-n (x_2 any_2) ... any_3))]
  [(subst-n any_3) any_3])

(define-metafunction λcm
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

(define-metafunction λcm
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

(provide λcm
         λcm-rr)
