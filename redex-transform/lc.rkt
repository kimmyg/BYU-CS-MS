#lang racket
(require redex)

(define-language λv
  (e (e e) x v)
  (x variable-not-otherwise-mentioned)
  (v number (λ (x) e))
  (E (E e) (v E) hole))

(define λv-rr
  (reduction-relation
   λv
   (--> (in-hole E ((λ (x) e) v))
        (in-hole E (subst-n (x v) e))
        "βv")
   (--> (in-hole E ((λ (x_1) e) x_2))
        (in-hole E (subst-n (x_1 x_2) e))
        "βv-v")))

(define-metafunction λv
  subst-n : (x any) ... any -> any
  [(subst-n (x_1 any_1) (x_2 any_2) ... any_3)
   (subst x_1 any_1 (subst-n (x_2 any_2) ... any_3))]
  [(subst-n any_3) any_3])

(define-metafunction λv
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

(define-metafunction λv
  subst-vars : (x any) ... any -> any
  [(subst-vars (x_1 any_1) x_1) any_1]
  [(subst-vars (x_1 any_1) (any_2 ...)) 
   ((subst-vars (x_1 any_1) any_2) ...)]
  [(subst-vars (x_1 any_1) any_2) any_2]
  [(subst-vars (x_1 any_1) (x_2 any_2) ... any_3) 
   (subst-vars (x_1 any_1) 
               (subst-vars (x_2 any_2) ... any_3))]
  [(subst-vars any) any])

(provide λv
         λv-rr)
