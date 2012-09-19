#lang racket
(require redex)

(define-language λv
  (e (e e) x v error)
  (x variable-not-otherwise-mentioned)
  (v (λ (x) e) number)
  (E (E e) (v E) hole))

(define λv-rr
  (reduction-relation
   λv
   (--> (in-hole E ((λ (x) e) v))
        (in-hole E (λv-subst x v e))
        "βv")
   (--> (in-hole E x)
        (in-hole E error)
        "error: unbound identifier")
   (--> (in-hole E (number v))
        (in-hole E error)
        "number in operator position")
   (--> (in-hole E (error e))
        (in-hole E error)
        "error in operator")
   (--> (in-hole E (v error))
        (in-hole E error)
        "error in operand")))

#;(define-metafunction λv
  rename : x x e -> e
  ;; 1. application
  [(rename x_1 x_2 (e_1 e_2))
   ((rename x_1 x_2 e_1) (rename x_1 x_2 e_2))]
  ;; 2a. variable -- same
  [(rename x_1 x_2 x_1)
   x_2]
  ;; 2b. variable -- different
  [(rename x_1 x_2 x_3)
   x_3]
  ;; 3a. abstration -- same
  [(rename x_1 x_2 (λ (x_1) e_1))
   (λ (x_1) e_1)]
  ;; 3b. abstraction -- different 
  [(rename x_1 x_2 (λ (x_3) e_1))
   (λ (x_3) (rename x_1 x_2 e_1))]
  ;; 4. error
  [(rename x_1 x_2 error)
   error]
  [(rename x_1 x_2 number_1)
   number_1])

(define-metafunction λv
  λv-subst : x v e -> e
  ;; 1. x_1 bound, so don't continue in λ body
  [(λv-subst x_0 v_0 (λ (x_0) e_0))
   (λ (x_0) e_0)]
  ;; 2. descend into abstraction
  [(λv-subst x_0 v_0 (λ (x_1) e_0))
   (λ (x_1) (λv-subst x_0 v_0 e_0))]
  
  [(λv-subst x_0 e_0 error)
   error]
  
  [(λv-subst x_0 e_0 number_1)
   number_1]
  ;; 3. substitute in application
  [(λv-subst x_0 v_0 (e_0 e_1))
   ((λv-subst x_0 v_0 e_0) (λv-subst x_0 v_0 e_1))]
  
  [(λv-subst x_0 v_0 x_0)
   v_0]
  
  [(λv-subst x_0 v_0 x_1)
   x_1])

(provide λv
         λv-rr
         λv-subst)
