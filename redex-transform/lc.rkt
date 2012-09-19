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
        "error in operand")
   (--> (in-hole E ((λ (x) e) v))
        (in-hole E (subst x v e))
        "βv")))

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
  subst : x v e -> e
  ;; 1. substitute in application
  [(subst x_1 v_1 (e_1 e_2))
   ((subst x_1 v_1 e_1) (subst x_1 v_1 e_2))]
  ;; 2a. substitute in variable (same)
  [(subst x_1 v_1 x_1)
   v_1]
  ;; 2b. substitute in variable (different)
  [(subst x_1 v_1 x_2)
   x_2]
  ;; 3a. substitute in abstraction (bound)
  [(subst x_1 v_1 (λ (x_1) e_1))
   (λ (x_1) e_1)]
  ;; 3b. substitute in abstraction (free)
  [(subst x_1 v_1 (λ (x_2) e_1))
   (λ (x_2) (subst x_1 v_1 e_1))]
   ;(λ (x_new) (subst x_1 v_1 (rename x_2 x_new e_1)))
   ;(where x_new ,(variable-not-in (term (x_2 v_1 e_1)) (term x_2)))]
  ;; 4. substitute in error
  [(subst x_1 v_1 error)
   error]
  [(subst x_1 v_1 number_1)
   number_1])

(provide λv
         λv-rr)
