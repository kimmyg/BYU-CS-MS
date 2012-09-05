#lang racket
(require redex)

(define-language λv
  (e (e e) x v error Cop)
  (Cop Cope Copf) ; C[opaque]
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
        "βv")
   (--> (in-hole E ((Cop (λ (x) e)) v))
        (in-hole E (subst x Cop e)))))

#;(define-metafunction λv
  rename : x x e -> e
  ;; 1. x_1 bound, so don't continue in λ body
  [(rename x_1 x_2 (λ (x_1) e_1))
   (λ (x_1) e_1)]
  ;; 2. 
    )

(define-metafunction λv
  subst : x e e -> e
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst x_1 e_2 (λ (x_1) e_1))
   (λ (x_1) e_1)]
  ;; 2. descend into abstraction
  [(subst x_1 e_2 (λ (x_2) e_1))
   (λ (x_2) (subst x_1 e_2 e_1))]
  
  [(subst x_1 e_1 error)
   error]
  
  [(subst x_1 e_1 number_1)
   number_1]
  ;; 3. substitute in application
  [(subst x_1 e_3 (e_1 e_2))
   ((subst x_1 e_3 e_1) (subst x_1 e_3 e_2))]
  
  [(subst x_1 e_1 x_1)
   e_1]
  
  [(subst x_1 e_1 x_2)
   x_2]
  
  [(subst x_1 e_1 Cop_1)
   Cop_1])
  


(provide λv
         λv-rr)
