#lang racket
(require redex)

(define-language λcm
  (e (e e) (wcm e e) (ccm) x v error)
  (x variable-not-otherwise-mentioned)
  (v (λ (x) e) number)
  (E (wcm v F) F)
  (F (E e) (v E) (wcm E e) hole))

(define λcm-rr
  (reduction-relation
   λcm
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
   
   #;(--> (in-hole E ((λ (x_1) e) x_2))
        (in-hole E (subst x_1 x_2 e))
        "βv-x")
   (--> (in-hole E (wcm v_1 v_2))
        (in-hole E v_2)
        "wcm")
   (--> (in-hole E (wcm v_1 (wcm v_2 e)))
        (in-hole E (wcm v_2 e))
        "tail")
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
  [(chi (wcm v E) v_ms) (chi E (λ (p) ((p v) v_ms)))])

(define-metafunction λcm
  subst : x v e -> e
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst x_1 v_1 (λ (x_1) e_1))
   (λ (x_1) e_1)]
  ;; 2. descend into abstraction
  [(subst x_1 v_1 (λ (x_2) e_1))
   (λ (x_2) (subst x_1 v_1 e_1))]
  
  [(subst x_1 v_1 error)
   error]
  
  [(subst x_1 v_1 number_1)
   number_1]
  
  ;; 3. substitute in application
  [(subst x_1 v_1 (e_1 e_2))
   ((subst x_1 v_1 e_1) (subst x_1 v_1 e_2))]
  
  [(subst x_1 v_1 x_1)
   v_1]
  
  [(subst x_1 v_1 x_2)
   x_2]

  [(subst x_1 v_1 (wcm e_1 e_2))
   (wcm (subst x_1 v_1 e_1) (subst x_1 v_1 e_2))]
  
  [(subst x_1 v_1 (ccm))
   (ccm)])

(provide λcm
         λcm-rr)
