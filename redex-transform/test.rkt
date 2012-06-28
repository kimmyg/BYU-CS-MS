#lang racket
(require redex)

(require "cm.rkt"
         "lc.rkt"
         "transform.rkt")


#;(define-metafunction λcm_l
  transform : any -> any
  [(transform x)
   ,(term-let ([k (variable-not-in (term x) 'k)]
               [m (variable-not-in (term x) 'm)])
              (term (λ (k) (λ (m) (k x)))))]
  [(transform (λ (x) e))
   ,(term-let ([k (variable-not-in (term (λ (x) e)) 'k)]
               [m (variable-not-in (term (λ (x) e)) 'm)])
              (term (λ (k) (λ (m) (k (transform e))))))]
  [(transform (e_1 e_2))
   ,(term-let ([k (variable-not-in (term (e_1 e_2)) 'k)]
               [m (variable-not-in (term (e_1 e_2)) 'm)]
               [e (variable-not-in (term (e_1 e_2)) 'e)]
               [f (variable-not-in (term (e_1 e_2)) 'f)]
               [snd_m (variable-not-in (term (e_1 e_2)) 'snd_m)])
              (λ (k) (λ (m) ((λ (k) (k (m (λ (x) (λ (y) y))))) (λ (snd_m) (((term (transform e_1)) (λ (e) (((term (transform e_2)) (λ (f) (((e f) k) m))) (λ (p) ((p (λ (x) (λ (y) y))) snd_m))))) (λ (p) ((p (λ (x) (λ (y) y))) snd_m))))))))]
  [(transform (wcm e_1 e_2))
   ,(term-let ([k (variable-not-in (term (wcm e_1 e_2)) 'k)]
               [m (variable-not-in (term (wcm e_1 e_2)) 'm)]
               [mark (variable-not-in (term (wcm e_1 e_2)) 'mark)]
               [fst_m (variable-not-in (term (wcm e_1 e_2)) 'fst_m)]
               [snd_m (variable-not-in (term (wcm e_1 e_2)) 'snd_m)]
               [tail_m (variable-not-in (term (wcm e_1 e_2)) 'tail_m)]
               [proper_tail_m (variable-not-in (term (wcm e_1 e_2)) 'proper_tail_m)])
              (λ (k) (λ (m) ((λ (k) (k (m (λ (x) (λ (y) y))))) (λ (snd_m) (((term (transform e_1)) (λ (mark) ((λ (k) (k (m (λ (x) (λ (y) x))))) (λ (fst_m) ((λ (k) (k ((fst_m snd_m) m)))) (λ (tail_m) ((λ (k) (k (tail_m (λ (x) (λ (y) y))))) (λ (proper_tail_m) ((λ (k) (k (λ (p) ((p (λ (x) (λ (y) x))) (λ (p) ((p mark) proper_tail_m)))))) (λ (n) (((term (transform e_2)) k) n)))))))))) (λ (p) ((p (λ (x) (λ (y) y))) snd_m))))))))]
  [(transform (ccm))
   (λ (k) (λ (m) (k (m (λ (x) (λ (y) y))))))]
  [(transform number)
   (λ (k) (λ (m) (k number)))])

(transform

(emit (parse '(λ (x) x)))