#lang racket
(require redex
         racket/pretty
         "alpha.rkt"
         "transform.rkt"
         "lc.rkt"
         "cm.rkt")

(define (term->latex term)
  (match term
    [(list t_1 t_2)
     (format "(~a\\,~a)" (term->latex t_1) (term->latex t_2))]
    [(list λ (list x) e)
     (format "\\lambda ~a.~a" x (term->latex e))]
    [x
     (format "~a" x)]))

(define (apply-reduction-relation*/tags R e)
  (let ((things (apply-reduction-relation/tag-with-names R e)))
    ;(pretty-print things)
  (match things
    [(list (list rule t))
     (cons (list rule e t)
           (apply-reduction-relation*/tags R t))]
    [(list)
     (list (list #f e e))])))

(define (transform-test P)
  (define P-ans-steps (apply-reduction-relation*/tags λcm-rr P))
  (define P-ans (third (last P-ans-steps)))
  (define C.P-ans (transform P-ans))
  (define C-steps (apply-reduction-relation*/tags λv-rr (init (transform P))))
  (define C.ans (third (last C-steps)))
  
  (displayln (alpha-eq? C.P-ans C.ans))
  #;(pretty-print P-ans-steps)
  (display (term->latex P-ans))
  (newline)
  (display (term->latex (term->first-alpha C.P-ans)))
  (newline)
  (pretty-print C-steps)
  (pretty-print (term->first-alpha C.ans)))

#;(transform-test 'x)
#;(transform-test '(x e_2))
(transform-test '(e_1 x))