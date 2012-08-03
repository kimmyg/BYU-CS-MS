#lang racket
(require redex
         racket/pretty
         "alpha.rkt"
         "transform.rkt"
         "lc.rkt"
         "cm.rkt"
         "tex.rkt")

(define (apply-reduction-relation*/tags R e)
  (let ((things (apply-reduction-relation/tag-with-names R e)))
    (match things
      [(list (list rule t))
       (begin
         (display rule)
         (newline)
         (lc->tex t)
         (newline)
         (cons (list rule e t)
               (apply-reduction-relation*/tags R t)))]
        [(list)
         (begin
           (display "done")
           (newline)
           (list (list #f e e)))])))

(define (transform-test P)
  (define P-ans-steps (apply-reduction-relation*/tags λcm-rr P))
  (define P-ans (third (last P-ans-steps)))
  (define C.P-ans (transform P-ans))
  (define C-steps (apply-reduction-relation*/tags λv-rr (init (transform P))))
  (define C.ans (third (last C-steps)))
  #t)
  ;(displayln (alpha-eq? C.P-ans C.ans))
  #;(pretty-print P-ans-steps)
  ;(lc->tex P-ans)
  ;(newline)
  ;(lc->tex (term->first-alpha C.P-ans))
  ;(newline)
  ;(pretty-print C-steps)
  ;(pretty-print (term->first-alpha C.ans)))

#;(transform-test 'x)
#;(transform-test '(x e_2))
(transform-test '(wcm 0 1))