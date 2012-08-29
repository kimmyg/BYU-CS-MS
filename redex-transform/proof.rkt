#lang racket
(require redex
         racket/pretty
         "alpha.rkt"
         "transform.rkt"
         "lc.rkt"
         "cm.rkt"
         "tex.rkt")

;(define p '(wcm ph (ccm)))
;(traces λcm-rr p)
;(traces λv-rr (init (transform p)))

(define (apply-reduction-relation*/tags R e)
  (let ((things (apply-reduction-relation/tag-with-names R e)))
    (match things
      [(list (list rule t))
       (cons (list rule e t)
             (apply-reduction-relation*/tags R t))]
      [(list)
       (list #f)])))

(define (transform-test P)
  (let ((P-steps (apply-reduction-relation*/tags λcm-rr P))
        (C.P (init (transform P))))
    (let ((C.P-steps (apply-reduction-relation*/tags λv-rr C.P)))
      (define display-step (λ (step)
                             (if step
                                 (begin
                                   (display (first step))
                                   (newline)
                                   (cm->tex (third step)))
                                 (begin
                                   (display "done")
                                   (newline)))))
      (cm->tex P)
      (map display-step P-steps)
      (cm->tex C.P)
      (map display-step C.P-steps))))

(transform-test 'x)
#;(transform-test '(x e_2))
#;(transform-test '(wcm 0 1))

(define (list-replace l x y)
  (map (λ (z)
         (if (eq? z x)
             y
             z))
       l))

#;(for* ((E '((wcm ph E) (E e) (ph E) (wcm E ph)))
         (e '((ph ph) (wcm ph ph) (ccm))))
    (display E)
    (newline)
    (display e)
    (newline)
    (let ((t (list-replace E 'E e)))
      (transform-test t)))