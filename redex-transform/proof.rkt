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
       (cons (list rule e t)
             (apply-reduction-relation*/tags R t))]
      [(list)
       (list #f)])))

(define (apply-substitutions s substitutions)
  (if (empty? substitutions)
      s
      (apply-substitutions (string-replace s (car (first substitutions)) (cdr (first substitutions))) (rest substitutions))))

(define (transform-test P (M `(λ (z) ((z (λ (x) (λ (y) y))) ,(transform '(λ (x) (λ (y) y)))))))
  (let ((P-steps (apply-reduction-relation*/tags λcm-rr P))
        (C.P `((,(transform P) (λ (v) (λ (k) (λ (m) (k v))))) ,M)))
    (let ((C.P-steps (apply-reduction-relation*/tags λv-rr C.P)))
      (define format-step (λ (step)
                             (if step
                                 (format "~a~n~a~n" (first step) (cm->slatex (third step)))
                                 (format "done~n"))))
      (let ((P.tex (cm->slatex P))
            (P-steps.tex (foldl (λ (step acc) (string-append acc (format-step step))) "" P-steps))
            (C.P.tex (cm->slatex C.P))
            (C.P-steps.tex (foldl (λ (step acc) (string-append acc (format-step step))) "" C.P-steps)))
        (begin
          (printf "~a~n" P.tex)
          (newline)
          (display P-steps.tex)
          (printf "~a~n" C.P.tex)
          (newline)
          (display C.P-steps.tex))))))

#;(transform-test '((λ (x) e0) (λ (x) f1)) (list (cons "\\k.\\m.(k e)" "C[e0]")
                                               (cons "\\k.\\m.(k f)" "C[e1]")
                                               (cons "\\k.k" "k")
                                               (cons "\\f.f" "FLAG")
                                               (cons "\\x.\\y.y" "FALSE")
                                               (cons "\\m.m" "m")))
#;(transform-test '(x e_2))
(transform-test '(ph qh))
;(transform-test '(wcm 0 1) `(λ (z) ((z (λ (x) (λ (y) y))) ,(transform '(λ (x) (λ (y) y))))))

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