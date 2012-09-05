#lang racket

(define (lc-length* e)
  (match e
    [(list 'λ (list x1) e1)
     (+ 1 (lc-length* e1))]
    [(list e1 e2)
     (+ (lc-length* e1) (lc-length* e2))]
    [x1
     1]))

(define (lc->tex* e)
  (define (lc->tex-inner e i)
    (match e
      [(list 'λ (list x1) e1)
       (begin
         (printf "$\\lambda ~c.$" (string-ref (symbol->string x1) 0))
         (lc->tex-inner e1 i))]
      [(list e1 e2)
       (if (and (> (lc-length* e1) 3) (> (lc-length* e2) 3))
           (begin
             (printf "(\\=")
             (lc->tex-inner e1 (+ i 1))
             (printf "\\\\")
             (newline)
             (for ([j i])
               (printf "\\>"))
             (lc->tex-inner e2 (+ i 1))
             (printf ")"))
           (begin
             (printf "(")
             (lc->tex-inner e1 (+ i 1))
             (printf "\\,")
             (lc->tex-inner e2 (+ i 1))
             (printf ")")))]
      [(? symbol? x1)
       (printf "$~c$" (string-ref (symbol->string x1) 0))]
      [x1
       (printf "~a" x1)]))
  (display "\\begin{tabbing}")
  (newline)
  (lc->tex-inner e 0)
  (newline)
  (display "\\end{tabbing}")
  (newline))

(define (cm-length e)
  (match e
    [(list 'wcm e1 e2)
     (+ 5 (cm-length e1) (cm-length e2))]
    [(list 'ccm)
     5]
    [(list 'λ (list x1) e1)
     (+ 3 (cm-length e1))]
    [(list e1 e2)
     (+ 3 (cm-length e1) (cm-length e2))]
    [x1
     1]))

(define (should-split e1 e2)
  (and (> (cm-length e1) 6) (> (cm-length e2) 6)))

(define (cm->verbatim e)
  (define (cm->verbatim-inner e i)
    (match e
      [(list 'wcm e0 e1)
       (if (should-split e0 e1)
           (format "(wcm ~a~n~a~a)"
                   (cm->verbatim-inner e0 (+ i 5))
                   (make-string (+ i 5) #\space)
                   (cm->verbatim-inner e1 (+ i 5)))
           (format "(wcm ~a ~a)"
                   (cm->verbatim-inner e0 (+ i 5))
                   (cm->verbatim-inner e1 (+ i 5 (cm-length e0)))))]
      [(list 'ccm)
       "(ccm)"]
      [(list 'λ (list x0) e0)
       (format "\\~c.~a"
               (string-ref (symbol->string x0) 0)
               (cm->verbatim-inner e0 (+ i 3)))]
      [(list e0 e1)
       (if (should-split e0 e1)
           (format "(~a~n~a~a)"
                   (cm->verbatim-inner e0 (+ i 1))
                   (make-string (+ i 1) #\space)
                   (cm->verbatim-inner e1 (+ i 1)))
           (format "(~a ~a)"
                   (cm->verbatim-inner e0 (+ i 1))
                   (cm->verbatim-inner e1 (+ i 1 (cm-length e0) 1))))]
      [(? symbol? x0)
       (format "~c" (string-ref (symbol->string x0) 0))]
      [x0
       (format "~a" x0)]))
  (format
   "{{{~n\\renewcommand{\\baselinestretch}{0.5}~n\\begin{verbatim}~n~a~n\\end{verbatim}~n}}}"
   (cm->verbatim-inner e 0)))

(define (cm->tex-ams e)
  (define (cm->tex-ams-inner e)
    (match e
      [(list e0 e1)
       (format "(~a\\,~a)" (cm->tex-ams-inner e0) (cm->tex-ams-inner e1))]
      [(list 'wcm e0 e1)
       (format "(\\mathrm{wcm}\\,~a\\,~a)" (cm->tex-ams-inner e0) (cm->tex-ams-inner e1))]
      [(list 'ccm)
       "(\\mathrm{ccm})"]
      [(list 'λ (list x0) e0)
       (format "\\lambda ~a.~a" (cm->tex-ams-inner x0) (cm->tex-ams-inner e0))]
      [(? symbol? x0)
       (if (eq? x0 'error)
           "\\mathrm{error}"
           (format "~c" (string-ref (symbol->string x0) 0)))]
      [x0
       (format "~a" x0)]))
  (cm->tex-ams-inner e))

(define (cm->slatex e)
  (define (cm-slatex-length e)
    (match e
      [(list 'wcm e0 e1)
       (+ 5 (cm-slatex-length e0) 1 (cm-slatex-length e1) 1)]
      [(list 'ccm)
       5]
      ['(λ (p) (p (λ (x) (λ (y) x))))
       3]
      ['(λ (p) (p (λ (x) (λ (y) x))))
       3]
      ['(λ (x) (λ (y) x))
       4]
      ['(λ (x) (λ (y) y))
       5]
      [(list 'λ (list k0) (list 'λ (list m0)
                               (list k0 (list 'λ (list 'x) (list 'λ (list k1) (list 'λ (list m1)
                                                                                  (list k1 (list 'λ (list 'y) (list 'λ (list k2) (list 'λ (list m2)
                                                                                                                                       (list k2 'y)))))))))))
       5]
      [(list 'λ (list 'z) (list (list 'z v0) v1))
       (+ 6 (cm-slatex-length v0) 1 (cm-slatex-length v1))]
      [(list 'λ (list x0) e0)
       (+ 4 (cm-slatex-length x0) 2 (cm-slatex-length e0) 1)]
      [(list e0 e1)
       (+ 1 (cm-slatex-length e0) 1 (cm-slatex-length e1))]
      [(? symbol? x0)
       (if (eq? x0 'error)
           5
           1)]
      [x0
       (cm-slatex-length x0)]))
  (define (cm-slatex-should-split e0 e1)
    (and (> (cm-slatex-length e0) 4) (> (cm-slatex-length e1) 4)))
  (define (cm->slatex-inner e i)
    (match e
      [(list 'wcm e0 e1)
       (if (cm-slatex-should-split e0 e1)
           (format "(wcm ~a~n~a~a)"
                   (cm->slatex-inner e0 (+ i 5))
                   (make-string (+ i 5) #\space)
                   (cm->slatex-inner e1 (+ i 5)))
           (format "(wcm ~a ~a)"
                   (cm->slatex-inner e0 (+ i 5))
                   (cm->slatex-inner e1 (+ i 5 (cm-length e0)))))]
      [(list 'ccm)
       "(ccm)"]
      ['(λ (p) (p (λ (x) (λ (y) x))))
       "FST"]
      ['(λ (p) (p (λ (x) (λ (y) x))))
       "SND"]
      ['(λ (x) (λ (y) x))
       "TRUE"]
      ['(λ (x) (λ (y) y))
       "FALSE"]
      [(list 'λ (list k0) (list 'λ (list m0)
                               (list k0 (list 'λ (list 'x) (list 'λ (list k1) (list 'λ (list m1)
                                                                                  (list k1 (list 'λ (list 'y) (list 'λ (list k2) (list 'λ (list m2)
                                                                                                                                       (list k2 'y)))))))))))
       "C[NIL]"]
      [(list 'λ (list 'z) (list (list 'z v0) v1))
       (format "(PAIR ~a ~a)" (cm->slatex-inner v0 (+ i 6)) (cm->slatex-inner v1 (+ i 6 (cm-slatex-length v0) 1)))]
      [(list 'λ (list x0) e0)
       (if (> (cm-slatex-length e0) 3)
           (format "(lambda (~c)~n~a~a)"
                   (string-ref (symbol->string x0) 0)
                   (make-string (+ i 2) #\space)
                   (cm->slatex-inner e0 (+ i 2)))
                   
           (format "(lambda (~c) ~a)"
                   (string-ref (symbol->string x0) 0)
                   (cm->slatex-inner e0 (+ i 6))))]
      [(list e0 e1)
       (if (should-split e0 e1)
           (format "(~a~n~a~a)"
                   (cm->slatex-inner e0 (+ i 1))
                   (make-string (+ i 1) #\space)
                   (cm->slatex-inner e1 (+ i 1)))
           (format "(~a ~a)"
                   (cm->slatex-inner e0 (+ i 1))
                   (cm->slatex-inner e1 (+ i 1 (cm-slatex-length e0) 1))))]
      [(? symbol? x0)
       (cond
         ((eq? x0 'error) "error")
         ((eq? x0 'Cope) "Ce")
         ((eq? x0 'Copf) "Cf")
         (else (format "~c" (string-ref (symbol->string x0) 0))))]
      [x0
       (format "~a" x0)]))
  (format
   "\\begin{schemedisplay}~n~a~n\\end{schemedisplay}~n"
   (cm->slatex-inner e 0)))

(provide cm->verbatim
         cm->tex-ams
         cm->slatex)
