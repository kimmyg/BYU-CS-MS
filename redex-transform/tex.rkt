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

(define (cm->tex e)
  (define (cm->tex-inner e i)
    (match e
      [(list 'wcm e1 e2)
       (if (should-split e1 e2)
           (begin
             (printf "wcm ")
             (cm->tex-inner e1 (+ i 4))
             (newline)
             (for ([j (+ i 4)])
               (printf " "))
             (cm->tex-inner e2 (+ i 4)))
           (begin
             (printf "wcm ")
             (cm->tex-inner e1 (+ i 4))
             (printf " ")
             (cm->tex-inner e2 (+ i 4 (cm-length e1) 1))))]
      [(list 'ccm)
       (printf "(ccm)")]
      [(list 'λ (list x1) e1)
       (begin
         (printf "\\~c." (string-ref (symbol->string x1) 0))
         (cm->tex-inner e1 (+ i 3)))]
      [(list e1 e2)
       (if (should-split e1 e2)
           (begin
             (printf "(")
             (cm->tex-inner e1 (+ i 1))
             (newline)
             (for ([j (+ i 1)])
               (printf " "))
             (cm->tex-inner e2 (+ i 1))
             (printf ")"))
           (begin
             (printf "(")
             (cm->tex-inner e1 (+ i 1))
             (printf " ")
             (cm->tex-inner e2 (+ i 1 (cm-length e1) 1))
             (printf ")")))]
      [(? symbol? x1)
       (printf "~c" (string-ref (symbol->string x1) 0))]
      [x1
       (printf "~a" x1)]))
  (display "{{{")
  (newline)
  (display "\\renewcommand{\\baselinestretch}{0.5}")
  (newline)
  (display "\\begin{verbatim}")
  (newline)
  (cm->tex-inner e 0)
  (newline)
  (display "\\end{verbatim}")
  (newline)
  (display "}}}")
  (newline))

(provide cm->tex)
