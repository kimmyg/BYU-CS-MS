; lc
; E is
; x
; (λ (x) E)
; (E F)
; n where n is a number
; (cons E F)
; nil

(define (parse-var var)
  `(var ,var))

(define (parse-abs abs)
  `(abs ,(first (second abs)) ,(parse (third abs))))

(define (parse-app app)
  `(app ,(parse (first app)) ,(parse (second app))))

(define (parse-num num)
  `(num ,num))

(define (parse-cons cons)
  `(cons ,(parse (second cons)) ,(parse (third cons))))

(define (parse-nil nil)
  '(nil))

(define (parse-fst fst)
  `(fst ,(parse (second fst))))

(define (parse-rst rst)
  `(rst ,(parse (second rst))))

(define (parse-isnil? isnil?)
  `(isnil? ,(parse (second isnil?))))