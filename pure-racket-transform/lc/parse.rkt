#lang racket

;lc
;E is
;x
;(λ (x) E)
;(E F)

(define parse-var
  (λ (var)
    `(var ,var)))

(define parse-abs
  (λ (abs)
    `(abs ,(first (second abs)) ,(parse (third abs)))))

(define parse-app
  (λ (app)
    `(app ,(parse (first app)) ,(parse (second app)))))
    
(define parse
  (λ (e)
    (if (symbol? e)
        (parse-var e)
        (if (list? e)
            (cond
              ((= (length e) 2) (parse-app e))
              ((= (length e) 3) (if (eq? (first e) 'λ)
                                    (if (list? (second e))
                                        (if (= (length (second e)) 1)
                                            (if (symbol? (first (second e)))
                                                (parse-abs e)
                                                (error "expected symbol as formal parameter, got " (first (second e))))
                                            (error "expected single parameter, got " (second e)))
                                        (error "expected parameter list, got " (second e)))
                                    (error "expected λ symbol, got " (first e))))
              (else error "expected list of length 2 or 3, got " e))
            (error "expected symbol or list, got" e)))))

(provide parse)
