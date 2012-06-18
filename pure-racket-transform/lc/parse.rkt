#lang racket

;lc
;E is
;x
;(λ (x) E)
;(E F)

(define lc-parse-var
  (λ (var)
    `(var ,var)))

(define lc-parse-abs
  (λ (abs)
    `(abs ,(first (second abs)) ,(lc-parse (third abs)))))

(define lc-parse-app
  (λ (app)
    `(app ,(lc-parse (first app)) ,(lc-parse (second app)))))
    
(define lc-parse
  (λ (e)
    (if (symbol? e)
        (lc-parse-var e)
        (if (list? e)
            (cond
              ((= (length e) 2) (lc-parse-app e))
              ((= (length e) 3) (if (eq? (first e) 'λ)
                                    (if (list? (second e))
                                        (if (= (length (second e)) 1)
                                            (if (symbol? (first (second e)))
                                                (lc-parse-abs e)
                                                (error "expected symbol as formal parameter, got " (first (second e))))
                                            (error "expected single parameter, got " (second e)))
                                        (error "expected parameter list, got " (second e)))
                                    (error "expected λ symbol, got " (first e))))
              (else error "expected list of length 2 or 3, got " e))
            (error "expected symbol or list, got" e)))))

(provide lc-parse)
