#lang racket

;cm
;E is
;x
;(λ (x) E)
;(E F)
;(wcm E F)
;(ccm)
;n where n is a number

(define parse-var
  (λ (var)
    `(var ,var)))

(define parse-abs
  (λ (abs)
    `(abs ,(first (second abs)) ,(parse (third abs)))))

(define parse-app
  (λ (app)
    `(app ,(parse (first app)) ,(parse (second app)))))

(define parse-wcm
  (λ (wcm)
    `(wcm ,(parse (second wcm)) ,(parse (third wcm)))))

(define parse-ccm
  (λ (ccm)
    `(ccm)))

(define (parse-num num)
  `(num ,num))

(define (parse e)
  (cond
    ((symbol? e) (parse-var e))
    ((list? e) (cond
                 ((= (length e) 1) (if (eq? (first e) 'ccm)
                                       (parse-ccm e)
                                       (error "expected ccm, got " (first e))))
                 ((= (length e) 2) (parse-app e))
                 ((= (length e) 3) (cond
                                     ((eq? (first e) 'λ) (if (list? (second e))
                                                             (if (= (length (second e)) 1)
                                                                 (if (symbol? (first (second e)))
                                                                     (parse-abs e)
                                                                     (error "expected symbol as formal parameter, got " (first (second e))))
                                                                 (error "expected single parameter, got " (second e)))
                                                             (error "expected parameter list, got " (second e))))
                                     ((eq? (first e) 'wcm) (parse-wcm e))
                                     (else (error "expected λ or wcm, got " (first e)))))
                 (else error "expected list of length 2 or 3, got " e)))
    ((number? e) (parse-num e))
    (else (error "expected symbol, list, or number; got" e))))

(provide parse)
