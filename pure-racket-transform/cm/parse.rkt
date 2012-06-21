#lang racket

(include "../lc/parse-base.rkt")
(include "parse-base.rkt")

(define (parse e)
  (cond
    ((symbol? e) (if (eq? e 'nil)
                     (parse-nil e)
                     (parse-var e)))
    ((list? e) (cond
                 ((= (length e) 1) (if (eq? (first e) 'ccm)
                                       (parse-ccm e)
                                       (error "expected ccm, got " (first e))))
                 ((= (length e) 2) (cond
                                     ((eq? (first e) 'fst) (parse-fst e))
                                     ((eq? (first e) 'rst) (parse-rst e))
                                     ((eq? (first e) 'isnil?) (parse-isnil? e))
                                     (else (parse-app e))))
                 ((= (length e) 3) (cond
                                     ((eq? (first e) 'λ) (if (list? (second e))
                                                             (if (= (length (second e)) 1)
                                                                 (if (symbol? (first (second e)))
                                                                     (parse-abs e)
                                                                     (error "expected symbol as formal parameter, got " (first (second e))))
                                                                 (error "expected single parameter, got " (second e)))
                                                             (error "expected parameter list, got " (second e))))
                                     ((eq? (first e) 'cons) (parse-cons e))
                                     ((eq? (first e) 'wcm) (parse-wcm e))
                                     (else (error "expected λ, cons, or wcm; got " (first e)))))
                 (else error "expected list of length 2 or 3, got " e)))
    ((number? e) (parse-num e))
    (else (error "expected symbol, list, or number; got" e))))

(provide parse)
