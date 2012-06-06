#lang racket

;cm
;E is
;x
;(λ (x) E)
;(E F)
;(wcm E F)
;(ccm)

(define cm-parse-var
  (λ (var)
    `(var ,var)))

(define cm-parse-abs
  (λ (abs)
    `(abs ,(first (second abs)) ,(cm-parse (third abs)))))

(define cm-parse-app
  (λ (app)
    `(app ,(cm-parse (first app)) ,(cm-parse (second app)))))

(define cm-parse-wcm
  (λ (wcm)
    `(wcm ,(cm-parse (second wcm)) ,(cm-parse (third wcm)))))

(define cm-parse-ccm
  (λ (ccm)
    `(ccm)))

(define cm-parse
  (λ (e)
    (if (symbol? e)
        (lc-parse-var e)
        (if (list? e)
            (cond
              ((= (length e) 1) (if (eq? (first e) 'ccm)
                                    (cm-parse-ccm e)
                                    (error "expected ccm, got " (first e))))
              ((= (length e) 2) (lc-parse-app e))
              ((= (length e) 3) (cond
                                  ((eq? (first e) 'λ) (if (list? (second e))
                                                          (if (= (length (second e)) 1)
                                                              (if (symbol? (first (second e)))
                                                                  (lc-parse-abs e)
                                                                  (error "expected symbol as formal parameter, got " (first (second e))))
                                                              (error "expected single parameter, got " (second e)))
                                                          (error "expected parameter list, got " (second e))))
                                  ((eq? (first e) 'wcm) (cm-parse-wcm e))
                                  (else (error "expected λ or wcm, got " (first e)))))
              (else error "expected list of length 2 or 3, got " e))
            (error "expected symbol or list, got" e)))))

(define cm-emit
  (λ (e)
    (if (list? e)
        (cond
          ((eq? (first e) 'var) (second e))
          ((eq? (first e) 'abs) `(λ (,(second e)) ,(cm-emit (third e))))
          ((eq? (first e) 'app) `(,(cm-emit (second e)) ,(cm-emit (third e))))
          ((eq? (first e) 'wcm) `(wcm ,(cm-emit (second e)) ,(cm-emit (third e))))
          ((eq? (first e) 'ccm) `(ccm))
          (else (error "unrecognized tag " (first e))))
        (error "expected a list, got " e))))

(define cm-eval-var
  (λ (var)
    var))

(define cm-eval-abs
  (λ (abs)
    abs))

(define cm-eval-app
  (λ (app)
    app))

(define cm-eval-wcm
  (λ (wcm)
    wcm))

(define cm-eval-ccm
  (λ (ccm)
    ccm))

(define cm-eval
  (λ (e k)
    (cond
      ((eq? (first e) 'ccm) '(ccm))
      (else (error "unrecognized tag " (first e))))))
