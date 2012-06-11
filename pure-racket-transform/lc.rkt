;(require racket/include)
;(include "fresh-variable.rkt")

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

(define lc-emit
  (λ (e)
    (if (list? e)
        (cond
          ((eq? (first e) 'var) (second e))
          ((eq? (first e) 'abs) `(λ (,(second e)) ,(lc-emit (third e))))
          ((eq? (first e) 'app) `(,(lc-emit (second e)) ,(lc-emit (third e))))
          (else (error "lc-emit unrecognized tag " (first e))))
        (error "expected a list, got " e))))

(define lc-rename-var
  (λ (var x y)
    (if (eq? (second var) x)
        `(var ,y)
        var)))

(define lc-rename-abs
  (λ (abs x y)
    (if (eq? (second abs) x)
        abs
        `(abs ,(second abs) ,(lc-rename (third abs) x y)))))

(define lc-rename-app
  (λ (app x y)
    `(app ,(lc-rename (second app) x y) ,(lc-rename (third app) x y))))

; change x to y in e
(define lc-rename
  (λ (e x y)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (lc-rename-var e x y))
        ((eq? tag 'abs) (lc-rename-abs e x y))
        ((eq? tag 'app) (lc-rename-app e x y))
        (else (error "lc-rename unrecognized tag " e))))))

(define lc-occurs-free-in-var
  (λ (var x)
    (eq? (second var) x)))

(define lc-occurs-free-in-abs
  (λ (abs x)
    (if (eq? (second abs) x)
        #f
        (lc-occurs-free-in (third abs) x))))

(define lc-occurs-free-in-app
  (λ (app x)
    (or (lc-occurs-free-in (second app) x) (lc-occurs-free-in (third app) x))))

(define lc-occurs-free-in
  (λ (e x)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (lc-occurs-free-in-var e x))
        ((eq? tag 'abs) (lc-occurs-free-in-abs e x))
        ((eq? tag 'app) (lc-occurs-free-in-app e x))
        (else (error "lc-occurs-free-in unrecognized tag " e))))))

(define lc-substitute-var
  (λ (var x f)
    (if (eq? (second var) x)
        f
        var)))

(define lc-substitute-abs
  (λ (abs x f)
    (if (eq? (second abs) x)
        abs
        (if (lc-occurs-free-in f (second abs))
            `(abs ,(second abs) ,(lc-substitute (third abs) x (lc-rename f (second abs) (fresh-variable))))
            `(abs ,(second abs) ,(lc-substitute (third abs) x f))))))

(define lc-substitute-app
  (λ (app x f)
    `(app ,(lc-substitute (second app) x f) ,(lc-substitute (third app) x f))))

(define lc-substitute
  (λ (e x f)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (lc-substitute-var e x f))
        ((eq? tag 'abs) (lc-substitute-abs e x f))
        ((eq? tag 'app) (lc-substitute-app e x f))
        (else (error "lc-substitute unrecognized tag " tag))))))

(define lc-eval-var
  (λ (var)
    var))

(define lc-eval-abs
  (λ (abs)
    abs))

(define lc-eval-app
  (λ (app)
    (let ((rator (lc-eval-inner (second app))))
      (if (eq? (first rator) 'abs)
          (lc-substitute (third rator) (second rator) (third app))
          `(app ,rator ,(third app))))))
        
(define lc-eval-inner
  (λ (e)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (lc-eval-var e))
        ((eq? tag 'abs) (lc-eval-abs e))
        ((eq? tag 'app) (lc-eval-app e))
        (else (error "lc-eval-inner unrecognized tag " tag))))))

(define lc-eval
  (λ (e)
    (lc-emit (lc-eval-inner (lc-parse e)))))
