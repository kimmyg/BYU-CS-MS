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
          (else (error "unrecognized tag " (first e))))
        (error "expected a list, got " e))))


(define fresh-variable
  (let ((seed 0))
    (λ ()
      (let ((x (string->symbol (string-append "x" (number->string seed)))))
        (begin
          (set! seed (+ seed 1))
          x)))))

(define rename-var
  (λ (var x y)
    (if (eq? (second var) x)
        `(var ,y)
        var)))

(define rename-abs
  (λ (abs x y)
    (if (eq? (second abs) x)
        abs
        `(abs ,(second abs) ,(rename (third abs) x y)))))

(define rename-app
  (λ (app x y)
    `(app ,(rename (second app) x y) ,(rename (third app) x y))))

; change x to y in e
(define rename
  (λ (e x y)
    (cond
      ((eq? (first e) 'var) (rename-var e x y))
      ((eq? (first e) 'abs) (rename-abs e x y))
      ((eq? (first e) 'app) (rename-app e x y))
      (else (error "unrecognized tag " e)))))

(define occurs-free-in-var
  (λ (var x)
    (eq? (second var) x)))

(define occurs-free-in-abs
  (λ (abs x)
    (if (eq? (second abs) x)
        #f
        (occurs-free-in (third abs) x))))

(define occurs-free-in-app
  (λ (app x)
    (or (occurs-free-in (second app) x) (occurs-free-in (third app) x))))

(define occurs-free-in
  (λ (e x)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (occurs-free-in-var e x))
        ((eq? tag 'abs) (occurs-free-in-abs e x))
        ((eq? tag 'app) (occurs-free-in-app e x))
        (else (error "unrecognized tag " e))))))

(define substitute-var
  (λ (var x f)
    (if (eq? (second var) x)
        f
        var)))

(define substitute-abs
  (λ (abs x f)
    (if (eq? (second abs) x)
        abs
        (if (occurs-free-in f (second abs))
            `(abs ,(second abs) ,(substitute (third abs) x (rename f (second abs) (fresh-variable))))
            `(abs ,(second abs) ,(substitute (third abs) x f))))))

(define substitute-app
  (λ (app x f)
    `(app ,(substitute (second app) x f) ,(substitute (third app) x f))))

(define substitute
  (λ (e x f)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (substitute-var e x f))
        ((eq? tag 'abs) (substitute-abs e x f))
        ((eq? tag 'app) (substitute-app e x f))
        (else (error "unrecognized tag " tag))))))

(define lc-eval-var
  (λ (var)
    var))

(define lc-eval-abs
  (λ (abs)
    abs))

(define lc-eval-app
  (λ (app)
    (let ((rator (lc-eval (second app))))
      (if (eq? (first rator) 'abs)
          (substitute (third rator) (second rator) (third app))
          `(app ,rator ,(third app))))))
        
(define lc-eval
  (λ (e)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (lc-eval-var e))
        ((eq? tag 'abs) (lc-eval-abs e))
        ((eq? tag 'app) (lc-eval-app e))
        (else (error "unrecognized tag " tag))))))
