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
        (cm-parse-var e)
        (if (list? e)
            (cond
              ((= (length e) 1) (if (eq? (first e) 'ccm)
                                    (cm-parse-ccm e)
                                    (error "expected ccm, got " (first e))))
              ((= (length e) 2) (cm-parse-app e))
              ((= (length e) 3) (cond
                                  ((eq? (first e) 'λ) (if (list? (second e))
                                                          (if (= (length (second e)) 1)
                                                              (if (symbol? (first (second e)))
                                                                  (cm-parse-abs e)
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

(define rename-wcm
  (λ (wcm x y)
    `(wcm ,(rename (second wcm) x y) ,(rename (third wcm) x y))))

(define rename-ccm
  (λ (ccm x y)
    ccm))

; change x to y in e
(define rename
  (λ (e x y)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (rename-var e x y))
        ((eq? tag 'abs) (rename-abs e x y))
        ((eq? tag 'app) (rename-app e x y))
        ((eq? tag 'wcm) (rename-wcm e x y))
        ((eq? tag 'ccm) (rename-ccm e x y))
        (else (error "unrecognized tag " e))))))

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

(define occurs-free-in-wcm
  (λ (wcm x)
    (or (occurs-free-in (second wcm) x) (occurs-free-in (third wcm) x))))

(define occurs-free-in-ccm
  (λ (ccm x)
    #f))

(define occurs-free-in
  (λ (e x)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (occurs-free-in-var e x))
        ((eq? tag 'abs) (occurs-free-in-abs e x))
        ((eq? tag 'app) (occurs-free-in-app e x))
        ((eq? tag 'wcm) (occurs-free-in-wcm e x))
        ((eq? tag 'ccm) (occurs-free-in-ccm e x))
        (else (error "unrecognized tag " e))))))

(define substitute-var
  (λ (e x f)
    (if (eq? (second e) x)
        f
        e)))

(define substitute-abs
  (λ (e x f)
    (if (eq? (second e) x)
        e
        (if (occurs-free-in f (second e))
            `(abs ,(second e) ,(substitute (third e) x (rename f (second e) (fresh-variable))))
            `(abs ,(second e) ,(substitute (third e) x f))))))

(define substitute-app
  (λ (app x f)
    `(app ,(substitute (second app) x f) ,(substitute (third app) x f))))

(define substitute-wcm
  (λ (wcm x f)
    `(wcm ,(substitute (second wcm) x f) ,(substitute (third wcm) x f))))

(define substitute-ccm
  (λ (ccm x f)
    ccm))

(define substitute
  (λ (e x f)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (substitute-var e x f))
        ((eq? tag 'abs) (substitute-abs e x f))
        ((eq? tag 'app) (substitute-app e x f))
        ((eq? tag 'wcm) (substitute-wcm e x f))
        ((eq? tag 'ccm) (substitute-ccm e x f))
        (else (error "unrecognized tag " tag))))))

(define cm-eval-var
  (λ (var k)
    var))

(define cm-eval-abs
  (λ (abs k)
    abs))

(define cm-eval-app
  (λ (app)
    (let ((rator (cm-eval (second app))))
      (if (eq? (first rator) 'abs)
          (substitute (third rator) (second rator) (third app))
          `(app ,rator ,(third app))))))

(define cm-eval-wcm
  (λ (wcm k)
    (if (eq? (first (third wcm)) 'wcm)
        (cm-eval-wcm (third wcm) k)
        (cm-eval (third wcm) `(abs z (app (app (var z) ,(cm-eval (second wcm) k)) ,k))))))

(define cm-eval-ccm
  (λ (ccm k)
    k))

(define cm-eval
  (λ (e k)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (cm-eval-var e k))
        ((eq? tag 'abs) (cm-eval-abs e k))
        ((eq? tag 'app) (cm-eval-app e k))
        ((eq? tag 'wcm) (cm-eval-wcm e k))
        ((eq? tag 'ccm) (cm-eval-ccm e k))
        (else (error "unrecognized tag " tag))))))
