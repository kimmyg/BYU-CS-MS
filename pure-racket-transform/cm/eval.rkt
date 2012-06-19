#lang racket

(require "emit.rkt")
(require "parse.rkt")

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
            `(abs ,(second e) ,(substitute (third e) x (rename f (second e) (gensym 'x))))
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

(define (eval-var var k)
  var)

(define (eval-abs abs k)
  abs)

(define (eval-app app k)
  (let ((rator (eval-inner (second app) (cons #f (cdr k)))))
    (let ((rand (eval-inner (third app) (cons #f (cdr k)))))
      (if (eq? (first rator) 'abs)
          (eval-inner (substitute (third rator) (second rator) rand) k)
          `(app ,rator ,rand)))))

(define (eval-wcm wcm k)
  (let ((v (eval-inner (second wcm) k)))
    (let ((k (cons #t (cons v (if (car k)
                                  (rest (cdr k))
                                  (cdr k))))))
      (eval-inner (third wcm) k))))

(define (chi k)
  (if (empty? k)
      '(abs x (abs y (var y)))
      `(abs p (app (app (var p) ,(first k)) ,(chi (rest k))))))

(define (eval-ccm ccm k)
  (chi (cdr k)))

(define (eval-inner e k)
  (let ((tag (first e)))
    (cond
      ((eq? tag 'var) (eval-var e k))
      ((eq? tag 'abs) (eval-abs e k))
      ((eq? tag 'app) (eval-app e k))
      ((eq? tag 'wcm) (eval-wcm e k))
      ((eq? tag 'ccm) (eval-ccm e k))
      (else (error "unrecognized tag " tag)))))

(define (eval e)
  (emit (eval-inner (parse e) (cons #f '()))))

(provide eval)