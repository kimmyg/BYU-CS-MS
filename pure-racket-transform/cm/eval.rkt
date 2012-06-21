#lang racket

(require "emit.rkt")
(require "parse.rkt")

(include "../lc/eval-base.rkt")
(include "eval-base.rkt")

; change x to y in e
(define (rename e x y)
  (let ((tag (first e)))
    (cond
      ((eq? tag 'var) (rename-var e x y))
      ((eq? tag 'abs) (rename-abs e x y))
      ((eq? tag 'app) (rename-app e x y))
      ((eq? tag 'num) (rename-num e x y))
      ((eq? tag 'cons) (rename-cons e x y))
      ((eq? tag 'nil) (rename-nil e x y))
      ((eq? tag 'fst) (rename-fst e x y))
      ((eq? tag 'rst) (rename-rst e x y))
      ((eq? tag 'isnil?) (rename-isnil? e x y))
      ((eq? tag 'wcm) (rename-wcm e x y))
      ((eq? tag 'ccm) (rename-ccm e x y))
      (else (error "unrecognized tag " e)))))

(define (occurs-free-in e x)
  (let ((tag (first e)))
    (cond
      ((eq? tag 'var) (occurs-free-in-var e x))
      ((eq? tag 'abs) (occurs-free-in-abs e x))
      ((eq? tag 'app) (occurs-free-in-app e x))
      ((eq? tag 'num) (occurs-free-in-num e x))
      ((eq? tag 'cons) (occurs-free-in-cons e x))
      ((eq? tag 'nil) (occurs-free-in-nil e x))
      ((eq? tag 'fst) (occurs-free-in-fst e x))
      ((eq? tag 'rst) (occurs-free-in-rst e x))
      ((eq? tag 'isnil?) (occurs-free-in-isnil? e x))
      ((eq? tag 'wcm) (occurs-free-in-wcm e x))
      ((eq? tag 'ccm) (occurs-free-in-ccm e x))
      (else (error "unrecognized tag " e)))))

(define substitute
  (λ (e x f)
    (let ((tag (first e)))
      (cond
        ((eq? tag 'var) (substitute-var e x f))
        ((eq? tag 'abs) (substitute-abs e x f))
        ((eq? tag 'app) (substitute-app e x f))
        ((eq? tag 'num) (substitute-num e x f))
        ((eq? tag 'cons) (substitute-cons e x f))
        ((eq? tag 'nil) (substitute-nil e x f))
        ((eq? tag 'fst) (substitute-fst e x f))
        ((eq? tag 'rst) (substitute-rst e x f))
        ((eq? tag 'isnil?) (substitute-isnil? e x f))
        ((eq? tag 'wcm) (substitute-wcm e x f))
        ((eq? tag 'ccm) (substitute-ccm e x f))
        (else (error "unrecognized tag " tag))))))

; eval

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


(define (eval-num num k)
  num)

(define (eval-cons kons k)
  (let ((car (eval-inner (second kons) (cons #f (cdr k)))))
    (let ((cdr (eval-inner (third kons) (cons #f (cdr k)))))
      `(cons ,car ,cdr))))

(define (eval-nil nil k)
  nil)

(define (eval-fst fst k)
  (let ((arg (eval-inner (second fst) (cons #f (cdr k)))))
    (if (eq? (first arg) 'cons)
        (second arg)
        (error "attempting to take fst of " arg))))

(define (eval-rst rst k)
  (let ((arg (eval-inner (second rst) (cons #f (cdr k)))))
    (if (eq? (first arg) 'cons)
        (third arg)
        (error "attempting to take rst of " arg))))

(define (eval-isnil? isnil? k)
  (let ((arg (eval-inner (second isnil?) (cons #f (cdr k)))))
    (let ((tag (first arg)))
      (cond
        ((eq? tag 'nil) '(abs x (abs y (var x))))
        ((eq? tag 'cons) '(abs x (abs y (var y))))
        (else (error "attempting isnil? on " arg))))))

(define (eval-wcm wcm k)
  (let ((v (eval-inner (second wcm) k)))
    (let ((k (cons #t (cons v (if (car k)
                                  (rest (cdr k))
                                  (cdr k))))))
      (eval-inner (third wcm) k))))

(define (chi k)
  (if (empty? k)
      '(nil)
      `(cons ,(first k) ,(chi (rest k)))))

(define (eval-ccm ccm k)
  (chi (cdr k)))

(define (eval-inner e k)
  (let ((tag (first e)))
    (cond
      ((eq? tag 'var) (eval-var e k))
      ((eq? tag 'abs) (eval-abs e k))
      ((eq? tag 'app) (eval-app e k))
      ((eq? tag 'num) (eval-num e k))
      ((eq? tag 'cons) (eval-cons e k))
      ((eq? tag 'nil) (eval-nil e k))
      ((eq? tag 'fst) (eval-fst e k))
      ((eq? tag 'rst) (eval-rst e k))
      ((eq? tag 'isnil?) (eval-isnil? e k))
      ((eq? tag 'wcm) (eval-wcm e k))
      ((eq? tag 'ccm) (eval-ccm e k))
      (else (error "unrecognized tag " tag)))))

(define (eval e)
  (emit (eval-inner (parse e) (cons #f '()))))

(provide eval)