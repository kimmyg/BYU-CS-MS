; rename

(define (rename-var var x y)
  (if (eq? (second var) x)
      `(var ,y)
      var))

(define (rename-abs abs x y)
  (if (eq? (second abs) x)
      abs
      `(abs ,(second abs) ,(rename (third abs) x y))))

(define (rename-app app x y)
  `(app ,(rename (second app) x y) ,(rename (third app) x y)))

(define (rename-num num x y)
  num)

(define (rename-cons cons x y)
  `(cons ,(rename (second cons) x y) ,(rename (third cons) x y)))

(define (rename-nil nil x y)
  nil)

(define (rename-fst fst x y)
  `(fst ,(rename (second fst) x y)))

(define (rename-rst rst x y)
  `(rst ,(rename (second rst) x y)))

(define (rename-isnil? isnil? x y)
  `(isnil? ,(rename (second isnil?) x y)))


; occurs free in

(define (occurs-free-in-var var x)
    (eq? (second var) x))

(define (occurs-free-in-abs abs x)
  (if (eq? (second abs) x)
      #f
      (occurs-free-in (third abs) x)))

(define (occurs-free-in-app app x)
  (or (occurs-free-in (second app) x) (occurs-free-in (third app) x)))

(define (occurs-free-in-num num x)
  #f)

(define (occurs-free-in-cons cons x)
  (or (occurs-free-in (second cons) x) (occurs-free-in (third cons) x)))

(define (occurs-free-in-nil nil x)
  #f)

(define (occurs-free-in-fst fst x)
  (occurs-free-in (second fst) x))

(define (occurs-free-in-rst rst x)
  (occurs-free-in (second rst) x))

(define (occurs-free-in-isnil? isnil? x)
  (occurs-free-in (second isnil?) x))

; substitute

(define (substitute-var var x f)
  (if (eq? (second var) x)
      f
      var))

(define (substitute-abs abs x f)
  (if (eq? (second abs) x)
      abs
      (if (occurs-free-in f (second abs))
          `(abs ,(second abs) ,(substitute (third abs) x (rename f (second abs) (gensym 'x))))
          `(abs ,(second abs) ,(substitute (third abs) x f)))))

(define (substitute-app app x f)
  `(app ,(substitute (second app) x f) ,(substitute (third app) x f)))

(define (substitute-num num x f)
  num)

(define (substitute-cons cons x f)
  `(cons ,(substitute (second cons) x f) ,(substitute (third cons) x f)))

(define (substitute-nil nil x f)
  nil)

(define (substitute-fst fst x f)
  `(fst ,(substitute (second fst) x f)))

(define (substitute-rst rst x f)
  `(rst ,(substitute (second rst) x f)))

(define (substitute-isnil? isnil? x f)
  `(isnil? ,(substitute (second isnil?) x f)))
