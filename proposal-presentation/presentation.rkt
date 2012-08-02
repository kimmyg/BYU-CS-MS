#lang slideshow

(require slideshow/base
         slideshow/code
         slideshow/pict
         racket/gui)

(current-page-number-font (make-object font% 40 'default))

(define (underline p)
  (refocus
   (vc-append p (colorize (linewidth 4 (hline (pict-width p) 0)) "red"))
   p))

(define (strike p)
  (refocus
   (cc-superimpose p (colorize (linewidth 4 (hline (pict-width p) 0)) "red"))
   p))

(define (reduces-to a b)
  (let ((reduction (hc-append a (blank 32 0) b)))
    (pin-arrow-line #:line-width 4 (/ (current-font-size) 2) reduction a rc-find b lc-find)))

(define (maps-to a b)
  (let ((reduction (hc-append a (blank 16 0) b)))
    (pin-arrow-line #:line-width 4 (/ (current-font-size) 2) reduction a rc-find b lc-find)))

(define (decompose a b c [blank? #f])
  (if blank?
      (vc-append a
                 (blank 0 64)
                 (hb-append (ghost b)
                            (blank 32 0)
                            (ghost c)))
      (let* ((decomposition (vc-append a
                                       (blank 0 64)
                                       (hb-append b
                                                  (blank 32 0)
                                                  c)))
             (left (pin-arrow-line #:line-width 4 (/ (current-font-size) 2) decomposition a cb-find b ct-find))
             (left-and-right (pin-arrow-line #:line-width 4 (/ (current-font-size) 2) left a cb-find c ct-find)))
        left-and-right)))

(define (emphasize p)
  (frame p #:color "red" #:line-width 4))

(slide
 (cc-superimpose (blank)
                 (vc-append (text "A CPS-like Transformation for Continuation Marks" (current-main-font) 36)
                            (text "Kimball Germane" (current-main-font) 36))))

(slide
 #:title "Thesis"
 #:layout 'top
 (para #:width 768 "A CPS-like global transformation can compile the λ-calculus with"
       "continuation marks to the plain λ-calculus in a semantics-preserving way."))
; maybe read the slide in this case
; "this statement may call for some unpacking"

(slide
 #:title "Thesis"
 #:layout 'top
 (para #:width 768 "A CPS-like global transformation can compile the" (underline (t "λ-calculus")) "with"
        "continuation marks to the plain" (underline (t "λ-calculus")) "in a semantics-preserving way."))
; "First, let's discuss the λ-calculus."

(slide
 #:title "The λ-calculus"
 #:layout 'top
 (t "a formal system for expressing computation")
; "Other formal systems include... (mention monads?)"
 'next
 (para (t "Individual terms take one of three forms:"))
; "Terms in the λ-calculus take one of three forms and are defined inductively"
 'next
 (item (para (t "variables") (tt "x") (t ",") (tt "y") (t ",") (tt "z") (t ",...")))
; "There are an infinite number of variables and they are compared syntactically. Basically, we assume we can, in all circumstances, come up with a free variable..."
 'next
 (item (para (t "abstractions") (tt "λx.M")))
; "An abstraction corresponds to a function; it binds variables for substitution."
 'next
 (item (para "applications" (tt "M N"))))
; "'application' refers to 'function application'. The process of application is called 'reduction'. It corresponds to 'evaluation'."

(slide
 #:title "λ-calculus example"
 #:layout 'top
 'alts
 (list (list (para (tt "λx.(z x)") (tt "λy.y")))
       (list (para (underline (tt "λx.(z x)")) (tt "λy.y")))
       (list (para (tt "λx.(z x)") (underline (tt "λy.y"))))
       (list (para (tt "λx.(z x)") (tt "λy.y"))))
 'next
 (para (arrow 18 0) (tt "z λy.y"))
 'next
 (blank)
 (para "We use the call-by-value λ-calculus and refer to it as " (it "λv") "."))
 
(slide
 #:title "Thesis"
 #:layout 'top
 (para #:width 768 "A" (underline (t "CPS-like")) "global transformation can compile the λ-calculus with"
       "continuation marks to the plain λ-calculus in a semantics-preserving way."))

(slide
 #:title "What does it mean to be CPS-like?"
 #:layout 'top
 (para "\"CPS\" abbreviates \"continuation-passing style\".")
 'next
 (para "The continuation is simply the rest of the computation."))

(let ((redex-title (t "possible redex"))
      (cont-title (t "continuation"))
      (r0 (tt "(+ 1 (+ 2 (+ 3 4)))"))
      (k0 (tt "•"))
      (r1 (tt "(+ 2 (+ 3 4))"))
      (k1 (tt "(+ 1 •)"))
      (r2 (tt "(+ 3 4)"))
      (k2 (tt "(+ 1 (+ 2 •))"))
      (v2 (tt "7"))
      (r3 (tt "(+ 2 7)"))
      (v1 (tt "9"))
      (r4 (tt "(+ 1 9)"))
      (v0 (tt "10")))
  (slide
   #:title "Continuation Example"
   #:layout 'top
   (para "Consider the evaluation of" r0 ".")
; "this says, add 3 and 4, add 2 to the result of that, and add 1 to the result of that"
; "when we evaluate a nested expression such as this, we naturally decompose it into the evaluation of a subexpression and what we do with that result
   'next
   'alts
   (list (list (ht-append (vl-append redex-title
                                     (ghost r0)
                                     (ghost r1)
                                     (ghost r2)
                                     (ghost v2)
                                     (ghost r3)
                                     (ghost v1)
                                     (ghost r4)
                                     (ghost v0))
                          (blank 16 0)
                          (vl-append (ghost cont-title)
                                     (ghost k0)
                                     (ghost k1)
                                     (ghost k2)
                                     (ghost k2)
                                     (ghost k1)
                                     (ghost k1)
                                     (ghost k0)
                                     (ghost k0))))
         (list (ht-append (vl-append redex-title
                                     (ghost r0)
                                     (ghost r1)
                                     (ghost r2)
                                     (ghost v2)
                                     (ghost r3)
                                     (ghost v1)
                                     (ghost r4)
                                     (ghost v0))
                          (blank 16 0)
                          (vl-append cont-title
                                     (ghost k0)
                                     (ghost k1)
                                     (ghost k2)
                                     (ghost k2)
                                     (ghost k1)
                                     (ghost k1)
                                     (ghost k0)
                                     (ghost k0))))
         (list (ht-append (vl-append redex-title
                                     r0
                                     (ghost r1)
                                     (ghost r2)
                                     (ghost v2)
                                     (ghost r3)
                                     (ghost v1)
                                     (ghost r4)
                                     (ghost v0))
                          (blank 16 0)
                          (vl-append cont-title
                                     k0
                                     (ghost k1)
                                     (ghost k2)
                                     (ghost k2)
                                     (ghost k1)
                                     (ghost k1)
                                     (ghost k0)
                                     (ghost k0))))
; "the hole represents where the value will be placed."
         (list (ht-append (vl-append redex-title
                                     r0
                                     r1
                                     (ghost r2)
                                     (ghost v2)
                                     (ghost r3)
                                     (ghost v1)
                                     (ghost r4)
                                     (ghost v0))
                          (blank 16 0)
                          (vl-append cont-title
                                     k0
                                     k1
                                     (ghost k2)
                                     (ghost k2)
                                     (ghost k1)
                                     (ghost k1)
                                     (ghost k0)
                                     (ghost k0))))
         (list (ht-append (vl-append redex-title
                                     r0
                                     r1
                                     r2
                                     (ghost v2)
                                     (ghost r3)
                                     (ghost v1)
                                     (ghost r4)
                                     (ghost v0))
                          (blank 16 0)
                          (vl-append cont-title
                                     k0
                                     k1
                                     k2
                                     (ghost k2)
                                     (ghost k1)
                                     (ghost k1)
                                     (ghost k0)
                                     (ghost k0))))
         (list (ht-append (vl-append redex-title
                                     r0
                                     r1
                                     r2
                                     (ghost v2)
                                     (ghost r3)
                                     (ghost v1)
                                     (ghost r4)
                                     (ghost v0))
                          (blank 16 0)
                          (vl-append cont-title
                                     k0
                                     k1
                                     k2
                                     (ghost k2)
                                     (ghost k1)
                                     (ghost k1)
                                     (ghost k0)
                                     (ghost k0))))
         (list (ht-append (vl-append redex-title
                                     r0
                                     r1
                                     r2
                                     v2
                                     (ghost r3)
                                     (ghost v1)
                                     (ghost r4)
                                     (ghost v0))
                          (blank 16 0)
                          (vl-append cont-title
                                     k0
                                     k1
                                     k2
                                     k2
                                     (ghost k1)
                                     (ghost k1)
                                     (ghost k0)
                                     (ghost k0))))
         (list (ht-append (vl-append redex-title
                                     r0
                                     r1
                                     r2
                                     v2
                                     r3
                                     (ghost v1)
                                     (ghost r4)
                                     (ghost v0))
                          (blank 16 0)
                          (vl-append cont-title
                                     k0
                                     k1
                                     k2
                                     k2
                                     k1
                                     (ghost k1)
                                     (ghost k0)
                                     (ghost k0))))
         (list (ht-append (vl-append redex-title
                                     r0
                                     r1
                                     r2
                                     v2
                                     r3
                                     v1
                                     (ghost r4)
                                     (ghost v0))
                          (blank 16 0)
                          (vl-append cont-title
                                     k0
                                     k1
                                     k2
                                     k2
                                     k1
                                     k1
                                     (ghost k0)
                                     (ghost k0))))
         (list (ht-append (vl-append redex-title
                                     r0
                                     r1
                                     r2
                                     v2
                                     r3
                                     v1
                                     r4
                                     (ghost v0))
                          (blank 16 0)
                          (vl-append cont-title
                                     k0
                                     k1
                                     k2
                                     k2
                                     k1
                                     k1
                                     k0
                                     (ghost k0))))
         (list (ht-append (vl-append redex-title
                                     r0
                                     r1
                                     r2
                                     v2
                                     r3
                                     v1
                                     r4
                                     v0)
                          (blank 16 0)
                          (vl-append cont-title
                                     k0
                                     k1
                                     k2
                                     k2
                                     k1
                                     k1
                                     k0
                                     k0))))))

(let ((r0 (tt "(+ 1 (+ 2 (+ 3 4)))"))
      (k0 (tt "•"))
      (r1 (tt "(+ 2 (+ 3 4))"))
      (k1 (tt "(+ 1 •)"))
      (r2 (tt "(+ 3 4)"))
      (k2 (tt "(+ 1 (+ 2 •))")))
  (slide
   #:title "Continuation Example"
   #:layout 'top
   'alts
   (list (list (decompose r0 r1 k1 #t))
         (list (decompose r0 r1 k1))
         (list (decompose r0 r1 (underline k1))))
   'next
   (para "This is like" (maps-to (tt "fun(x)") (tt "x+1")) ".")))

(slide
 #:title "Continuation-passing style"
 #:layout 'top
 (para "In continuation-passing style," (it "every") "function has an additional"
       "formal parameter representing the current continuation.")
 'next
 'alts
 (list (list (para (maps-to (tt "fun(x)") (tt "return x*2"))))
; "continuation-passing style is a style in which functions never return a value."        
       (list (para (strike (maps-to (tt "fun(x)") (tt "return x*2"))))
             (para (maps-to (tt "fun(x,k)") (tt "k(x*2)"))))))
; "instead, they pass the computed value to another argument, the continuation"

(slide
 #:title "Continuation-passing style"
 #:layout 'top
 (para "Why use continuation-passing style?")
; "There are an immense number of benefits of CPS (see Appel), but we use it almost exclusively for this:
 'next
 (para "To \"compile away\" continuation marks")
 'next
 (blank)
 (para "Bonus: It allows control over evaluation order."))
; "I will discuss this in a few minutes."
 
(slide
 #:title "Thesis"
 #:layout 'top
 (para #:width 768 "A CPS-like global transformation can compile the λ-calculus with"
       (underline (t "continuation marks")) "to the plain λ-calculus in a semantics-preserving way."))

(slide
 #:title "Continuation marks"
 #:layout 'top
 (para "a programming language feature that allows us to annotate the continuation"))
; "the continuation is often modelled with the stack."

(slide
 #:title "Continuation marks example"
 #:layout 'top
 (para (code (define (fac n)
               (if (zero? n)
                   1
                   (* n (fac (- n 1))))))))

(slide
 #:title "Continuation marks example"
 #:layout 'top
 (para #:width 960 (code (define (fac n)
                           (if (zero? n)
                               (begin
                                 (display (ccm))
                                 (newline)
                                 1)
                               (wcm 'fac n (* n (fac (- n 1))))))))
 'next
 (para #:width 960 (code (fac 3)))
 'next
 (para #:width 960 (tt "(((fac 1)) ((fac 2)) (fac 3)))"))
 (para #:width 960 (code 6)))

(slide
 #:title "The tail-call behavior of continuation marks"
 #:layout 'top
 (para (code (define (fac n acc)
               (if (zero? n)
                   acc
                   (fac (- n 1) (* n acc)))))))

(slide
 #:title "The tail-call behavior of continuation marks"
 #:layout 'top
 (para #:width 960 (code (define (fac n acc)
                           (if (zero? n)
                               (begin
                                 (display (ccm))
                                 (newline)
                                 acc)
                               (wcm 'fac n (fac (- n 1) (* n acc)))))))
 'next
 (para #:width 960 (code (fac 3 1)))
 'next
 (para #:width 960 (tt "(((fac 1)))"))
 (para #:width 960 (code 6)))

(slide
 #:title "Some applications of continuation marks"
 #:layout 'top
 (item "profilers")
 (item "debuggers")
 (item "steppers"))

(let ((ev (vl-append (ht-append (tt "e=")
                                (vl-append (tt "x")
                                           (tt "v")
                                           (tt "(e e)")
                                           (tt "(wcm e e)")
                                           (tt "(ccm)")))
                     (ht-append (tt "v=")
                                (tt "λx.e"))))
      (EF (vl-append (ht-append (tt "E=")
                                  (vl-append (tt "(wcm v F)")
                                             (tt "F")))
                       (ht-append (tt "F=")
                                  (vl-append (tt "•")
                                             (tt "(E e)")
                                             (tt "(v E)")
                                             (tt "(wcm E e)"))))))
  (slide
   #:title "λcm"
   'alts
   (list (list (ht-append ev (blank 128 0) EF))
         (list (ht-append (emphasize ev) (blank 128 0) EF))
         (list (ht-append ev (blank 128 0) (emphasize EF))))))
 
; "we consider the λ-calculus with facilities for continuation marks"
 

(let ((r1 (tt "E[(λx.e) v]"))
      (r2 (tt "E[(wcm v (wcm v' e))]"))
      (r3 (tt "E[(wcm v v')]"))
      (r4 (tt "E[(ccm)]"))
      (q1 (tt "E[e[x <- v]]"))
      (q2 (tt "E[(wcm v' e)]"))
      (q3 (tt "E[v']"))
      (q4 (tt "E[chi(E)]"))
      (ar (tt "-->"))
      (chi (ht-append (vr-append (tt "chi(•)")
                                 (tt "chi((E e))")
                                 (tt "chi((v E))")
                                 (tt "chi((wcm E e))")
                                 (tt "chi((wcm v E))"))
                      (vc-append (tt " = ")
                                 (tt " = ")
                                 (tt " = ")
                                 (tt " = ")
                                 (tt " = "))
                      (vl-append (tt "empty")
                                 (tt "chi(E)")
                                 (tt "chi(E)")
                                 (tt "chi(E)")
                                 (tt "v:chi(E)")))))
  (slide
   #:title "λcm"
   'alts
   (list (list (vc-append (ht-append (vr-append r1 r2 r3 r4) (vc-append ar ar ar ar) (vl-append q1 q2 q3 q4)) (blank 0 32) chi))
         (list (vc-append (ht-append (vr-append (underline r1) r2 r3 r4) (vc-append ar ar ar ar) (vl-append (underline q1) q2 q3 q4)) (blank 0 32) chi))
         (list (vc-append (ht-append (vr-append r1 (underline r2) r3 r4) (vc-append ar ar ar ar) (vl-append q1 (underline q2) q3 q4)) (blank 0 32) chi))
         (list (vc-append (ht-append (vr-append r1 r2 (underline r3) r4) (vc-append ar ar ar ar) (vl-append q1 q2 (underline q3) q4)) (blank 0 32) chi))
         (list (vc-append (ht-append (vr-append r1 r2 r3 (underline r4)) (vc-append ar ar ar ar) (vl-append q1 q2 q3 (underline q4))) (blank 0 32) chi))
         (list (vc-append (ht-append (vr-append r1 r2 r3 r4) (vc-append ar ar ar ar) (vl-append q1 q2 q3 q4)) (blank 0 32) (emphasize chi))))))

(slide
 #:title "Thesis"
 #:layout 'top
 (para #:width 768 "A CPS-like global transformation can compile the λ-calculus with"
       "continuation marks to the plain λ-calculus in a" (underline (t "semantics-preserving")) "way."))

(slide
 #:title "Meaning-preservation Theorem"
 #:layout 'top
 (para "For expressions" (it "e") "and values" (it "v") "," (it "C") "is a syntactic transformation with the property that")
 (bitmap "theorem.png")
 (para "with the reduction relations from λcm and λv, respectively."))

(let* ((first-term (code (wcm e (wcm e\' e\'\'))))
       (reduces-to (λ (term [first? #f])
                     (let* ((ghost-first-term (ghost first-term))
                            (divider (blank 32 0))
                            (line (hc-append (if first? first-term ghost-first-term) divider term)))
                       (para #:width 960 (pin-arrow-line 12 line (if first? first-term ghost-first-term) rc-find term lc-find #:line-width 4))))))
  (slide
   #:title "Importance of evaluation order"
   #:layout 'left
   'alts
   (list (list (para #:width 960 first-term))
         (list (reduces-to (code (wcm v (wcm e\' e\'\'))) #t)))
   'next
   (reduces-to (code (wcm v (wcm v\' e\'\'))))
   'next
   (reduces-to (code (wcm v\' e\'\')))
   'next
   (reduces-to (code (wcm v\' v\'\')))
   'next
   (reduces-to (code v\'\'))))

(slide
 #:title "Thesis"
 #:layout 'top
 (para #:width 768 "A CPS-like global transformation can compile the λ-calculus with"
       "continuation marks to the plain λ-calculus in a semantics-preserving way."))

(slide
 #:title "The road to legitimacy..."
 (item (text "monad*" (current-main-font) 48))
 (item (text "CPS transform" (current-main-font) 48))
 (item (text "type system" (current-main-font) 48))
 (item (text "expressiveness proof" (current-main-font) 48)))
#|
the λ-calculus
a formal system that expresses computation


introduce continuation marks
(code (with-continuation-mark 'fact n (* n (fac (- n 1))))
show application of continuation marks; probably an algebraic stepper
talk about CPS
what it stands for (truth and...)
what a continuation is:  the "rest of the computation"
continuation-passing style is so termed because the "rest of the computation" is passed as a function at each call site
for instance, consider
(define (fac n)
 (if (zero? n)
     1
     (* n (fac (sub1 n)))))

in continuation-passing style, /every/ function receives an additional parameter k, itself a function, into which the result 
is passed

(define (fac n k)
 (zero? n (λ (n-is-zero)
           (if n-is-zero
               (k 1)
               (sub1 n (λ (n-minus-one)
                        (fac n-minus-one (λ (fac-n-sub-1)
                                          (*' n fac-n-sub-1 k))))))))

What's the appeal of continuation-passing style?
There are many, but we are interested in
- its ability to control evaluation order
commentary: the semantics of continuation marks define an evaluation/reduction order.
(wcm e (wcm e' e'')) =>*
(wcm v (wcm e' e'')) =>*
(wcm v (wcm v' e'')) =>
(wcm v' e'') =>*
(wcm v' v'') =>
v''
[use an algebraic stepper to show the reduction, then discuss how it is an application of continuation marks!]

reiterate thesis
talk about transformation
when we say the transformation is meaning-preserving, we mean that if a program in cm reduces, according 
to the semantics of cm, to a value v, then the transformed program--now a program in lv--reduces, according to the 
semantics of lv, to a value v' and v \equiv v'.
formally, if e ->cm v and C[e] ->lv v' then v \equiv v'
specifically C[v]=v'


The continuation is commonly described as the "rest of the computation".
This is computation that occurs both "inside" and "outside" the continuation. (static vs. dynamic continuation?)
(Note tail recursive factorial.)
Consider the expression (+ 3 (+ 1 1)) (in an strict regime?).
If we directly interpret this, we can decompose the term with a hole (+ 3 <>) from the redex (+ 1 1).
The computation (+ 3 <>) is pending after the computation (+ 1 1). It is part of the continuation.
(In this case, the whole thing.)

Now consider (+ 5 (+ 4 (+ 3 (+ 2 1))))
This is one view of the evaluation process:

(+ 5 (+ 4 (+ 3 (+ 2 1))))

(+ 4 (+ 3 (+ 2 1)))
(+ 5 <>)

(+ 3 (+ 2 1))
(+ 4 <>)
(+ 5 <>)

(+ 2 1)
(+ 3 <>)
(+ 4 <>)
(+ 5 <>)

3
(+ 3 <>)
(+ 4 <>)
(+ 5 <>)

(+ 3 3)
(+ 4 <>)
(+ 5 <>)

6
(+ 4 <>)
(+ 5 <>)

(+ 4 6)
(+ 5 <>)

10
(+ 5 <>)

(+ 5 10)

15

It looks like a stack. In some models of computation, the continuation /is/ the stack.
The continuation is more general and abstract. It is induced by the model and may not take 
the form of a stack conceptually.

Now suppose I had the ability to annotate the continuation with whatever I wanted.

[simple, illustrative example here]

Keeping with tradition, suppose we had a function

(launch-the-missile)

and we can call it from the clearance function:

(clearance (launch-the-missile))

and the no-clearance function:

(no-clearance (launch-the-missile))

with obvious behavior.

(launch-the-missile)
(clearance <>) "do whatever you like"

(launch-the-missile)
(no-clearance <>) "you can do nothing"

(launch-the-missile)
(clearance <>) "do whatever you like"
(no-clearance <>) "you can do nothing" (including grant clearance)

A correct clearance implementation would check to see if it could grant clearance.

Key is used in the sense of maps but what if it were a cryptographic key? Since we have to 
explicitly state the keys we want to use, what if launch-the-missile looked for, say, an 
encryption of a periodically changing value. if it found it, it would know that fresh 
clearance was given. if it didn't, then it would know that either clearance wasn't given 
or it wasn't fresh. of course systems aren't designed by this, but is there any use for 
such a scheme?

This gives us context-aware behavior. 

Example

(w-c-m key-expr value-expr body-expr)
(w-c-m 'key 'outer (if x (wcm 'key 'inner (c-c-m '(key)))))

Useful for fluid let values, debuggers, profilers, steppers.

Explain use in fluid lets and steppers.
|#
