#lang slideshow

(require slideshow/base
         slideshow/code
         slideshow/pict)

(define (underline p)
  (refocus
   (vc-append p (colorize (linewidth 4 (hline (pict-width p) 0)) "red"))
   p))

(define (strike p)
  (refocus
   (cc-superimpose p (colorize (linewidth 4 (hline (pict-width p) 0)) "red"))
   p))

(slide
 #:title "Thesis"
 (para "A CPS-like global transformation can compile the λ-calculus with"
       "continuation marks to the plain λ-calculus in a semantics-preserving way."))
; maybe read the slide in this case
; "this statement may call for some unpacking"

(slide
 #:title "Thesis"
 (para "A CPS-like global transformation can compile the" (underline (t "λ-calculus")) "with"
        "continuation marks to the plain" (underline (t "λ-calculus")) "in a semantics-preserving way."))
; "First, let's discuss the λ-calculus."

(slide
 #:title "The λ-calculus"
 (t "a formal system for expressing computation")
; "Other formal systems include... (mention monads?)"
 'next
 (t "consists of")
 (item (para (t "variables") (tt "x") (t ",") (tt "y") (t ",") (tt "z") (t ",...")))
; "There are an infinite number of variables and they are compared syntactically."
 'next
 (item (para (t "abstractions") (tt "λx.M")))
; "An abstraction corresponds to a function; it binds variables for substitution."
 'next
 (item (para "applications" (tt "M N"))))
; "'application' refers to 'function application'. The process of application is called 'reduction'. It corresponds to 'evaluation'."

(slide
 #:title "Example"
 (para (tt "λx.x λy.y"))
 'next
 (para (tt "-> λy.y")))

(slide
 #:title "Thesis"
 (para "A CPS-like global transformation can compile the λ-calculus with"
       (underline (t "continuation marks")) "to the plain λ-calculus in a semantics-preserving way."))
 
(slide
 #:title "Thesis"
 (para "A" (underline (t "CPS-like")) "global transformation can compile the λ-calculus with"
       "continuation marks to the plain λ-calculus in a semantics-preserving way."))

(slide
 #:title "CPS-like"
 (para "'CPS' abbreviates 'continuation-passing style'")
 'next
 (para "the continuation represents the \"rest of the computation\""))

(slide
 #:title "Continuation Example"
 (para "Consider the evaluation of" (tt "(+ 1 (* 2 3))") ".")
; "this says, multiply 2 by 3 and add 1 to the result"
 'next
 (para "The continuation of" (tt "(* 2 3)") "is" (tt "(+ 1 •)") "."))
; "it represents the rest of the computation after the subexpression (* 2 3) is evaluated."
; "the hole represents where that value will be placed."

(slide
 #:title "Continuation example"
 (para (tt "(+ 1 •)"))
; "the continuation (+ 1 •) awaits a value to perform a computation."
 'next
 (para "This is like" (tt "fun(x) -> x+1") "."))
; "this is like the function that takes x and adds 1 to it, only this function doesn't return the value back to the caller"

(slide
 #:title "Continuation-passing style"
 'alts
 (list (list (para (strike (tt "fun(x) -> return x+1"))))
; "continuation-passing style is a style in which functions never return a value."        
       (list (para (strike (tt "fun(x) -> return x+1")))
             (para (tt "fun(x,k) -> k(x+1)")))))
; "instead, they pass the computed value to another argument, the continuation"

(slide
 #:title "Continuation-passing style"
 (para "In continuation-passing style," (text "every" (cons 'italic 'default) (current-font-size)) ; the current-font-size is /not/ the current font size
       "function is given an additional formal parameter representing the current continuation."))

(slide
 (para "Why continuation-passing style?")
; "There are an immense number of benefits of CPS (see Appel), but we use it almost exclusively for this:
 'next
 (para "It allows control over evaluation order.")
; "I will discuss this in a few minutes."
 
(slide
 #:title "Continuation marks"
 (para "a programming language feature that allows us to annotate the stack"))
 

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
