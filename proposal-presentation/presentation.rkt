#lang slideshow

(slide
  #:title "Thesis"
  (para "A CPS-like global transformation can compile the λ-calculus with"
        "continuation marks to the plain λ-calculus in a semantics-preserving way."))

#|
Introduce continuation marks

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