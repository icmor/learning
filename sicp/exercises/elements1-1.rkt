#lang sicp

;--------------------------------------------------

;; Exercise 1.1

10
;; 10

(+ 5 3 4)
;; 12

(- 9 1)
;; 8

(/ 6 2)
;; 3

(+ (* 2 4) (- 4 6))
;; 6

(define a 3)
;;

(define b (+ a 1))
;;

(+ a b (* a b))
;; 19

(= a b)
;; #f

(if (and (> b a) (< b (* a b)))
    b
    a)
;; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; 16

(+ 2 (if (> b a) b a))
;; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; 16

;--------------------------------------------------

;; Exercise 1.2

(/ (+ 5 4 (- 2
             (- 3
                (+ 6 4/5))))
   (* 3
      (- 6 2)
      (- 2 7)))

;--------------------------------------------------

;; Exercise 1.3

(define (foo a b c)
  (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
        ((and (>= b a) (>= c a)) (sum-of-squares b c))
        ((and (>= c b) (>= a b)) (sum-of-squares c a))))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (square x)
  (* x x))

;--------------------------------------------------

;; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;--------------------------------------------------

;; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

;; (test 0 (p))
;; This helps to determine whether the interpreter uses applicative or normal
;; order evaluation. In applicative order, all formal parameters are
;; fully evaluated before being substituted in the procedure, which in this case
;; would result in an infinite loop caused by evaluating (p). Normal order
;; substitutes the expressions as they are which, because of the if special form,
;; causes (p) to never be evaluated.

;--------------------------------------------------

;; Exercise 1.6

;; The procedure is going to enter an endless loop. New-if is a procedure which
;; means all arguments get evaluated before executing the procedure. This is a
;; problem since one of the arguments is a recursive call to sqrt-iter which
;; depends on new-if to stop. This of course causes an infinite recursive call
;; trying to evaluate the arguments to the first new-if. With the if special-form
;; if the predicate is true then the second argument is not evaluated.

;--------------------------------------------------

;; Exercise 1.7

;; The test fails on small numbers because there will always be numbers so small
;; that a set precision (say 0.0001) is too big to get a valuable result (it will be
;; reached immediately or may be bigger than the number itself!)

;; For big enough numbers the test will fail because of number precision.
;; Numbers can only have a certain amount of precision which results in reaching a
;; point where the improve procedure will return the same values. When this value
;; is not close enough to the desired nubmer the program will be stuck in a loop.
;; Eg. 99999888887777766666.0 is converted to 9.999988888777777e+19
;; Try (sqrt 1.0d12) vs (sqrt 1.0d13)

(define (sqrt x)
  (define (compute guess old-guess)
    (if (= old-guess guess)
        guess
        (compute (improve guess) guess)))

  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))

  (compute (improve 1.0) 1.0))


;--------------------------------------------------

;; Exercise 1.8

(define (cube x)
  (define (compute guess old-guess)
    (if (= guess old-guess)
        guess
        (compute (improve guess) guess)))
  (define (improve guess)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  (compute (improve 1.0) 1.0))
