;; there are many ways to define a lambda
;; using define keyword
(define foo (lambda (x y)
	      (if (< x y) (+ x y) (* x y))))
(foo 7 5)				; 3
(foo 5 7)				; 3

;; or lambdas can be used directly
((lambda (a b) (* a b)) 5 3)		; 15

;; TODO: add some syntactic sugar expressions
;; this is also not working
;; (defun fuzz (x y) (+ x y))		; bar
;; (defun (buzz x y) (- x y))		; bar
;; (define (baz x y) (+ x y))		; baz

;; XXX: the factorial is not working
;; due to some scope binding issues
(define fact (lambda (n)
	       (if (n <= 1) 1 (* n (fact (- n 1))))))
(fact 4)
