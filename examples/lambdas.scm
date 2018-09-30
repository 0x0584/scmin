;; this is recursive factorial -- NOT SUPPORTED YET
;; (define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
;; (fact 5)
;; there are many ways to define a lambda
;; using define keyword
(define foo (lambda (x y) (if (< x y) (+ x y) (* x y))))

;; and then calling them using as expression
;; (bar 3 2)				; 5
;; (baz 2 4)				; 6
(foo 7 5)				; 3
(foo 5 7)				; 3

;; and some syntactic sugar expressions
;; (define (baz x y) (+ x y))		; baz
;; (defun bar (x y) (+ x y))		; bar
;; or lambdas can be used directly
((lambda (a b) (* a b)) 5 3)		; 15
