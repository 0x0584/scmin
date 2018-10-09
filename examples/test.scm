;; cons-pair is an elementary datatype, it's just a cell that holds
;; two values, car and cdr for the head and the rest values respectively.
;; nesting cons-pair with adding a nil at the end would gives us a list.

;; `cons` creates a pair, which is not the same as a list
(define p0 (cons 1 2))			; (1 . 2)
(pair? p0)				; t
(list? p0)				; nil

(cons 1 2)				; (1 . 2)
(cons 1 (cons 2 nil))			; (1 2) <-> (1 . (2 . nil))

;; list are terminated with nil, always
(define p1 (cons 'A (cons 'B nil)))	; (A B)
(pair? p1)				; nil
(list? p1)				; t

;; another way to create lists
(define p2 (list 1 '(2 a b) 3))		; (1 '(2 a b) 3)
(pair? p2)				; t
(list? p2)				; t

;; using cons-pair operators
(car p0)				; 1
(cdr p1)				; B
(car (cdr p2))				; (2 a b)

;; setting car/cdr
(set-car p1 'a)				; t
(print p1)				; (5 2)

(set-car (cdr p1) '(a b c))		; t
(print p1)				; (5 (a b c))

(set-cdr p2 p1)				; t
(print p2)				; (1 (2 a b) 3 5 (a b c))

(define p3 '((a b c d) e f
	     (g h (i g k) (l m)) n))	; ((a b c d) e f (g h (i g k) (l m)) n)
(car (cdr (car (cdr (cdr (cdr p3))))))	; h

;; getting information on a list
(length p0)				; 2
(length p1)				; 2
(length p2)				; 2
(length (car p3))			; 4

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
