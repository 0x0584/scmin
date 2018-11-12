;; XXX: implement lambdas andtheir functions

((lambda (x y) (* x y)) 3 2)		; 6

(define f (lambda (x y) (+ x y)))	; (lambda (x y) (+ x y))
(defun g (x y) (* x y))			; (lambda (x y) (* x y))
