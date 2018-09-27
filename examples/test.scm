;; This is a test file

(define x (+ 5 4))
(define y (+ 1 (square x)))
(define z (if (> x y) (list x (+ x 1) (+ x 2)) (* y 2)))

(print z)
