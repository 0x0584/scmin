;; number is an elementary datatype, all numeric values are
;; represented as doubles.

(number? 4)				; t
(number? "this is a string")		; nil

;; all arithmetic operators take n-arguments except
;; for division. it takes exactly two arguments.
(+ 1 2)					; 3
(- 1 2 2)				; -3
(* 1 2 2)				; 4
(/ 25 5)				; 5

;; operators could be nested
(+ 1 2 3 (- 8 5))			; 9
(/ (* 5 5) 5)				; 5
(+ (- 96 1) (+ 2 13 4))			; 144
(+ (- 3 (* 4 2)) 4 (+ 6 1) (/ 3 4))	; 6
(+ 11 (* 22 33))			; 737
(* 2 (+ 3 (* 6 2)))			; 30
(* (+ 5 5) (+ 3 (* (- 4 1) 2)))

;; some standard mathematical functions
(sqrt 4)				; 2
(square 2)				; 4
(sqrt (square 2))			; 2

;; defining numerical variables
(define num-1 8)			; 8
(define num-2 (+ 5 1))			; 6

;; using variables in operations
(+ num-1 num-2)				; 14
(+ num-1 (* 3 num-2))			; 26
