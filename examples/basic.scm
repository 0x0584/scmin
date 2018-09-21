(define foo '(a b c))			; (quote (a b c))
(define bar (quote (a (b c) (d) f g)))	; (quote (a (b c) (d) f g))

(car foo)				; a
(car (cdr foo))				; b
(car bar)				; a
(car (cdr bar))				; (b c)
(cdr (cdr bar))				; ((d) f g)

(define num-1 8)			; 8
(define num-2 (+ 5 1))			; 6

(+ num-1 num-2)				; 14

(and num-1 num-2)			; t
(and num-1 num-2 '())			; nil
(and nil num-2)				; nil

(or num-1 nil)				; t
(or num-1 num-2)			; t
(or nil '())				; nil

(not num-1)
(not '())
(not nil)
(not t)

(define fuzz (cons 1 (cons 2 '())))

(pair? fuzz)
(atom? fuzz)
(atom? (car fuzz))
(car fuzz)

(diplay fuzz)
(set-car fuzz 5)
(diplay fuzz)
