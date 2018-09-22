;; this

(and nil t)
(and nil nil)
(and t t)
(and t nil)

(or nil t)
(or nil nil)
(or t t)
(or t nil)

(not nil)
(not t)

(define foo '(a b c))			; (quote (a b c))
(define bar (quote (a (b c) (d) f g)))	; (quote (a (b c) (d) f g))

(print foo)
(length foo)
(print bar)
(length bar)

(car foo)				; a
(car (cdr foo))				; b
(car bar)				; a
(car (cdr bar))				; (b c)
(cdr (cdr bar))				; ((d) f g)

(define fuzz (cons 1 (cons 3 (cons 2 '()))))
(define kuzz (list 1 2 3))


(print kuzz)
(length kuzz)

(print fuzz)
(length fuzz)

(set-car fuzz 5)
(print fuzz)
(length fuzz)

(set-cdr fuzz foo)
(print fuzz)
(length fuzz)

(set-cdr fuzz 8)
(print fuzz)
(length fuzz)

(set-car fuzz '(a b c))
(print fuzz)
(length fuzz)

(define buzz (cons 1 2))
(print buzz)
(length buzz)

(define baz (cons '(a v s) '(a s s)))
(print baz)

(string? "this is a test")
(string? 1)

(pair? fuzz)
(pair? buzz)
(list? fuzz)
(list? buzz)

(pair? (car fuzz))
(pair? (cdr fuzz))

(atom? fuzz)
(atom? (car fuzz))
(atom? (cdr fuzz))
