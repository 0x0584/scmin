;; build a list using cons
(cons 'a (cons 'b (cons 'c nil))) ; (a b c)

;; appliying car/cdr
(car (cons 'a (cons 'b (cons 'c nil))))	; a
(cdr (cons 'a (cons 'b (cons 'c nil))))	; (b c)
