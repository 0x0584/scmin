;; this

(define fuzz '(1 2 3))			; (1 2 3)

(set 'fuzz '(5 2 4))			; (5 2 4)
(setq fuzz (3 2 4))			; (3 2 4)
(set (car fuzz) 7)			; 7

(define here '(a b c d))		; (a b c d)
(undef here)				; t
(null? here)				; t

(atom? fuzz)				; nil
(atom? (car fuzz))			; t
(atom? (cdr fuzz))			; nil

(symbol? 'a)				; t
(symbol? 1)				; nil

(and t t)				; t
(and t nil)				; nil
(and nil t)				; nil
(and nil nil)				; nil

(or t t)				; t
(or t nil)				; t
(or nil t)				; t
(or nil nil)				; nil

(not nil)				; t
(not t)					; nil

(and (or 1 2) (not (or nil '())))	; t

;; this would print yeah
;; XXX: a bug when inserting comments between expressions
(if (or t nil)
    (print "yeah")
    (print "no"))			; "yeah"
(if t nil t)				; nil
