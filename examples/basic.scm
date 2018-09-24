;; this

(define fuzz '(1 2 3))

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
;; FIXME: a bug when inserting comments between expressions
(if (or t nil)
    (print "yeah")
    (print "no"))

(if t nil t)				; nil
