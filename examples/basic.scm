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
