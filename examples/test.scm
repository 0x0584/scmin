;; TODO: implement those functions
;; string is an elementary datatype, it represents the usual
;; idea of character sequence, which are stored in a c-array

(string? "this is a string")		; t
(string? 'a)				; nil

(quote a)
(define p3 '((A B C D) E F (G H (I J K) (L M)) N) O)
;; (cdr (cdr (cdr (car (cdr (car p3))))))

(car (cdr (car (cdr (cdr (cdr p3))))))

(print "hahaha")
(print 1)
(print 'a)
(print p3)
