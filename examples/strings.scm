;; FIXME: implement those functions
;; string is an elementary datatype, it represents the usual
;; idea of character sequence, which are stored in a c-array

(string? "this is a string")		; t
(string? 'a)				; nil
(quote a)

;; [ ] converting to string
(as-string 4)				; "4"
(as-string '(a b c))			; "(a b c)"

(string? (as-string 4))			; t
(string? (as-string '(a b c)))		; t

;; [ ] concatenating strings
(string-cat "Hello" ", " "World")	; "Hello, World"

(define strs '("aaa" "BBB" "ccc"))	; ("aaa" "BBB" "ccc")
(string-cat strs)			; "aaaBBBccc"

;; [ ] copying strings cretes a new string
(define str-old "this is old")		; "this is old"
(define str-new (string-cpy str-old))	; "this is old"

;; [ ] length of a string
(length? "this is another string!")	; 23

;; [ ] regular expresion on string
(regex "*a$" "Ana mada Chadan")		; ("Ana" "mada")
(regex "[:num:]" "this is really bad")	; nil
