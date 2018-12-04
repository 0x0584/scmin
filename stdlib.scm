;;; stdlib.scm ---- Summary: Standard Scheme Library
;;
;; Filename: stdlib.scm
;; Description: Standard Scheme Library for the `scmin' Interpreter
;; Author: Anas (0x0584)
;; Created: Nov 12, 2018
;; Updated: Nov 20, 2018
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   Standard Scheme Library
;;
;; Summary:
;;
;;   This is the standard library provided by the scmin Interpreter
;;   this file contains definitions to Scheme/Lisp functions that
;;   does not need to be implemented in pure C, instead, it makes
;;   real sense to be written in Scheme/Lisp
;;
;;  Library functions defined here:
;;
;;    (c[ad]+r lst) -- call car and cdr based on the pattern; it is
;;	more clear to write (cadr lst) than (car (cdr (car lst)))
;;
;;    (map callback lst) -- map returns a list where callback is
;;	mapped to all lst objects.
;;
;;    (in lst obj) -- returns t if obj is in lst, or nil otherwise
;;
;;    (append lst obj) -- sets the cdr of lst to obj and returns lst
;;
;;    (append-to lst obj) -- same as append except it sets the cdr
;;	of lst to (obj) instead i.e. (obj . nil) also returns lst
;;
;;  Other Functions defined here:
;;
;;    Identity functions: `pair?'
;;
;;    Logical functions: `not'
;;
;;    Numerical functions: `half', `square', `cube', `fact', `fib'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANT ABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;
;; Function: (square x)
;; -----------------------------
;; return the square of `x'
;;;
(define square
  (lambda (x)
    (* x x)))

;;;
;; Function: (cube x)
;; -----------------------------
;; return the cube of `x'
;;;
(define cube
  (lambda (x)
    (* x x x)))

;;;
;; Function: (half x)
;; -----------------------------
;; return the half of `x'
;;;
(define half
  (lambda (x)
    (/ x 2)))
;;;
;; Function: (not x)
;; -----------------------------
;; return t if `x' is nil, or nil otherwise
;;;
(define not
  (lambda (x)
    (nil? x)))

;;;
;; Function: (pair? x)
;; -----------------------------
;; return t if `x' is a pair, i.e. (a . b) or (cons a b)
;;;
(define pair?
  (lambda (x)
    (not (nil? (cdr x)))))

;;;
;; Function: (map callback lst)
;; -----------------------------
;; return a list of `callback' applied on `lst`
;;
;;(map (lambda (n) (+ 5 n)) '(1 2 3)) => (6 7 8)
;;;
(define map
  (lambda (callback lst)
    (if (nil? lst)
	'()
	(cons (callback (car lst))
	      (map callback (cdr lst))))))

;;;
;; Function: (in lst foo)
;; -----------------------------
;; return t if `foo' is in `lst', or nil otherwise
;;
;; (in '(1 2 (1 2 3) 3) '(1 2 3)) => t
;;;
(define in
  (lambda (lst foo)
    (let loop ((res '()) (current lst))
      (if (not (list? current))
	  res
	  (if (eq? (car current) foo)
	      (set res t)
	      (loop res (cdr current)))))))

;;;
;; Function: (append lst foo)
;; -----------------------------
;; return `lst' with `foo' appended at the end. note that append
;; sets the cdr of `lst`
;;
;;(append '(1 2 3) '(1 2)) => (1 2 3 1 2)
;;;
(define append
  (lambda (lst foo)
    (begin
      (if (nil? (cdr lst))
	  (set-cdr lst foo)
	  (append (cdr lst) foo))
      lst)))

;;;
;; Function: (append-to lst foo)
;; --------------------------------
;; return `lst' with `foo' appended at the end as (foo). note that append
;; sets the cdr of `lst`
;;
;; (append-to '(1 2 3) '(1 2)) => (1 2 3 (1 2))
;;;
(define append-to
  (lambda (lst foo)
    (begin
      (append lst (cons foo '()))
      lst)))

;;;
;; Function: (range init size inc)
;; ------------------------------------
;; return a list with all the elements within the range of `init'
;; and `size' incriminating by `inc'.
;;
;; (range -1 2 0.3) => (-1 -0.7 -0.4 -0.1 0.2 0.5 0.8 1.1 1.4 1.7 2)
;;
;; note that something like (range 0 -5 -1) is not supported for now
;;;
(define range
  (lambda (init size inc)
    (let loop ((n init))
      (if (> n size)
	  '()
	  (cons n (loop (+ n inc)))))))

;;;
;; Function: (fib n)
;; -----------------------------
;; return Fibonacci of the `n'-th element. this is an efficient
;; implementation since it carries the results while evaluating
;; so that Fibonacci won't reevaluated already calculated results
;;
;; (fib 100) => 3.54225e+20
;;;
(define fib
  (lambda (n)
    (let loop ((tmp n) (n0 0) (n1 1))
      (cond ((> 0 tmp) nil)
	    ((= 0 tmp) n0)
	    ((= 1 tmp) n1)
	    (else (loop (- tmp 1) n1 (+ n0 n1)))))))

;; Function: (fact n)
;; -----------------------------
;; return factorial of `n'.
;;
;; (fact 100) => 9.33262e+157
;;;
(define fact
  (lambda (n)
    (if (<= n 1)
	1 (* n (fact (- n 1))))))


;; used in testings

;; (let ((a '()))
;;   (let ((b 4) (c 5))
;;     (list a b c)))


;; testings

;; (let* ((x 3) (y x))
;;   (+ x y))
