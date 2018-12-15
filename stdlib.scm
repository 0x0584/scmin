;;; stdlib.scm ---- Summary: Standard Scheme Library
;;
;; Filename: stdlib.scm
;; Description: Standard Scheme Library for the `scmin' Interpreter
;; Author: Anas (0x0584)
;; Created: <2018-11-12 Mon 23:11:29>
;; Updated: <2018-12-15 Sat 02:40:17>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;	 Standard Scheme Library
;;
;; Summary:
;;
;;	 This is the standard library provided by the scmin Interpreter
;;	 this file contains definitions to Scheme/Lisp functions that
;;	 does not need to be implemented in pure C, instead, it makes
;;	 real sense to be written in Scheme/Lisp
;;
;;	Library functions defined here:
;;
;;	  (c[ad]+r lst) -- call car and cdr based on the pattern; it is
;;	more clear to write (cadr lst) than (car (cdr (car lst)))
;;
;;	  (map callback lst) -- map returns a list where callback is
;;	mapped to all lst objects.
;;
;;	  (in lst obj) -- returns t if obj is in lst, or nil otherwise
;;
;;	  (append lst obj) -- sets the cdr of lst to obj and returns lst
;;
;;	  (append-to lst obj) -- same as append except it sets the cdr
;;	of lst to (obj) instead i.e. (obj . nil) also returns lst
;;
;;	Other Functions defined here:
;;
;;	  Identity functions: `pair?'
;;
;;	  Logical functions: `not'
;;
;;	  Numerical functions: `half', `square', `cube', `fact', `fib'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; FIXME: add condition in sexpr_print() based ot the value
;;;
;; if numerical output format is scientific, then the outpout
;; would be justified, otherwise the interpreter would show
;; any number as is.
;;;
(define *numeric-style* 'scientific)

;; TODO: implment a (power x y) such as x^y
;;;
;; Function: (square x)
;; (square 2.9) => 8.41
;; -----------------------------
;; return the square of `x'
;;;
(define (square x) (* x x))

;;;
;; Function: (cube x)
;; (cube 5) => 125
;; -----------------------------
;; return the cube of `x'
;;;
(define (cube x) (* x x x))

;;;
;; Function: (half x)
;; (half 0.25)
;; -----------------------------
;; return the half of `x'
;;;
(define (half x) (/ x 2))

;;;
;; Function: (not foo)
;; (not 'a) => nil
;; (not '()) => t
;; -----------------------------
;; since nil is the only value counted as false, (not foo) would
;; return t only if (nil? foo) return t.
;;
;; return t if `foo' is nil, or nil otherwise
;;;
(define (not foo) (nil? foo))

;;;
;; Function: (pair? foo)
;; (pair? 'a) => nil
;; (pair? '(a b)) => nil
;; (pair? '(cons a b)) => t
;; -----------------------------
;; a pair is is fundamental data type in Scheme, it constists of a pair
;; of a car and cdr. Lists are built using many nested pairs, and always
;; terminated with a nil. while pairs has a cdr which is always not nil.
;; thus, a pair is any object that has a its cdr not a list and not nil.
;;
;; return t if `foo' is a pair, i.e. (a . b) or (cons a b)
;;;
(define (pair? foo)
  (and (not (list? (cdr foo)))
	   (not (nil? (cdr foo)))))

;; FIXME: apply callback on lst if it was an atom too
;;		  sounds reasonable, right?
;;;
;; Function: (map callback lst)
;; (map (lambda (n) (+ 5 n)) '(1 2 3)) => (6 7 8)
;; -----------------------------
;; map is a common functioanlity in Lisp dirivitives, all it does is
;; creating a new list in which we apply `callback' on all the
;; elements of `lst`.
;; return a list of `callback' applied on `lst`
;;;
(define (map callback lst)
  (if (nil? lst)
	  nil
	  (cons (callback (car lst))
			(map callback (cdr lst)))))

;;;
;; Function: (in lst foo)
;; (in '(1 (2 1 2) (1 2 3) 3) '(1 2 3)) => t
;; -----------------------------
;; this is a dummy implementation since the return mechanism in
;; not implemented yet, so we have to iterate over the whole `lst'
;; to find `foo', and even if we have found it, we need to wait
;; until we reach the end, and then return the value.
;; return t if `foo' is in `lst', or nil otherwise
;;;
(define (in lst foo)
  (let loop ((res nil) (current lst))
	(if (not (list? current))
		res
		(if (eq? (car current) foo)
			(set res t)
			(loop res (cdr current))))))

;;;
;; Function: (append lst foo)
;; (append '(1 2 3) '(1 2)) => (1 2 3 1 2)
;; -----------------------------
;; return `lst' with `foo' appended at the end. note that append
;; sets the cdr of `lst`
;;;
(define (append lst foo)
  (begin
	(if (nil? (cdr lst))
		(set-cdr lst foo)
		(append (cdr lst) foo))
	lst))

;;;
;; Function: (append-to lst foo)
;; (append-to '(1 2 3) '(1 2)) => (1 2 3 (1 2))
;; --------------------------------
;; return `lst' with `foo' appended at the end as (foo). note that append
;; sets the cdr of `lst`
;;
;;;
(define (append-to lst foo)
  (begin
	(append lst (cons foo nil)) lst))

;;;
;; Function: (range init size inc)
;; (range -1 2 0.3) => (-1 -0.7 -0.4 -0.1 0.2 0.5 0.8 1.1 1.4 1.7 2)
;; ------------------------------------
;; return a list with all the elements within the range of `init'
;; and `size' incriminating by `inc'.
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
;; (fib 100) => 3.54225e+20
;; -----------------------------
;; return Fibonacci of the `n'-th element. this is an efficient
;; implementation since it carries the results while evaluating
;; so that Fibonacci won't reevaluated already calculated results
;;;
(define (fib n)
  (let loop ((tmp n) (n0 0) (n1 1))
	(cond ((> 0 tmp) nil)
		  ((= 0 tmp) n0)
		  ((= 1 tmp) n1)
		  (else (loop (- tmp 1) n1 (+ n0 n1))))))

;; Function: (fact n)
;; (fact 100) => 9.33262e+157
;; -----------------------------
;; return factorial of `n'.
;;;
(define (fact n)
  (cond ((<= n 1) 1)
		(else (* n (fact (- n 1))))))


;; used in testings

;; (let ((a '()))
;;	 (let ((b 4) (c 5))
;;	   (list a b c)))


;; testings

;; (let* ((foo 3) (y foo))
;;	 (+ foo y))
