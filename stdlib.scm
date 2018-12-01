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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
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

(define square
  (lambda (x)
    (* x x)))

(define cube
  (lambda (x)
    (* x (square x))))

(define half
  (lambda (x)
    (/ x 2)))

(define not
  (lambda (x)
    (nil? x)))

(define pair?
  (lambda (x)
    (not (nil? (cdr x)))))

(define map
  (lambda (callback lst)
    (if (nil? lst)
	'()
	(cons (callback (car lst))
	      (map callback (cdr lst))))))

(define in
  (lambda (lst foo)
    (let loop ((res '()) (current lst))
	 (if (not (list? current))
	  res
	  (if (eq? (car current) foo)
	      (set res t)
	      (loop res (cdr current)))) res)))

(define append
  (lambda (lst foo)
    (begin
	 (if (nil? (cdr lst))
	  (set-cdr lst foo)
	  (append (cdr lst) foo))
	 lst)))

(define append-to
  (lambda (lst foo)
    (begin
	 (append lst (cons foo '()))
	 lst)))

(define range
  (lambda (init size inc)
    (let loop ((n init))
	 (if (> n size)
	  '()
	  (cons n (loop (+ n inc)))))))

(define fib
  (lambda (n)
    (if (< n 0)
	'()
	(if (or (= n 0) (= n 1) )
	    1 (+ (fib (- n 2)) (fib (- n 1)))))))

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
;;   y)

;; (+ 5 10 (* 7 8 4 (- 7 9) (/ 8 4)) 10)
;; (* 55 88)

;; (define x (+ 1 2))
;; (print x)
;; (symbol? '())
;; (define add-five (lambda (n) (+ 5 n)))
;; (print (add-five x))

;; (map add-five '(1 2 3))
;; (define x ((lambda (n) (+ 1 n)) 5))
;; (define x (+ 1 5))
;; (print (+ x x))

;; (map (lambda (n) (+ 1 n)) '(1 2 3))

;; (let ((x '(1 2 8)))
;;   (map add-five x))

;; (+ 55 88)
;; (fib 15)

;; (define x (+ 8 7))
