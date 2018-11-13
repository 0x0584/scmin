;; This is teh standard library provided by the scmin Interpreter
;; this file contains definitions to Scheme/Lisp functions that
;; does not need to be implemented in pure C, instead, it makes
;; real sense to be written in Scheme/Lisp

(define caar (lambda (foo) (car (car foo))))
(define cadr (lambda (foo) (car (cdr foo))))
(define cdar (lambda (foo) (cdr (car foo))))
(define cddr (lambda (foo) (cdr (cdr foo))))

(define caddr (lambda (foo) (car (cddr foo))))
(define cddar (lambda (foo) (cddr (car foo))))

(define caadr (lambda (foo) (car (cadr foo))))
(define cdaar (lambda (foo) (cdr (caar foo))))

(define square (lambda (x) (* x x)))
(define cube (lambda (x) (* x (square x))))
(define half (lambda (x) (/ x 2)))

(define not (lambda (x) (if (nil? x) t nil)))
(define pair? (lambda (x) (not (nil? (cdr x)))))

(define map (lambda (callback lis)
	      (if (nil? lis)
		  '()
		  (cons (callback (car lis))
			(map callback (cdr lis))))))

(define fib (lambda (n)
	      (if (= n 0) 1 (if (= n 1) 1 (+ (fib (- n 2)) (fib (- n 1)))))))

(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))

(define add-five (lambda (a) (+ a 7)))	; not really
