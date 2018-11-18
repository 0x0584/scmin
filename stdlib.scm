;; This is the standard library provided by the scmin Interpreter
;; this file contains definitions to Scheme/Lisp functions that
;; does not need to be implemented in pure C, instead, it makes
;; real sense to be written in Scheme/Lisp

(define caar  (lambda (foo) (car (car foo))))
(define cadr  (lambda (foo) (car (cdr foo))))
(define cdar  (lambda (foo) (cdr (car foo))))
(define cddr  (lambda (foo) (cdr (cdr foo))))
(define caddr (lambda (foo) (car (cddr foo))))
(define cddar (lambda (foo) (cdr (cdar foo))))
(define caadr (lambda (foo) (car (cadr foo))))
(define cdaar (lambda (foo) (cdr (caar foo))))

(define square (lambda (x) (* x x)))

(define not (lambda (x) (nil? x)))
(define pair? (lambda (x) (not (nil? (cdr x)))))
(define cube (lambda (x) (* x (square x))))
(define half (lambda (x) (/ x 2)))

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


;; used in testings

(define add-five
  (lambda (a)
    (+ a 7)))	; not really

(define f
  (lambda (n)
    (let ((x (add-five n)))
      (square x))))

((lambda (n)
   (+ n n)) 7)

(let ((foo '+))
  (let ((+ *))
    (eval (list foo 2 3))))

(define let-loop
  (lambda (init size inc)
    (let loop ((n init))
      (if (> n size)
	  '()
	  (cons n (loop (+ n inc)))))))
(let ((a '()))
  (let ((b 4) (c 5))
    (list a b c)))

;; testings
;; (let* ((x 3) (y x))
;;   y)
