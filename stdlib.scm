;;; this would contain basic Scheme functions that
;;; doesn't need to be implemented in native code

(define caar (lambda (foo) (car (car foo))))
(define cadr (lambda (foo) (car (cdr foo))))
(define cdar (lambda (foo) (cdr (car foo))))
(define cddr (lambda (foo) (cdr (cdr foo))))

(define caddr (lambda (foo) (car (cddr foo))))
(define caadr (lambda (foo) (car (cadr foo))))

(define cdaar (lambda (foo) (cdr (caar foo))))
(define cddar (lambda (foo) (cddr (car foo))))
(define caar (lambda (foo) (car (car foo))))
