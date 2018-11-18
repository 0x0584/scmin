;; this files contains orginal scheme code
;; the task is to evaluate those expressions
;; in this interpreter

(let* ((var1 10)
       (var2 (+ var1 12))) this)

((lambda (var1 var2) this) 10 ((lambda (var1) (+ var1 12)) 10))

(define (hofstadter-male-female n)
  (letrec ((female (lambda (n)
		     (if (= n 0)
			 1
			 (- n (male (female (- n 1)))))))
	   (male (lambda (n)
		   (if (= n 0)
		       0
		       (- n (female (male (- n 1))))))))
    (let loop ((i 0))
      (if (> i n)
	  '()
	  (cons (cons (female i)
		      (male i))
		(loop (+ i 1)))))))

(hofstadter-male-female 8)

;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
(define (generate-one-element-at-a-time lst)

  ;; Hand the next item from a-list to "return" or an end-of-list marker
  (define (control-state return)
    (for-each
     (lambda (element)
       (set! return (call-with-current-continuation
		     (lambda (resume-here)
		       ;; Grab the current continuation
		       (set! control-state resume-here)
		       (return element)))))
     lst)
    (return 'you-fell-off-the-end))

  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time
  (define (generator)
    (call-with-current-continuation control-state))

  ;; Return the generator
  generator)

(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))

(generate-digit) ;; 0
(generate-digit) ;; 1
(generate-digit) ;; 2
(generate-digit) ;; you-fell-off-the-end

;; Cooperative multitasking using call-with-current-continuation
;; in 25 lines of scheme

;; The list of threads waiting to run. This is a list of one
;; argument non-returning functions (continuations, mostly)
;; A continuation is a non-returning function, just like (exit),
;; in that it never gives up control to whoever called it.

(define readyList '())

;; A non-returning function. If there is any other thread
;; waiting to be run, it causes the next thread to run if there
;; is any left to run, otherwise it calls the original exit
;; which exits the whole environment.
(define exit
  ;; The original exit which we override.
  (let ((exit exit))
    ;; The overriding function.
    (lambda ()
      (if (not (null? readyList))
	  ;; There is another thread waiting to be run.
	  ;; So we run it.
	  (let ((cont (car readyList)))
	    (set! readyList (cdr readyList))
	    ;; Since the readyList is only non-returning
	    ;; functions, this will not return.
	    (cont '()))
	  ;; Nothing left to run.
	  ;; The original (exit) is a non returning function,
	  ;; so this is a non-returning function.
	  (exit)))))

;; Takes a one argument function with a given
;; argument and forks it off.  The forked function's new
;; thread will exit if/when the function ever exits.
(define (fork fn arg)
  (set! readyList
	(append readyList
		;; This function added to the
		;; readyList is non-returning,
		;; since exit is non returning.
		(cons
		 (lambda (x)
		   (fn arg)
		   (exit)) '()))))

;; Gives up control for the next thread waiting to be run.
;; Although it will eventually return, it gives up control
;; and will only regain it when the continuation is called.
(define (yield)
  (call-with-current-continuation
   ;; Capture the continuation representing THIS call to yield
   (lambda (thisCont)
     ;; Stick it on the ready list
     (set! readyList
	   (append readyList
		   (cons thisCont '())))
     ;; Get the next thread, and start it running.
     (let ((cont (car readyList)))
       (set! readyList (cdr readyList))
       ;; Run it.
       (cont '())))))

(let* ((yin
	((lambda (cc) (display #\@) cc)
	 (call-with-current-continuation (lambda (c) c))))
       (yang
	((lambda (cc) (display #\*) cc)
	 (call-with-current-continuation (lambda (c) c)))))
  (yin yang))	      ; @*@**@***@****@*****@******@*******@********...

(define (find-first func lst)
  (call-with-current-continuation
   (lambda (return-immediately)
     (for-each (lambda (x)
		 (if (func x)
		     (return-immediately x)))
	       lst)
     #f)))

(find-first integer? '(1/2 3/4 5.6 7 8/9 10 11)) ; ===> 7
(find-first zero? '(1 2 3 4))			 ; ===> #f

;; functional programming:
(apply + '(1 2 3 4 5 6))

;; Sum of three rational real numbers and two rational complex numbers
(define x (+ 1/3 1/4 -1/5 -1/3i 405/50+2/3i))

(define a 10)
(define eval-aplus2 (delay (+ a 2)))
(set! a 20)
(force eval-aplus2)			; ===> 22
(define eval-aplus50 (delay (+ a 50)))
(let ((a 8))
  (force eval-aplus50))			; ===> 70
(set! a 100)
(force eval-aplus2)			; ===> 22

(define-syntax let
  (syntax-rules ()
    ((let ((var expr) ...) body ...)
     ((lambda (var ...) body ...) expr ...))))
