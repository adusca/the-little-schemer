#lang r5rs

; more on the Y combinator, from http://mvainer.livejournal.com/2897.html

(define almost-factorial
  (lambda (f)
    (lambda (n)
      (cond
       ((= n 0) 1)
       (else (* n (f (- n 1))))))))

(define almost-fibonacci
  (lambda (f)
    (lambda (n)
      (cond ((= n     0) 0)
	    ((= n 1) 1)
	    (else (+ (f (- n 1)) (f (- n 2))))))))

(define (part-factorial self)
  (let ((f (self self)))
    (lambda (n)
      (cond
       ((= n 0) 1)
       (else (* n (f (- n 1))))))))

; (define factorial (part-factorial part-factorial))

(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

(define factorial (Y almost-factorial))

(define fibonacci (Y almost-fibonacci))

(display (factorial 5))
(newline)
(display (fibonacci 9))
(newline)
