#lang r5rs

; Some helper functions from previous chapters

(define add1
  (lambda (n)
    (+ n 1)))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define sub1
  (lambda (n)
    (- n 1)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define first car)

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define one?
  (lambda (n)
    (= n 1)))

; Now the chapter actually begins

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else (eq? a sorn)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(newline)
(display (looking 'coisa '(2 3 coisa 1)))
(newline)

; The most partial function

(define eternity
  (lambda (x)
    (eternity x)))

; (display (eternity '()))

(define shift
  (lambda (pair)
    (build (first (first pair))
	   (build (second (first pair))
		  (second pair)))))

(newline)
(display (shift '((1 2) (3 4))))
(newline)

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
		  (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (length* (first pora))
	 (length* (second pora)))))))

(newline)
(display (length* '((1 2) (3 4))))
(newline)

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (* (weight* (first pora)) 2)
	 (weight* (second pora)))))))

(newline)
(display (weight* '((1 2) (3 4))))
(newline)
(display (weight* '(1 (2 3))))
(newline)

(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora)
		  (shuffle (second pora)))))))

(newline)
(display (shuffle '((1 2) (3 4))))
(newline)
(display (shuffle '(1 (2 3))))
(newline)

(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (cond
       ((even? n) (C (/ n 2)))
       (else (C add1 (* 3 n))))))))

(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n)
	      (A n (sub1 m)))))))

; (display (A '(4 3)))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else add1 (length (cdr l))))))
    (mk-length mk-length))))

; Y combinator

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
