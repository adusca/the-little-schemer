#lang r5rs

(define add1
  (lambda (n)
    (+ n 1)))

(display (add1 67))
(newline)

(define sub1
  (lambda (n)
    (- n 1)))

(display (sub1 5))
(newline)

(define o+
  (lambda (n m)
    (cond 
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(display (o+ 12 13))
(newline)

(define o-
  (lambda (n m)
    (cond 
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(display (o- 13 12))
(newline)

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else 
      (o+ (car tup) (addtup (cdr tup)))))))

(display (addtup '(1 2 3 4 5)))
(newline)

(define o*
  (lambda (n m)
    (cond 
     ((zero? m) 0)
     (else (o+ n (o* n (sub1 m)))))))

(display (o* 13 12))
(newline)

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(display (tup+ '(3 7) '(4 6 8 1)))
(newline)

(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))

(display (o> 3 3))
(newline)

(define o<
  (lambda (n m)
    (o> m n)))

(display (o< 3 1))
(newline)

(define o=
  (lambda (n m)
    (cond
     ((o> n m) #f)
     ((o< n m) #f)
     (else #t))))

(display (o= 3 3))
(newline)

(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (o* n (^ n (sub1 m)))))))

(display (^ 3 3))
(newline)

(define o/
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else (add1 (o/ (o- n m) m))))))

(display (o/ 15 4))
(newline)

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(display (length '(ham and cheese on rye)))
(newline)

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(display (pick 4 '(lasagna spaghetti ravioli macaroni meatball)))
(newline)

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat))
      (no-nums (cdr lat)))
     (else (cons (car lat)
		 (no-nums
		  (cdr lat)))))))

(display (no-nums '(a 1 b 2 c 3)))
(newline)

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat))
      (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(display (all-nums '(a 1 b 2 c 3)))
(newline)

(define eqan?
  (lambda (a1 a2)
  (cond
   ((and (number? a1) (number? a2))
    (o= a1 a2))
   ((or (number? a1) (number? a2)) #f)
   (else (eq? a1 a2)))))

(display (eqan? 2 2))
(newline)

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? (car lat) a) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(display (occur 'a '(a b b a)))
(newline)

(define one?
  (lambda (n)
    (o= n 1)))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
		 (rempick (sub1 n)
			  (cdr lat)))))))

(display (rempick 4 '(a b c d e f g)))
(newline)

