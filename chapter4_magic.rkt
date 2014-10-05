#lang r5rs

(define add1
  (lambda (n)
    (cons 'x n)))

(define sub1 cdr)

(define ozero? null?)

(define zero '())
(define one (add1 zero))
(define two (add1 one))
(define three (add1 two))
(define four (add1 three))
(define five (add1 four))

(define o+
  (lambda (n m)
    (cond 
     ((ozero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond 
     ((ozero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) zero)
     (else 
      (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (n m)
    (cond 
     ((ozero? m) zero)
     (else (o+ n (o* n (sub1 m)))))))

(display (o* three two))
(newline)

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
     ((ozero? n) #f)
     ((ozero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (o> m n)))

(define o=
  (lambda (n m)
    (cond
     ((o> n m) #f)
     ((o< n m) #f)
     (else #t))))


(define ^
  (lambda (n m)
    (cond
     ((ozero? m) (add1 zero))
     (else (o* n (^ n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
     ((o< n m) zero)
     (else (add1 (o/ (o- n m) m))))))

(display (^ four three))
(newline) 
