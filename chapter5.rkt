#lang r5rs

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
	(rember* a (cdr l)))
       (else (cons (car l)
		   (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
		  (rember* a (cdr l)))))))

(display (rember* 'cup '((coffe) cup ((tea cup) (and (hick)) cup))))
(newline)

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons old
	      (cons new
		    (insertR* new old
			      (cdr l)))))
       (else (cons (car l)
		   (insertR* new old
			     (cdr l))))))
     (else (cons (insertR* new old
			   (car l))
		 (insertR* new old
			   (cdr l)))))))

(display (insertR* 'b 'a '((a b) (c a s a)((a b)))))
(newline)

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
	     (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (+ (occur* a (car l))
	      (occur* a (cdr l)))))))

(display (occur* 'a '((a b b a) (c a s a)(a b c))))
(newline)

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new
	      (subst* new old (cdr l))))
       (else (cons (car l)
		   (subst* new old
			   (cdr l))))))
     (else
      (cons (subst* new old (car l))
	    (subst* new old (cdr l)))))))

(display (subst* 'b 'a '((a b c) (c a s a))))
(newline)

(define insertL*
  (lambda (new old l)
    (cond 
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new
	      (cons old
		    (insertL* new old
			      (cdr l)))))
       (else (cons (car l)
		   (insertL* new old
			     (cdr l))))))
     (else (cons (insertL* new old
			   (car l))
		 (insertL* new old
			   (cdr l)))))))

(display (insertL* 'b 'a '((a b) (c a s a)((a b)))))
(newline)

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a)
	  (member* a (cdr l))))
     (else (or (member* a (car l))
	       (member* a (cdr l)))))))

(display (member* 'b '((a b) (c a s a)((a b)))))
(newline)

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(display (leftmost '((a b) (c a s a)((a b)))))
(newline)

(define eqan?
  (lambda (a1 a2)
  (cond
   ((and (number? a1) (number? a2))
    (= a1 a2))
   ((or (number? a1) (number? a2)) #f)
   (else (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))
     
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(display (equal? '(a b) '(a b)))
(newline)

(define rember
  (lambda (s  l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l)
		 (rember s (cdr l)))))))

(display (rember 'alguma '(alguma coisa)))
(newline)
