#lang r5rs

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a)(cdr lat))
     (else (cons (car lat)
		 (rember a (cdr lat)))))))

(display (rember 'and '(bacon lettuce and tomato)))
(newline)

(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l))(firsts (cdr l)))))))

(display (firsts '((a b c) (coisa alguma))))
(newline)

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons old
		   (cons new (cdr lat))))
	    (else (cons (car lat)
			(insertR new old
				 (cdr lat)))))))))

(display (insertR 'c 'b '(a b d)))
(newline)

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons new lat))
	    (else (cons (car lat)
			(insertL new old
				 (cdr lat)))))))))

(display (insertL 'c 'b '(a b d)))
(newline)

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons new (cdr lat)))
	    (else (cons (car lat)
			(subst new old
			       (cdr lat)))))))))

(display (subst 'c 'b '(a b d)))
(newline)

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((or (eq? (car lat) o1) (eq? (car lat) o2))
	     (cons new (cdr lat)))
	    (else (cons (car lat)
			(subst2 new o1 o2
				(cdr lat)))))))))

(display (subst2 'novo 'b 'c '(a b c)))
(newline)

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) a) (multirember a (cdr lat)))
	    (else (cons (car lat)
			(multirember a
				     (cdr lat)))))))))

(display (multirember 'cup '(coffee cup tea cup hick cup)))
(newline)

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons (car lat)
		   (cons new 
			 (multiinsertR new old 
				       (cdr lat)))))
	    (else (cons (car lat)
			(multiinsertR new old
				 (cdr lat)))))))))

(display (multiinsertR 'a 'b '(b c b c)))
(newline)

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons new 
		   (cons old 
			 (multiinsertL new old (cdr lat)))))
	    (else (cons (car lat)
			(multiinsertL new old
				 (cdr lat)))))))))
(display (multiinsertL 'a 'b '(b c b c)))
(newline)

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons new 
		   (multisubst new old (cdr lat))))
	    (else (cons (car lat)
			(multisubst new old
			       (cdr lat)))))))))

(display (multisubst 'c 'b '(a b d b)))
(newline)
