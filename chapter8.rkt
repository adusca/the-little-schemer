#lang r5rs

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l)
		   ((rember-f test?) a 
		    (cdr l))))))))

(display ((rember-f eq?) 'tuna '(tuna salad) ))
(newline)

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-algo (eq?-c 'algo))

(newline)
(display (eq?-algo 'algo))
(newline)
(display (eq?-algo 'coisa))
(newline)
(display ((eq?-c 'algo) 'algo))
(newline)

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old)
	(seq new old (cdr l)))
       (else (cons (car l)
		   ((insert-g seq) new old
		    (cdr l))))))))
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define insertL (insert-g seqL))

(newline)
(display (insertL 'a 'b '(a b)))
(newline)

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insertR (insert-g seqR))

(newline)
(display (insertR 'a 'b '(a b)))
(newline)

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(newline)
(display (subst 'a 'b '(a b)))
(newline)

(define seqrem
  (lambda (new old l)
    l))

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(newline)
(display (rember 'a '(a b)))
(newline)

;some helper functions for value

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) +)
     ((eq? x '*) *)
     (else expt))))

;Finally, value:

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function
	(operator nexp))

       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))

(newline)
(display (value '(3 * 5)))
(newline)
(display (value '(3 ^ 3)))
(newline)
(display (value '(2 + 2)))
(newline)

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? a (car lat))
	((multirember-f test?) a 
			(cdr lat)))
       (else (cons (car lat)
		   ((multirember-f test?) a 
		    (cdr lat))))))))

(newline)
(display ((multirember-f eq?) 'a '(c a s a)))
(newline)

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else (cons (car lat)
		 (multiremberT test?
			       (cdr lat)))))))

(newline)
(display (multiremberT eq?-algo '(algo aqui algo ali)))
(newline)

(define a-friend
  (lambda (x y)
    ((null? y))))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
	      (cons 'algo seen))))

(define lastest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat)
    seen)))

(define last-friend
  (lambda (x y)
    (length x)))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a (cdr lat)
		      (lambda (newlat seen)
			(col newlat
			     (cons (car lat seen))))))
     (else
      (multirember&co a
		      (cdr lat)
		      (lambda (newlat seen)
			(col (cons (car lat) newlat)
			     seen)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR
			(cdr lat)
			(lambda (newlat L R)
			  (col (cons new
				     (cons oldL newlat))
			       (add1L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR
			(lambda newlat L R)
			(col (cons oldR (cons new newlat))
			     L (add1 R))))
     (else
      (multiinsertLR&co new oldL oldR
			(cdr lat)
			(lambda (newlat L R)
			  (col (cons (car lat) newlat)
			       L R)))))))
