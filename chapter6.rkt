#lang r5rs

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
	   (numbered?
	    (car (cdr (cdr aexp)))))))))

(display (numbered? '(2 * 2)))
(newline)
(display (numbered? '(t + 2)))
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

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp)'+)
      (+ (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp)'*)
      (* (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp))))
     (else
      (exp (value (1st-sub-exp nexp)) 
	 (value (2nd-sub-exp nexp)))))))

(display (value '(3 + (4 * 5))))
(newline)

;This part implements arithmetic without numbers

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1 cdr)

(define sero? null?)

(define o+
  (lambda (n m)
    (cond 
     ((sero? m) n)
     (else (edd1 (o+ n (zub1 m)))))))

;1 + 1
(display (o+ '(()) '(())))
(newline)



