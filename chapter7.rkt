#lang r5rs

;Some auxiliary functions from previous chapters

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (equal? (car lat) a)
	       (member? a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) a) (multirember a (cdr lat)))
	    (else (cons (car lat)
			(multirember a
				     (cdr lat)))))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l))(firsts (cdr l)))))))

;Here the Chapter 7 functions actually begin

(define set?
  (lambda (lat)
    (cond 
     ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(display (set? '(algumas palavras aqui)))
(newline)
(display (set? '(mais algumais palavras mais)))
(newline)

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat)
		 (makeset
		  (multirember (car lat)
			       (cdr lat))))))))
(newline)
(display (makeset '(oranges and apples and pears and)))
(newline)

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
	   (subset? (cdr set1) set2))))))
(newline)
(display (subset? '(a b) '(a b c d)))
(newline)
(display (subset? '(a b c) '(a b d)))
(newline)

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1))))
(newline)
(display (eqset? '(a b) '(b a)))
(newline)
(display (eqset? '(a b) '(a c)))
(newline)

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2)
	       (intersect?
		(cdr set1) set2))))))
(newline)
(display (intersect? '(a b c d) '(d e f g)))
(newline)
(display (intersect? '(a b c) '(d e f)))
(newline)

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1)
	    (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(newline)
(display (intersect '(a b c) '(b c d)))
(newline)

(define union
  (lambda (set1 set2)
    (cond 
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1)
		 (union (cdr set1) set2))))))

(newline)
(display (union '(a b c) '(b c d)))
(newline)

(define diff
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (diff (cdr set1) set2))
     (else (cons (car set1)
		 (diff (cdr set1) set2))))))

(newline)
(display (diff '(a b c) '(b c d)))
(newline)

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
		       (intersectall (cdr l-set)))))))

(newline)
(display (intersectall '((a b c)(b c d)(c d a r))))
(newline)

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(newline)
(display (a-pair? '(1 2)))
(newline)
(display (a-pair? '(1 2 3)))
(newline)

;Auxiliary one-liners to build pairs

(define first car)

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

;Back to the main functions of the chapter

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
		 (revrel (cdr rel)))))))

(newline)
(display (revrel '((a 1) (b 2) (c 3))))
(newline)

(define one-to-one?
  (lambda (fun)
    (and (fun? fun)(fun? (revrel fun)))))

(newline)
(display (one-to-one? '((1 2) (3 4))))
(newline)
(display (one-to-one? '((1 2) (3 2))))
(newline)
