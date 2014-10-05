#lang r5rs

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(display (lat? '(Alguma coisa aqui)))
(newline)
(display (lat? '(falso (a))))
(newline)
