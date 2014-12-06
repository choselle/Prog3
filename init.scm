;length
(define (length l)
	(cond ((null? l) 0)
		((+ 1 (length (cdr l))))))
;append
(define (append l1 l2)
	(cond ((null? l1) l2)
		((cons (car l1)
			(append (cdr l1) l2)))))
;reverse
(define (reverse l)
  (if (null? l) '()
     (append (reverse (cdr l)) (list (car l)))))
;memq
(define (memq n l)
  (if (null? l) #f
      (cond ((or (eq? n (car l)) (member n (cdr l))) l)
            (else #f))))
;memv
(define (memv n l)
  (if (null? l) #f
      (cond ((or (eqv? n (car l)) (member n (cdr l))) l)
            (else #f))))
;member
(define (member n l)
  (if (null? l) #f
      (cond ((or (equal? n (car l)) (member n (cdr l))) l)
            (else #f))))
;assq
(define (assq n l)
  (if (null? l) #f
      (cond ((not (pair? (car l)) ) #f)
            ((and (eq? n (car l)) (assq n (cdr l))) (car l))
            (else #f))))
;assv
(define (assv n l)
  (if (null? l) #f
      (cond ((not (pair? (car l)) ) #f)
            ((and (eqv? n (car l)) (assv n (cdr l))) (car l))
            (else #f))))
;assoc
(define (assoc n l)
  (if (null? l) #f
      (cond ((not (equal? (car l)) ) #f)
            ((and (equal? n (car l)) (assoc n (cdr l))) (car l))
            (else #f))))

;nodes
(define (caar l) (car (car l)))

(define (cadr l) (car (cdr l)))

(define (cdar l) (cdr (car l)))

(define (cddr l) (cdr (cdr l)))

(define (caaar l) (car (car (car l))))

(define (caadr l) (car (car (cdr l))))

(define (cadar l) (car (cdr (car l))))

(define (cdaar l) (cdr (car (car l))))

(define (caddr l) (car (cdr (cdr l))))

(define (cdadr l) (cdr (car (cdr l))))

(define (cddar l) (cdr (cdr (car l))))

(define (cdddr l) (cdr (cdr (cdr l))))

(define (caaadr l) (car (car (car (cdr l)))))

(define (caadar l) (car (car (cdr (car l)))))

(define (cadaar l) (car (cdr (car (car l)))))

(define (cadadr l) (car (cdr (car (cdr l)))))

(define (cdaadr l) (cdr (car (car (cdr l)))))

(define (cdaddr l) (cdr (car (cdr (cdr l)))))

(define (cddadr l) (cdr (cdr (car (cdr l)))))

(define (cadddr l) (car (cdr (cdr (cdr l)))))

(define (caaddr l) (car (car (cdr (cdr l)))))

(define (cdddar l) (cdr (cdr (cdr (car l)))))

(define (cddaar l) (cdr (cdr (car (car l)))))

(define (cdaaar l) (cdr (car (car (car l)))))

(define (cdadar l) (cdr (car (cdr (car l)))))

(define (caddar l) (car (cdr (cdr (car l)))))

(define (cddddr l) (cdr (cdr (cdr (cdr l)))))

(define (caaaar l) (car (car (car (car l)))))