;Chase Hoselle
;Prog 3

;length
(define (length l)
	(cond ((null? l) 0)
		((+ 1 (length (cdr l))))))

;list
(define (list . l)
  (if (null? l) '()
  l))

;append
(define (append l1 l2)
	(cond ((null? l1) l2)
		((cons (car l1)
			(append (cdr l1) l2)))))

;not
(define (not x)
  (if x #f #t))

;and
(define (and . l)
  (if (null? (cdr l)) (car l) 
    (if (eq? (car l) #f) #f (apply and (cdr l)))))

;or
(define (or . l) 
  (if (eq? (car l) #f) (apply or (cdr l)) (car l)))

;reverse
(define (reverse l)
  (if (null? l) '()
    (append (reverse (cdr l)) (list (car l)))))

;memq
(define (memq x l)
  (if (null? l) #f
    (cond ((or (eq? x (car l)) (member x (cdr l))) l)
  (else #f))))

;memv
(define (memv x l)
  (if (null? l) #f
    (cond ((or (eqv? x (car l)) (member x (cdr l))) l)
  (else #f))))

;member
(define (member x l)
  (if (null? l) #f
    (cond ((or (equal? x (car l)) (member x (cdr l))) l)
  (else #f))))

;assq
(define (assq x l)
  (if (null? l) #f
    (cond ((not (pair? (car l)) ) #f)
    ((and (eq? x (car l)) (assq x (cdr l))) (car l))
    (else #f))))

;assv
(define (assv x l)
  (if (null? l) #f
    (cond ((not (pair? (car l)) ) #f)
      ((and (eqv? x (car l)) (assv x (cdr l))) (car l))
      (else #f))))

;assoc
(define (assoc x l)
  (if (null? l) #f
    (cond ((not (equal? (car l)) ) #f)
      ((and (equal? x (car l)) (assoc x (cdr l))) (car l))
      (else #f))))

;map
(define (map x l)
  (cond ((null? l) '())
    (cons (x (car l))
      (map x (cdr l)))))

;for-each
(define (for-each x l)
  (cond ((null? (cdr l))
    (x (car l)))
  (else
    (x (car l))
    (for-each x (cdr l)))))

;zero?
(define (zero? x)
  (cond ((null? x) #f)
    ((= x 0) #t)
    (else #f)))

;positive?
(define (positive? x)
  (cond ((null? x) #f)
    ((> x 0) #t)
    (else #f)))

;negative?
(define (negative? x)
  (cond ((null? x) #f)
    ((< x 0) #t)
    (else #f)))

;abs
(define (abs x)
  (cond ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))))

;odd?
(define (odd? x) 
  ((cond ((= x 0) #f) ((= x 1) #t) 
      (else (odd? (abs (- 2 x)))))))

;even?
(define (even? x) 
  (not (odd? x)))

;max
(define (max . l)
  (if (> (car l) (car (cdr l))) (car l)
    (max (cdr l))))

;min
(define (min n)
  (cond ((null? (cdr n) (car n))
    ((< (car n) (min (cdr n))) (car n))
    (else (min (cdr n))))))

;+
(define (+ . l)
  (if (null? l) 0
    (b+ (car l) (apply + (cdr l)))))

;-
(define (- . l)
  (if (null? l) 0
    (b- (car l) (apply - (cdr l)))))

;*
(define (* . l)
  (if (null? l) 0
    (b* (car l) (apply * (cdr l)))))

;eqv? 
(define (eqv? x y)
  (if (and (number? x) (number? y)) (= x y) (eq? x y)))  
    
;equal?
(define (equal? x y)
  (cond ((eqv? x y) #t)
    ((and (pair? x) (pair? y) (equal? (car x) (car y)) (equal? (cdr x) (cdr y))) #t)
    (else #f)))

;=
(define (= . l)
  (if (null? l) 0
    (cond 
      ((zero? (- (car l) (car (cdr l)))))
      (else #f))))

;<
(define (< . l)  
  (if (null? l) 0
    (cond
      ((negative? (- (car l) (car (cdr l)))))
      (else #f))))

;>
(define (> . l)
  (if (null? l) 0
    (cond
      ((positive? (- (car l) (car (cdr l)))))
      (else #f))))

;<=
(define (<= . l)
  (if (null? l) 0
    (cond
      ((zero? (- (car l) (car (cdr l)))))
      ((negative? (- (car l) (car (cdr l)))))
      (else #f))))

;>=
(define (>= . l)
  (if (null? l) 0
    (cond
      ((zero? (- (car l) (car (cdr l)))))
      ((positive? (- (car l) (car (cdr l)))))
      (else #f))))

;list functions
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