;Chase Hoselle
;Prog 3

;length
(define (length l)
	(cond ((null? l) 0)
		((+ 1 (length (cdr l))))))
;list
(define (list . l)
  (cond (null? l) '()
  1))
;append
(define (append l1 l2)
	(cond ((null? l1) l2)
		((cons (car l1)
			(append (cdr l1) l2)))))
;not
(define (not n)
  (if n #f #t))
;and
(define (and . l)
  (if (null? (cdr l)) (car l) 
    (if (eq? (car l) #f) #f (apply and (cdr l)))))
;or
(define (or . l) (if (eq? (car l) #f) (apply or (cdr l)) (car l)))

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
;map
(define (map n l)
  (cond ((null? l) '())
    (cons (n (car l))
      (map n (cdr l)))))
;zero?
(define (zero? n)
  (cond ((null? n) #f)
        ((= n 0) #t)
        (else #f)))
;positive?
(define (pos? n)
  (cond ((null? n) #f)
        ((> n 0) #t)
        (else #f)))
;negative?
(define (neg? n)
  (cond ((null? n) #f)
        ((< n 0) #t)
        (else #f)))
;odd?
(define (odd? n)
  (if (eqv? n 0) #f
      (even? (- n 1))))
;even?
(define (even? n)
  (if (eqv? n 0) #t
      (odd? (- n 1))))
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