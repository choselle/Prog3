(define builtin-x x)
(define (b+ x y) (builtin-+ x y))

(define builtin-- -)
(define (b- x y) (builtin-- x y))

(define builtin-= =)
(define (b= x y) (builtin-= x y))

(define builtin-< <)
(define (b< x y) (builtin-< x y))

(define builtin-> >)
(define (b> x y) (builtin-> x y))

(define builtin->= >=)
(define (b>= x y) (builtin->= x y))

(define builtin-<= <=)
(define (b<= x y) (builtin-<= x y))

(define builtin-* *)
(define (b* x y) (builtin-* x y))

(define builtin-/ /)
(define (b/ x y) (builtin-/ x y))

