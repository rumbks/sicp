; 

`Molly

`(a b c)
(a b c)
(+ `4  `5) 
(define a 5)
(define c 6)
(list `a b)

(cdr `(`(a b) c)) 

(eq? `(`a `b) `(`a `b))
(eq? 1 1)
(eq? `a `a)
(eq? `abc `abc)
(eq? #f #f)
(eq? (cons 1 2) (cons 1 2))

(`a `b)
`(a `b)

; 2.53

(list `a `b `c)

(list (list `george))

(cdr `((x1 x2) (y1 y2)))

`(`a `b)

`(a b)

`a
(car ``a ) 
```a

(pair? (car `(a short list)))
(= 1 2)

(eq? 1 (list 2))

; 2.54

(define (equal? a b) 
  (if (eq? a b) #t 
    (and (equal? (car a) (car b)) 
         (equal? (cdr a) (cdr b)))))
(equal? `(a b) `(a b))
(equal? ``a ``a)
(equal? `(`a `b) `(`a `b))
`(`a `b)
(quote a)
(quote (define a))

(eq? `a`b `ab)
(define (test x y) 1)

(test 1)
(test `a)
(test `a `b)

(symbol? 1)

(define (symbol-? x) 
  (or (eq? `quasiquote (car x)) 
      ()))

; deriv
(define (variable? expr) (symbol? expr))

(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list `+ a1 a2))

(define (make-product a1 a2) (list `* a1 a2))

(define (sum? x) 
  (and (pair? x) (eq? (car x) `+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) 
  (and (pair? x) (eq? (car x) `*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (=number? expr num) 
  (and (number? expr) (= expr num)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2) 
        ((=number? a2 0) a1) 
        ((and (number? a1) (number? a2)) (+ a1 a2)) 
        (else (list `+ a1 a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
        ((=number? m1 1) m2) 
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        (else (list `* m1 m2))))

(define (exponentiation? expr) 
  (and (pair? expr) (eq? (car expr) `**)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))

(define (make-exponentiation base expon) 
  (cond ((=number? expon 0) 1) 
        ((=number? base 1) 1) 
        ((=number? expon 1) base) 
        (list `** base expon)))
; 2.57
(define (addend s) (cadr s))

(define (augend s) 
  (if (not (pair? (cdddr s))) (cddr s) (append (list `+) (cddr s))))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2) 
        ((=number? a2 0) a1) 
        ((and (number? a1) (number? a2)) (+ a1 a2)) 
        ((eq? a2 `()) a1)
        (else (append (list `+ a1) a2))))



(define (deriv expr var) 
 (cond ((number? expr) 0) 
       ((variable? expr) (if (same-variable? expr var) 1 0)) 
       ((sum? expr) (make-sum (deriv (addend expr) var) 
                              (deriv (augend expr) var))) 
       ((product? expr) (make-sum 
                          (make-product (multiplier expr) 
                                        (deriv (multiplicand expr) var)) 
                          (make-product (deriv (multiplier expr) var) 
                                        (multiplicand expr)))) 
       ((exponentiation? expr) 
        (make-product (exponent expr) 
                      (make-exponentiation (base expr) (dec (exponent expr))))) 
       (else 
         (error "wrong expression type" expr))))

(deriv (list `+ `x 3) `x)

(deriv `(* x y) `x)

(deriv  (list `+ (list `** `x 2) 2) `x)

(deriv (list `+ `x `x 2) `x)

(define my-sum (list `+ `x `x 2))
(pair? (cdddr (list 1 2 3)) ) 


