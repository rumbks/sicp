; 1.29

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx) 
  (define (add-dx x) (+ x dx)) 
  (* (sum f (+ a (/ dx 2)) add-dx b) 
     dx))

(define (cube x) (* x x x))

(integral cube 0 1 0.001)

(define (simpson-integral f a b n) 
  (define (h) (/ (- b a) n))
  (define (y-k i) (f (+ 
                       a 
                       (* 
                         i 
                         (h)))))
  (define (plus2 x) (+ x 2))
  (* (+ 
       (y-k 0) 
       (* 
         (sum y-k 1 plus2 (dec n)) 
         4)
       (*
         (sum y-k 2 plus2 (- n 2)) 
         2)
       (y-k n))
     (/ (h) 3)))

(simpson-integral cube 0 1 100)

; 1.30
(define (i-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a))))) 
  (iter a 0))

(define (cube x) (* x x x))

(define (i-integral f a b dx) 
  (define (add-dx x) (+ x dx)) 
  (* (i-sum f (+ a (/ dx 2)) add-dx b) 
     dx))

(i-integral cube 0 1 0.001)

; 1.31
(define (i-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter 
          (next a) 
          (* result (term a))))) 
  (iter a 1))

(define (identity x) x)
(define (add-two x) (+ x 2))
(define (mul-with-step-two a b) 
  (i-product 
    identity 
    a 
    add-two
    b))

(define (calculate-pi) (* 
                         4.0 
                          (/
                            (* (mul-with-step-two 2 100) (mul-with-step-two 4 102))
                            (* (mul-with-step-two 3 101) (mul-with-step-two 3 101)))))
(calculate-pi)

(define (r-product term a next b) 
  (if (> a b) 
    1 
    (* (term a) (r-product term (next a) next b))))
(define (t-product a b) 
  (r-product identity a inc b))
(t-product 2 4)

; 1.42
(define (compose f g) 
  (lambda (x) (f (g x))))
(define (square x) (* x x))

((compose square inc) 6)

;1.43
(define (repeated f n) 
  (lambda (x) 
    (define (iter i result) 
           (if (< i n) (iter (inc i) (f result)) result)) 
         (iter 0 x)))

(define (alt-repeated f n) 
  (if (= n 1) 
    f 
    (compose f (repeated f (dec n)))))

((repeated square 2) 5)
((alt-repeated square 2) 5)

;1.44

(define dx 0.0001)

(define (smooth f) 
  (lambda (x) (/ (+ 
                   (f (- x dx)) 
                   (f x) 
                   (f (+ x dx))) 
                 3)))

(define (r-smooth f n) (repeated smooth n))
