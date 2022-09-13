#lang sicp

(#%require rackunit)


(define (squares_sum x y) (+ (* x x) 
                             (* y y)))
(define (>= x y) (or (> x y) (= x y)))
(define (max x y) (if (< x y) y x))
(define (greatest_squares_sum x y z) (cond ((>= x y) (squares_sum (max y z) x)) 
                                           (else (squares_sum (max x z) y))))
; 1.6
(define (new-if predicate then-clause else-clause) 
  (cond (predicate then-clause) 
        (else else-clause)))

(define (average x y) 
  (/ (+ x y) 2))

(define (sqrt-improve guess x) 
  (average guess (/ x guess)))

(define (fucn arg1 arg2 ...) (body))
(define name (expr))
(define (good-enough-cmp? guess x) 
  (< 
    (abs 
      (- 
        (* guess guess) 
        x)) 
    0.001))
(define (sqrt-iter guess x) 
        (if (good-enough? guess x)
        guess
        (sqrt-iter (sqrt-improve guess x)
                   x)))

(define (sqrt x) 
  (sqrt-iter 1.0 x))
; 1.7
; Generate predicate that checks if guess and improved guess are close enough
(define (good-enough-difference? improve-func guess x) 
  (< 
    (abs 
      (- 
        (improve-func guess x) 
        guess)) 
    0.000001))

(define (root-func step-func stop? guess x) 
  (if (stop? guess x)
  guess 
  (root-func step-func stop? (step-func guess x) x)))


(define (sqrt-cmp x) 
  (root-func sqrt-improve good-enough-cmp? 1.0 x))

(define (sqrt-good-enough-difference? guess x) 
  (good-enough-difference? sqrt-improve guess x))

(define (sqrt-difference x) 
  (root-func sqrt-improve sqrt-good-enough-difference? 1.0 x))
;1.8
(define (cbrt-improve guess x) 
  (/ (+ (/ x 
           (* guess guess)) 
        (* 2 guess)) 
     3))

(define (cbrt-good-enough-cmp? guess x) 
  (< 
    (abs 
      (- 
        (* guess guess guess) 
        x)) 
    0.001))

(define (cbrt-good-enough-difference? guess x) 
  (good-enough-difference? cbrt-improve guess x))

(define (cbrt-difference x) 
  (root-func cbrt-improve cbrt-good-enough-difference? 1.0 x))

(define (cbrt-cmp x) 
  (root-func cbrt-improve cbrt-good-enough-cmp? 1.0 x))


(#%provide (all-defined))
