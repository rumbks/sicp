;1.10.
(define (A x y) 
  (cond ((= y 0) 0) 
        ((= x 0) (* 2 y)) 
        ((= y 1) 2) 
        (else (A (dec x) 
                 (A x (dec y))))))
(A 1 10)
(A 2 4)
(A 3 3)

; 2*n
(define (f n) (A 0 n))
; 2^n
(define (g n) (A 1 n))
; 2^...^2 n times
(define (h n) (A 2 n))


; 1.11.
; f(n)=n if n<3 else f(n-1)+f(n-2)+f(n-3)
(define (recursive-f n) 
  (if (< n 3) 
    n 
    (+ 
      (recursive-f (- n 1))
      (recursive-f (- n 2))
      (recursive-f (- n 3)))))
(define (iterative-f n) (iter-f 0 1 2 n))
(define (iter-f a b c count) 
  (if (= count 0) 
    a 
    (iter-f 
      b 
      c 
      (+ a b c)
      (dec count))))
;1.12.

(define (pascal-triangle-element n) 
  (define (iter-layer element-number layer-n layer-greatest-element-number)
    (if (< 
          element-number 
          (inc layer-greatest-element-number)) 
      layer-n
      (iter-layer 
        element-number 
        (inc layer-n)
        (+ 
          layer-greatest-element-number
          (+ layer-n 2)))))
  (define (layer element-n) 
    (iter-layer element-n 0 0))
  (define (on-edge) 
    (or 
      (= n 0)
      (< 
        (layer (dec n))
        (layer n))
      (> 
        (layer (inc n))
        (layer n))))
  (define (upper-left-element-n) 
    (- 
      n 
      (inc (layer n))))
  (if (on-edge)
      1
      (+ 
        (pascal-triangle-element (upper-left-element-n)) 
        (pascal-triangle-element (inc (upper-left-element-n))))))

;1.15.
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle) 
  (if (not (> (abs angle) 0.1)) 
    angle 
    (p (sine (/ angle 3.0)))))

;a)
(define (count-p-calls angle) 
  (if (not (> (abs angle) 0.1)) 
    0 
    (inc (count-p-calls (/ angle 3)))))
(count-p-calls 12.15)
;b) Константная память и логарифмическое время

;1.16
(define (even? num) 
  (= (remainder num 2) 0))
(define (square x) 
  (* x x))
(define (fast-exp-iter b n result) 
  (cond ((= n 0) result) 
        ((even? n) (fast-exp-iter 
                     b 
                     (/ n 2) 
                     (square result))) 
        (else (fast-exp-iter 
                b 
                (dec n) 
                (* result b)))))
(define (fast-exp b n) 
  (fast-exp-iter b n 1))
(fast-exp 2 9)

;1.17
(define (double x) 
  (* x 2))
(define (halve x) 
  (/ x 2))
(define (fast-mul-rec b n) 
  (cond ((= n 0) 0) 
        ((even? n) (double (fast-mul-rec 
                             b 
                             (halve n)))) 
        (else (+ 
                b 
                (fast-mul-rec 
                  b 
                  (dec n))))))
;1.18
(define (fast-mul-iter b n result) 
  (cond ((= n 0) result) 
        ((even? n) (fast-mul-iter 
                     b 
                     (halve n) 
                     (double result))) 
        (else (fast-mul-iter 
                b 
                (dec n) 
                (+ result b)))))
(define (fast-mul b n) 
  (fast-mul-iter b n 0))
(fast-mul 9 9)

;1.19

(define (fib-iter a b p q count) 
  (cond ((= count 0) b) 
        ((even? count) (fib-iter a 
                                 b 
                                 (+ 
                                   (* p p) 
                                   (* q q)) 
                                 (+ 
                                   (* 2 p q) 
                                   (* q q)) 
                                 (/ count 2))) 
        (else (fib-iter (+ (* b q) (* a q) (* a p)) 
                        (+ (* b p) (* a q)) 
                        p 
                        q 
                        (dec count)))))
(define (fib n) 
  (fib-iter 1 0 0 1 n))
(fib 5)

; 1.20

(define (gcd a b) 
  (if (= b 0) 
    a 
    (gcd 
      b 
      (remainder a b))))

; Normal order
(gcd 206 40);->
;(gcd 40 6) 
(gcd 40 (remainder 206 40));->
;(gcd 6 4)
(gcd (remainder 206 40) (remainder 
                          40 
                          (remainder 206 40)) );->

;(gcd 4 2)
(gcd (remainder 
      40 
      (remainder 206 40)) 
     (remainder 
       (remainder 206 40) 
       (remainder 
         40 
         (remainder 206 40))));->
;(gcd 2 0)
(gcd (remainder 
       (remainder 206 40) 
       (remainder 
         40 
         (remainder 206 40))) 
     (remainder 
       (remainder 
         40 
         (remainder 206 40))
       (remainder 
        (remainder 206 40) 
        (remainder 
         40 
         (remainder 206 40)))));
;11 remainder calls

;Applicative order - 4 calls

; 1.21
(define (square x) (* x x))
(define (divides? a b) 
  (= (remainder b a) 0))

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor) 
        (else (find-divisor n (inc test-divisor)))))

(define (smallest-divisor n) 
  (find-divisor n 2))
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; 1.22
(define (prime? n)
  (= n (smallest-divisor n)))

(define (start-prime-test n start-time) 
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time) 
  (display " *** ")
  (display elapsed-time))

(define (timed-prime-test n) 
  (newline)
  (display n)
  (start-prime-test n (runtime)))

