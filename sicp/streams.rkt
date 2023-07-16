#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))          

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))


;Exercise 3.50
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map (cons proc (map stream-cdr argstreams))))))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))


(define (generate-stream constant) 
  (cons-stream constant (generate-stream constant)))

;Exercise 3.51-3.52
;(define (test)
  ;(define sum (stream-map + (generate-stream 1) (generate-stream 2) (generate-stream 3)))
  ;(display (stream-ref sum 1))
  ;(display (stream-ref sum 3))
  ;(display (stream-ref sum 5))

  ;(define x (stream-map show  (stream-enumerate-interval 0 10)))

  ;(stream-ref x 5)
  ;(stream-ref x 1))

;(define (test)
;    (define sum 0)
;    (define (accum x)
;    (set! sum (+ x sum))
;    sum)
;
;    (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;    (define y (stream-filter even? seq))
;    (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

;    (display (stream-ref y 7))
;    (display-stream z))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

;Exercise 3.53
(define s (cons-stream 1 (add-streams s s)))
;(display (stream-ref s 3))
;(display (stream-ref s 5))


;Exercise 3.54
(define factorials (cons-stream 1 (mul-streams integers factorials)))
;(display (stream-ref factorials 0))
;(display (stream-ref factorials 1))
;(display (stream-ref factorials 2))
;(display (stream-ref factorials 3))

;Exercise 3.55
(define (partial-sums s)
  (define without-first (stream-cdr s))
  (define partial-sums-stream
    (cons-stream (car s) (add-streams partial-sums-stream without-first)))
  partial-sums-stream)


(define partial-sums-ones (partial-sums integers))
;(display (stream-ref partial-sums-ones 0))
;(display (stream-ref partial-sums-ones 1))
;(display (stream-ref partial-sums-ones 2))
;(display (stream-ref partial-sums-ones 3))


(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))


(define (show x) 
  (display-line x) x)

(define S (cons-stream
	    1
	    (merge (stream-map show (scale-stream S 2))
		   (merge (stream-map show (scale-stream S 3))
			  (stream-map show (scale-stream S 5))))))


;(stream-ref S 0)
;(stream-ref S 1)
;(stream-ref S 2)
;(stream-ref S 3)
;(stream-ref S 4)
;(stream-ref S 5)
;(stream-ref S 0)



; 3.56
(define fibs 
  (cons-stream 0 
               (cons-stream 1 (add-streams (stream-cdr fibs) 
                                           fibs))))

; 3.57
(define (expand num den radix) 
  (cons-stream 
    (quotient (* num radix) den) 
    (expand (remainder (* num radix) den) den radix)))

(define stream (expand 1 7 10))

;(stream-ref stream 0)
;(stream-ref stream 1)
;(stream-ref stream 2)
;(stream-ref stream 3)
;(stream-ref stream 4)
;(stream-ref stream 5)
;(stream-ref stream 6)
;(stream-ref stream 7)
;(stream-ref stream 8)
;(stream-ref stream 9)


; 3.63

; Original
(define (average x y) (/ (+ x y) 2))


(define (sqrt-improve guess x) 
         (average guess (/ x guess)))

(define (sqrt-stream-original x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)


; Excersize


(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))


(define sstream (sqrt-stream 2))

;(stream-ref sstream 0)
;(stream-ref sstream 1)
;(stream-ref sstream 2)
;(stream-ref sstream 3)


; 3.64

(define (stream-cadr s) (stream-car (stream-cdr s)))

(define (stream-limit s tolerance) 
  (if (< (abs (- (stream-car s) (stream-cadr s))) tolerance)
    (stream-cadr s)
    (stream-limit (stream-cdr s) tolerance)))


; 3.65
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))


(define (ln2-summands n) 
  (cons-stream (/ 1.0 n) 
               (stream-map - (ln2-summands (+ n 1)))))


(define ln2-stream (partial-sums (ln2-summands 1)))

(stream-ref ln2-stream 5)
(stream-ref ln2-stream 6)
(stream-ref ln2-stream 7)
(stream-ref ln2-stream 8)
(stream-ref ln2-stream 9)
