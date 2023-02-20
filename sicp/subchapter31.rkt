#lang sicp

;Exercise 3.1

(define (make-accumulator value)
  (lambda (a)
    (begin (set! value (+ value a)) value)))


(define A (make-accumulator 5))

;(A 10)
;(A 10)


;Exercise 3.2

(define (make-monitored func)
  (define calls-count 0)

  (define (reset-count)
    (begin (set! calls-count 0) calls-count))

  (define (call m)
    (begin (set! calls-count (+ calls-count 1)) (func m)))

  (define (dispatch m)
    (cond ((eq? m `how-many-calls?) calls-count)
	  ((eq? m `reset-count) (reset-count))
	  (else (call m))))
  dispatch)

;(define s (make-monitored sqrt))
;(s 100)
;(s `how-many-calls?)
;(s 100)
;(s `how-many-calls?)
;(s `reset-count)
;(s 100)
;(s `how-many-calls?)


;Exercie 3.3

(define (make-account balance password)
  (define incorrect-tries 0)

  (define (call-the-cops)
    (error "Call the police"))

  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Not enough money in account"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch pass m)
    (if (not (eq? password pass))
      (handle-wrong-password)
      (cond ((eq? m `withdraw) withdraw)
	    ((eq? m `deposit) deposit)
	    (else "Unknown operation"))))

  (define (handle-wrong-password)
    (set! incorrect-tries (+ incorrect-tries 1))
    (if (>= incorrect-tries 3)
      (call-the-cops)
      (display "Wrong password try again "))
    (lambda (x) (display "")))

  dispatch)

;(define acc (make-account 100 `secret))
;((acc `secret `withdraw) 40)
;((acc `incorrect-secret `deposit) 40)
;((acc `incorrect-secret `deposit) 40)
;((acc `incorrect-secret `deposit) 40)



;Exercise 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(random-in-range 1.0 5.0)
(random-in-range -1.0 1.0)

(define (rect-square x1 x2 y1 y2)
  (* (abs (- x1 x2)) (abs (- y1 y2))))


(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (define (predicate-test)
    (let ((x (random-in-range x1 x2))
	  (y (random-in-range y1 y2)))
      (predicate x y)))

  (* (rect-square x1 x2 y1 y2) (monte-carlo trials predicate-test)))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else (iter (- trials-remaining 1) trials-passed))))

  (iter trials 0))


(define (circle-1 x y)
  (< (+ (* x x) (* y y)) 1))

(rect-square -1.0 1.0 -1.0 1.0)
(estimate-integral circle-1 -1.0 1.0 -1.0 1.0 99900)

; Exercise 3.6
(define (rand-update x)
  (remainder (+ (* x 17) 11) 13))

;(define (rand m))



;Exercise 3.7
; Think about different approach.

(define (make-joint acc acc-password password)

  (define (dipatch pass m)
    (if (not (eq? pass password))
      (acc `wrong-pass m)
      (acc acc-password m)))

  dipatch)


;(define peter-acc (make-account 100 `peter-secret))
;(define paul-acc (make-joint peter-acc `peter-secret `paul-secret))

;((paul-acc `paul-secret `withdraw) 40)
;((paul-acc `incorrect-secret `deposit) 40)




;Exercise 3.8 - Solve with Igor
(define first #t)

(define (f x) 
  (if (not first) 0 (begin (set! first #f) x)))

(+ (f 0) (f 1))
(+ (f 1) (f 0))



