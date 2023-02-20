#lang sicp

;(define (count-pairs x) 
  ;(if (not (pair? x)) 
    ;0 
    ;(+ (count-pairs (car x)) 
       ;(count-pairs (cdr x))
       ;1)))

;(define e1 `(a b c))
;(count-pairs e1)
;(define m `(a b))
;(define e2 (cons m (cdr m)))
;(count-pairs e2)



; 3.17

(define (make-set) 
  (define set `()) 
  (define (in-set? x set)
    (cond ((null? set) #f) 
          ((equal? x (car set)) #t) 
          (else (in-set? x (cdr set)))))

  (define (add x) 
    (if (in-set? x set) 
      set 
      (set! set (cons x set))))
  (define (dispatch op-name x) 
    (cond ((eq? op-name `contains) (in-set? x set)) 
          ((eq? op-name `add) (add x)) 
          (else (error "Unknown operation"))))
  dispatch)


(define (count-pairs x) 
  (define encountered-pairs (make-set))
  (define (iter x)
    (if (not (pair? x)) 
        0 
        (+ (count-pairs (car x)) 
           (count-pairs (cdr x))
           (if (not (encountered-pairs `contains x)) 
             (begin (encountered-pairs `add x) 1) 
             0))))
  (iter x))

(define (check-cycle x) 
  (define encountered-items (make-set)) 
  (define (iter x) 
    (cond ((not (pair? x)) #f) 
          ((encountered-items `contains (car x)) #t) 
          (else (begin (encountered-items `add (car x)) 
                       (iter (cdr x)))))) 
  (iter x))



(define e1 `(a b c))
;(count-pairs e1) 
(define m `(a b))
(define e2 (cons m (cdr m)))
;(count-pairs e2) 
(check-cycle e1)
(check-cycle e2)

(define e3 (cons `a `()))
(set-cdr! e3 e3)
(check-cycle e3)

(define e4 (list `a `b `c))

(cddr e4)
(set-cdr! (cddr e4) e4)
(check-cycle e4)

(define e5 (list `a `b `c))
(set-cdr! (cddr e5) (cdr e5))
(check-cycle e5)

(define (make-queue) 
  (let 
    ((front-ptr `()) 
     (rear-ptr `())) 
    (define (empty-queue?) 
      (null? front-ptr)) 
    (define (front-queue) 
      (if (empty-queue?) 
        (error "front-queue called for empty queue") 
        (car front-ptr))) 
    (define (insert-queue! x) 
      (let ((new-pair (cons x `()))) 
        (cond ((empty-queue?) 
               (set! front-ptr new-pair) 
               (set! rear-ptr new-pair)) 
              (else 
                (set-cdr! rear-ptr new-pair) 
                (set! rear-ptr new-pair))))) 
    (define (delete-queue!) 
      (cond ((empty-queue?) 
             (error "delete-queue called for empty queue")) 
            (else (set! front-ptr (cdr front-ptr))))) 
    (define (dispatch m) 
      (cond ((eq? m `empty?) empty-queue?) 
            ((eq? m `insert!) insert-queue!) 
            ((eq? m `front) front-ptr) 
            ((eq? m `delete!) delete-queue!))) 
    dispatch))

(define q (make-queue))
((q `insert!) `a)
((q `insert!) `b)
(q `front)
((q `delete!))
(q `front)