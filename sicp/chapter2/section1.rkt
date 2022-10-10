; 2.1
(define (gcd a b) 
  (if (= b 0) 
    a 
    (gcd 
      b 
      (remainder a b))))

(define (make-rat-gcd n d) 
  (let ((g (gcd  (abs n)  (abs d))) )
  (cons (/ n g) (/ d g))))

(define (make-rat n d) 
  (cond ((negative? (* n d)) (make-rat-gcd (- (abs n)) (abs d))) 
        (else (make-rat-gcd (abs n) (abs d)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(make-rat -1 -2)
(make-rat 1 -2)
(make-rat -1 2)
(make-rat 1 2)


; 2.2
(define (average x y)
    (/ (+ x y) 2))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint segment)
  (let ((start (start-segment segment))
        (end  (end-segment segment)))
   (make-point (average (x-point start) (x-point end)) 
               (average (y-point start) (y-point end)))))

(define start (make-point 0 0))
(define end (make-point 1 1))
(define segment (make-segment start end))
(define middle (midpoint segment))

; 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (sub-interval x y) 
  (make-interval (- (lower-bound x) (upper-bound y)) 
                 (- (upper-bound x) (lower-bound y))))

(define x (make-interval 2 5))
(define y (make-interval 1 6))
(define diff (sub-interval x y))
(upper-bound diff)
(lower-bound diff)

;2.10
(define (contains-zero? interval) 
  (<= (* (lower-bound interval) (upper-bound interval))))

;2.12
(define (make-center-width c w) 
  (make-interval (- c w) (+ c w)))

(define (center i) 
  (/ 
   (+ 
     (lower-bound i) 
     (upper-bound i)) 
   2))

(define (width i) 
  (/ 
   (- 
     (upper-bound i) 
     (lower-bound i)) 
   2))

(define (make-center-percent c p) 
  (make-center-width c (* c p 0.01)))

(make-center-percent 1 30)

; 2.17
(define (last-pair l) 
  (if (null? (crd l) ) l (last-pair (car l))))

(last-pair (list 1 2 3))

; ---
; Fragment lost
;

; 2.29

(define (make-mobile left right) 
  (list left right))

(define (make-branch len structure) 
  (list len structure))

; a

(define (left-branch mobile) 
  (car mobile))


(define (right-branch mobile) 
  (cadr mobile))

(define (branch-length branch) 
  (car branch))

(define (branch-structure branch) 
  (cadr branch))

; b
(define (total-branch-weight branch) 
  (if (not (pair? (branch-structure branch))) 
    (branch-structure branch) 
    (total-weight (branch-structure branch))))

(define (total-weight mobile) 
  (+ 
    (total-branch-weight (left-branch mobile)) 
    (total-branch-weight (right-branch mobile))))

(define b1 (make-branch 1 2))
(define b2 (make-branch 1 3))
(define m1 (make-mobile b1 b2))
(define b3 (make-branch 1 m1))
(define m (make-mobile b1 b3))
(total-weight m)

;c
(define (force-momentum branch) 
  (* 
    (branch-length branch) 
    (total-branch-weight branch)))

(define (balanced-branch? branch) 
  (if (not (pair? (branch-structure branch))) 
    true 
    (balanced? (branch-structure branch))))

(define (balanced? mobile) 
  (and 
    (= (force-momentum (left-branch mobile)) 
       (force-momentum (right-branch mobile))) 
    (balanced-branch? (left-branch mobile)) 
    (balanced-branch? (right-branch mobile))))

(balanced? m)

(define b4 (make-branch 2 3))
(define b5 (make-branch 3 2))
(define mb (make-mobile b4 b5))
(balanced? mb)


; 2.30
(define (square x) (* x x))

(define (square-tree tree) 
  (cond ((null? tree) nil) 
        ((not (pair? tree)) (square tree)) 
        (else (cons (square-tree (car tree)) 
                    (square-tree (cdr tree))))))

(square-tree 
  (list 1 
        (list 2 (list 3 4) 5) 
        (list 6 7)))

(define (square-tree-with-map tree) 
  (map (lambda (elem) 
         (if (pair? elem) 
           (square-tree-with-map elem) 
           (square elem)))
       tree))

(square-tree-with-map
  (list 1 
        (list 2 (list 3 4) 5) 
        (list 6 7)))

; 2.31


(define (tree-map func tree) 
  (cond ((null? tree) nil) 
        ((not (pair? tree)) (func tree)) 
        (else (cons (square-tree (car tree)) 
                    (square-tree (cdr tree))))))

(define (square-tree-with-tree-map tree) 
  (tree-map square tree))

(square-tree-with-tree-map
  (list 1 
        (list 2 (list 3 4) 5) 
        (list 6 7)))


; 2.32
(define (subsets s) 
  (if (null? s) (list nil) 
    (let ((rest (subsets (cdr s)))) 
      (append rest (map 
                     (lambda (elem) 
                       (append  elem (list (car s)))) rest)))))

(subsets (list 1 2 3))

; 2.33

(define (accumulate op initial sequence) 
  (if (null? sequence) 
    initial 
    (op (car sequence) 
        (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence) 
  (cond ((null? sequence) nil) 
        ((predicate (car sequence)) 
         (cons (car sequence) (filter predicate (cdr sequence)))) 
        (else (filter predicate (cdr sequence)))))

(filter positive? (list 1 2 3 -4))


(define (map- p sequence) 
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(map- square (list 2 3))


(define (append- seq1 seq2) 
  (accumulate cons seq2 seq1))

(append- (list 1 2) (list 3 4))

(define (length- sequence) 
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length- (list 1 2 3))


; 2.34
(define (horner-eval x coefficient-sequence) 
  (accumulate 
    (lambda (this-coeff higher-terms) 
      (+ 
        this-coeff 
        (* x higher-terms))) 
    0 
    coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))


; 2.35
(define (enumerate-tree tree) 
  (cond ((null? tree) nil) 
        ((not (pair? tree)) (list tree)) 
        (else (append (enumerate-tree (car tree)) 
                    (enumerate-tree (cdr tree))))))

(define (count-leaves t) 
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(define t (list 1 2 (list 3)))
(count-leaves t)

; 2.36

(define (accumulate op initial sequence) 
  (if (null? sequence) 
    initial 
    (op (car sequence) 
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs) 
  (if (null? (car seqs)) 
    nil 
    (cons (accumulate op init (map car seqs)) 
          (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2) (list 3 4)))

; 2.37
(define matrix (list (list 1 2) (list 3 4)))
(define vector (list 2 3))

(define (dot-product v w) 
  (accumulate + 0 (map * v w)))


(define (matrix-*-vector m v) 
  (map (lambda (row) (dot-product row v)) m))

(matrix-*-vector matrix vector)


(define (transpose matrix) 
  (accumulate-n cons nil matrix))

(transpose matrix)

(define (matrix-*-matrix m n) 
  (let ((cols (transpose n))) 
    (map (lambda (r) (matrix-*-vector cols r)) m)))

(matrix-*-matrix matrix matrix)


; 2.38
(define (fold-left op initial sequence) 
  (define (iter result rest) 
    (if (null? rest) 
      result
      (iter (op result (car rest)) 
            (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (reverse-r sequence) 
  (fold-right (lambda (first result) (append result (list first))) 
              nil sequence))

(define (reverse-l sequence) 
  (fold-left (lambda (result first) (cons first result)) 
             nil sequence))

(reverse-r (list 1 2 3))
(reverse-l (list 1 2 3))
(list 3 2 1)

;;
(define (enumerate-interval low high) 
  (if (> low high) 
    nil 
    (cons low (enumerate-interval (inc low) high))))

(enumerate-interval 0 5)

(map (lambda (i) 
       (map (lambda (j) (list i j))
            (enumerate-interval 1 (dec i)))) 
     (enumerate-interval 1 5))

; 2.40

(define (flatmap proc seq) 
  (accumulate append nil (map proc seq)))

(define (unique-pairs n) 
  (flatmap (lambda (i) 
             (map (lambda (j) (list i j)) 
                            (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(cadr (unique-pairs 5) ) 

(define (even-sum-pair? pair) 
  (even? (+ (car pair) (cadr pair))))

(define (even-sum-pairs n) 
  (filter even-sum-pair? (unique-pairs n)))

(even-sum-pairs 5)
(filter even? (enumerate-interval 1 3))

; 2.41

(define (unique-triples n) 
  (flatmap (lambda (i) 
             (flatmap (lambda (j) 
                    (map (lambda (k) (list i j k)) 
                         (enumerate-interval 1 (dec j)))) 
                  (enumerate-interval 1 (dec i)))) 
           (enumerate-interval 1 n)))

(define (sum-equals-s? s triple) 
  (= s 
     (+ (car triple) (cadr triple) (caddr triple))))

(define (triples-with-sum-equal-to-s s n) 
  (define (filter-triple t) (sum-equals-s? s t))
  (filter filter-triple (unique-triples n)))

(sum-equals-s? 6 (list 1 2 3))

(triples-with-sum-equal-to-s 10 20)

; 2.42
(define (queens board-size) 
  (define (queen-cols k) 
    (if (= k 0) 
      (list empty-board) 
      (filter 
        (lambda (positions) (safe? k positions)) 
        (flatmap 
          (lambda (rest-of-queens) 
            (map (lambda (new-row) 
                   (addjoin-position new-row k rest-of-queens)) 
                 (enumerate-interval 1 board-size))) 
          (queen-cols (dec k)))))) 
  (queen-cols board-size))
