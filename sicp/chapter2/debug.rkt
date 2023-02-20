#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight)
)

(define (leaf? object)
  (eq? (car object) 'leaf)
)

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
  )
)

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)
  )
)

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)
  )
)



(define sample-tree 
  (make-code-tree (make-leaf `A 4) 
                  (make-code-tree 
                    (make-leaf `B 2) 
                    (make-code-tree (make-leaf `D 1) 
                                    (make-leaf `C 1)))))

(define sample-message `(0 1 1 0 0 1 0 1 0 1 1 1 0))




(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs) 
  (if (null? pairs) 
    `()
    (let ([pair (car pairs)])
      (adjoin-set (make-leaf (car pair)
                             (cadr pair))
                  (make-leaf-set (cdr pairs))))))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge leaves)
  (cond ((= (length leaves) 1) leaves)
        ((= (length leaves) 2) (make-code-tree (car leaves) (cadr leaves)))
        (else (successive-merge (adjoin-set (make-code-tree 
                                              (car leaves) 
                                              (cadr leaves)) 
                                            (caddr leaves))))))

; (define test-pairs (list `(D 1) `(C 1) `(B 2) `(A 4)))
(define test-pairs (list `(D 1) `(C 1) `(B 2)))

(define test-leaves (make-leaf-set test-pairs))

(make-code-tree (car test-leaves) (cadr test-leaves)) 

(generate-huffman-tree test-pairs)
