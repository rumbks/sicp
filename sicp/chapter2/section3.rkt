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
(define (two-element-expression? expr) 
  (= (length expr) 3))

(define (addend s) (cadr s))

(define (augend s) 
  (if (two-element-expression? s) (caddr s) 
    (append (list `+) (cddr s))))

(define (multiplier p) (cadr p))

(define (multiplicand p) 
  (if (two-element-expression? p) (caddr p) 
      (append (list `*) (cddr p))))

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

;(deriv (list `+ `x 3) `x)

;(deriv `(* x y) `x)

;(deriv  (list `+ (list `** `x 2) 2) `x)

(deriv (list `+ (list `+ `x `x `x `y) `x 2) `x)

(deriv (list `* `x `y `z) `x)


; 2.58

(define (variable? expr) (symbol? expr))

(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (sum? x) 
  (and (pair? x) (eq? (cadr x) `+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x) 
  (and (pair? x) (eq? (cadr x) `*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (=number? expr num) 
  (and (number? expr) (= expr num)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2) 
        ((=number? a2 0) a1) 
        ((and (number? a1) (number? a2)) (+ a1 a2)) 
        (else (list a1 `+ a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
        ((=number? m1 1) m2) 
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        (else (list m1 `* m2))))


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

(deriv (list `x `+ (list `y `+ `x)) `x)



;2.59

(define (element-of-set? x set) 
  (cond ((null? set) #f) 
        ((equal? x (car set)) #t) 
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) 
  (if (element-of-set? x set) 
    set 
    (cons x set)))

(define (intersection-set set1 set2) 
  (cond ((or (null? set1) (null? set2)) `()) 
        ((element-of-set? (car set1) set2) 
         (cons (car set1) (intersection-set (cdr set1) set2))) 
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2) 
  (cond ((null? set1) set2) 
        ((null? set2) set1) 
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2) ) 
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set (list 1) (list 1 2 3))
(intersection-set (list 1) (list 2))
(intersection-set (list 1 2) (list 1 2 3 4))

;2.60

(define (element-of-set? x set) 
  (cond ((null? set) #f) 
        ((equal? x (car set)) #t) 
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) 
  (cons x set))

(define (intersection-set set1 set2) 
  (cond ((or (null? set1) (null? set2)) `()) 
        ((element-of-set? (car set1) set2) 
         (cons (car set1) (intersection-set (cdr set1) set2))) 
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2) 
  (cond ((null? set1) set2) 
        ((null? set2) set1) 
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2) ) 
        (else (cons (car set1) (union-set (cdr set1) set2)))))


(union-set (list 3 1 1 2) (list 3 1 2 2 4))


; 2.61, 2.62
(define (union-set set1 set2) 
  (cond ((null? set1) set2) 
        ((null? set2) set1) 
        (else 
          (let ((x1 (car set1)) 
                (x2 (car set2))) 
            (cond 
              ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2)))) 
              ((< x1 x2) (cons x1 (union-set (cdr set1) set2))) 
              ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))

(union-set (list 1 2 3) (list 3 4 5))
(union-set (list 1 2 5 7) (list 1 3 6 8))


(define (addjoin-set x set) 
  (union-set (list x) set))

(addjoin-set 3 (list 1 2 3))
(addjoin-set 4 (list 1 2 5))

(quotient 4 2)
(quotient 5 4)

;

(define (make-tree left-branch node-value right-branch) 
  (list left-branch node-value right-branch))

(define (get-left-branch tree) 
  (car tree))

(define (get-right-branch tree) 
  (caddr tree))

(define (get-node-value tree) 
  (cadr tree))

(define tree (make-tree (list null 2 (list null 3 null)) 1 null))

(get-left-branch tree)
(get-right-branch tree)
(get-node-value tree)


(define (lookup given-key set-of-records) 
  (cond ((null? set-of-records) #f) 
        ((= given-key (get-node-value set-of-records)) given-key) 
        ((< given-key (get-node-value set-of-records)) 
         (lookup given-key (get-left-branch set-of-records))) 
        (else (lookup given-key (get-right-branch set-of-records)))))


(define test-tree (list (list (list null 1 null) 
                              2 
                              (list null 3 null)) 
                        4 
                        (list null 5 null)))

(lookup 0 test-tree)


; 2.68

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

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
            (decode-1 (cdr bits) tree)
          )
          (decode-1 (cdr bits) next-branch)
        )
      )
    )
  )

  (decode-1 bits tree)
)

(define (choose-branch bit branch)
  (cond
    ((= bit 0) (left-branch branch))
    ((= bit 1) (right-branch branch))
    (else (error "bad bit -- CHOOSE_BRANCH" bit))
  )
)

(define (encode message tree) 
  (if (null? message) `() 
    (append (encode-symbol (car message) tree) 
            (encode (cdr message) tree))))

(define (in? element elements) 
  (if (eq? (memq element elements) #f) #f #t))

(in? 1 (list 1 2 3))
(in? 4 (list 1 2 3))

(define (encode-symbol symbol tree) 
  (define (encode-symbol-iter bits currect-branch) 
    (let ([left (left-branch currect-branch)] 
          [right (right-branch currect-branch)])
      (cond ((leaf? currect-branch) bits) 
            ((in? symbol (symbols left)) (encode-symbol-iter 
                                           (append bits (list 0)) 
                                           left)) 
            ((in? symbol (symbols right)) (encode-symbol-iter 
                                           (append bits (list 1)) 
                                           right)) 
            (else (error "Wrong symbol: " symbol))))) 
  (encode-symbol-iter `() tree))


(define sample-tree 
  (make-code-tree (make-leaf `A 4) 
                  (make-code-tree 
                    (make-leaf `B 2) 
                    (make-code-tree (make-leaf `D 1) 
                                    (make-leaf `C 1)))))

(define sample-message `(0 1 1 0 0 1 0 1 0 1 1 1 0))

(encode `(A D A B B C A) sample-tree)


; 2.69 Some language-specific issue occured. 
; Requires further investigation
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

test-leaves
(car test-leaves)
(cadr test-leaves)
(caddr test-leaves)
(adjoin-set (make-code-tree (car test-leaves) 
                            (cadr test-leaves)) 
            (caddr test-leaves))

(car test-leaves)
(cadr test-leaves)
(make-code-tree (car test-leaves) (cadr test-leaves)) 
(make-code-tree (make-leaf `D 1) (make-leaf `C 1))
(make-leaf `D 1)

(define test-leaf (make-leaf `D 1))
(symbols test-leaf)
(weight test-leaf)

(generate-huffman-tree test-pairs)




