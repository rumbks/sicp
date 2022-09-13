#lang sicp

(#%require rackunit)
(#%require "section1.rkt")

(check-equal? (greatest_squares_sum 1 2 3) 13)
(check-equal? (greatest_squares_sum 0 0 0) 0)
(check-equal? (greatest_squares_sum 1 1 2) 5)
(check-equal? (greatest_squares_sum 1 1 1) 2)
