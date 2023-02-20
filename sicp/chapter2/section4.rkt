; 2.73

(define (get-salary-tag-1) (display "Get salary on tag 1"))
(define (get-salary-tag-2) (display "Get salary on tag 2"))


(define (get op tag)
  (cond 
    ((= op `get-salary) 
     (cond 
       (((= tag `tag-1) (get-salary-tag-1)) 
        ((= tag `tag-2) (get-salary-tag-2)))))))


(define (get-record filename employee)
  ())
(define (get-salary filename employee) ())
(define (find-employee-record employee files) ())



