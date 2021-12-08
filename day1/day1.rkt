#lang racket

(define depths
  (for/list ([l (file->lines "input.txt")]) 
    (string->number l)))

(define (count-increasing depths)
  (for/fold ([counter 0]
             [prev (car depths)]
             #:result counter)
            ([n (in-list (cdr depths))])
      (values (cond [(and (number? n) (< prev n))  (add1 counter)]
                    [#t counter])
               n)))

(count-increasing depths)
