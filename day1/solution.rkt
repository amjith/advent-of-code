#lang racket

(define depths
 (for/list ([line (file->lines "input.txt")])
  (string->number line)))

(define (count-increases depths)
 (for/fold ([counter 0]
	    [previous-number (car depths)]
#:result counter)
  ([n (in-list (cdr depths))])
  (values (if (< previous-number n)
	   (add1 counter)
	   counter)
   n)))

(count-increases depths)
