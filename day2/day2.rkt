#lang racket

(define commands
  (for/list ([l (file->lines "input.txt")])
            (match-define (list k v) (string-split l))
            (list k (string->number v))))

; part 1

(define (calc-forward data)
  (for/sum ([x (in-list data)]
             #:when (equal? (car x) "forward"))
            (cadr x)))


(define (calc-up data)
  (for/sum ([x (in-list data)]
            #:when (equal? (car x) "up"))
           (cadr x)))

(define (calc-down data)
  (for/sum ([x (in-list data)]
            #:when (equal? (car x) "down"))
           (cadr x)))

(define (part1-calc data)
  (* (- (calc-down commands) (calc-up commands)) (calc-forward commands)))

; part 2

(define (part2-calc data)
  (for/fold 
    ([aim 0] [depth 0] [position 0]
     #:result (* depth position))
    ([line (in-list data)])
    (match line
           [(list "forward" x) (values aim (+ depth (* aim x)) (+ position x))]
           [(list "up" x) (values (- aim x) depth position)]
           [(list "down" x) (values (+ aim x) depth position)])))

