#lang racket

(require threading)

(define lantern-fish-timers
  (~> (file->lines "input.txt")
      car
      (string-split _ ",")
      (map string->number _)))

;(define (advance-timer timers)
;  (for/list ([timer (in-list timers)])
;            (if (= timer 0) 6 (sub1 timer))))


;(define (grow-fishes timers)
;  (for/fold ([acc timers]
;            #:result (length acc))
;            ([day 60])
;            (begin
;             (define new-timers (advance-timer acc))
;             (define new-spawns (count (lambda (x) (= x 6)) new-timers))
;             (append new-timers (make-list new-spawns 8)))))


; Build a hash-map with days as keys and num of fishes as value.
; Each iteration update the numbers by shifting them from one day to another.
; Everything in day 0 goes to day 6.
; Everything in day 6 gets added to day 8.

(define fishies (make-hash
  (for/list ([x 9])
       (cons x (count (lambda (y) (= x y)) lantern-fish-timers))
       )
  ))

(define (next-day day)
            (if (= day 0) 
              6 
              (sub1 day)))


(define (grow-fishies fishies)
  (hash 
    8 (hash-ref fishies 0)
    7 (hash-ref fishies 8)
    6 (+ (hash-ref fishies 7) (hash-ref fishies 0))
    5 (hash-ref fishies 6)
    4 (hash-ref fishies 5)
    3 (hash-ref fishies 4)
    2 (hash-ref fishies 3)
    1 (hash-ref fishies 2)
    0 (hash-ref fishies 1)
   )
  )

(define (day-iteration initial-fishies days)
  (for/fold ([fishies initial-fishies]
             #:result (apply + (hash-values fishies)))
    ([x days])
    (grow-fishies fishies)
       ))

