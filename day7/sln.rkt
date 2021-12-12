#lang racket

(require threading)

(define crabs
  (~> (file->lines "input.txt")
      car
      (string-split _ ",")
      (map string->number _)))

(define (calc-distance target points)
  (for/fold ([tot 0])
            ([point (in-list points)])
            (+ tot (abs (- target point)))))

(define (find-min xs)
  (foldl min (first xs) (rest xs)))

(define (find-max xs)
  (foldl max (first xs) (rest xs)))

(define part1
  (find-min (for/list ([x (in-list crabs)])
                      (calc-distance x crabs))))

(define (sum-of-first n)
  (/ (* n (add1 n)) 2))

(define (calc-progressive-distance target points)
  (for/fold ([tot 0])
            ([point (in-list points)])
            (+ tot (sum-of-first (abs (- target point))))))

(define part2
  (find-min (for/list ([x (find-max crabs)])
                      (calc-progressive-distance x crabs))))
