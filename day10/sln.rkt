#lang racket

(require threading)
(require math/statistics)

(define syntax-lines
  (~>> (file->lines "input.txt")
       (map (lambda (x) (string-split x "")))
       (map (lambda (x) (filter (lambda (y) (not (equal? y ""))) x)))
       ))

; part 1

(define opens (set "(" "[" "{" "<"))
(define closes (set ")" "]" "}" ">"))

(define (corrupt-score line)
  (for/fold ([stack '()] 
             [score 0]
             #:result score
             )
            ([char (in-list line)])
            #:break (> score 0)
            (cond
              [(set-member? opens char) (values (cons char stack) 0)]
              [(equal? ")" char) (if (equal? "(" (car stack)) (values (cdr stack) 0) (values (cdr stack) 3))]
              [(equal? "]" char) (if (equal? "[" (car stack)) (values (cdr stack) 0) (values (cdr stack) 57))]
              [(equal? "}" char) (if (equal? "{" (car stack)) (values (cdr stack) 0) (values (cdr stack) 1197))]
              [(equal? ">" char) (if (equal? "<" (car stack)) (values (cdr stack) 0) (values (cdr stack) 25137))]
              )
            ))

(define (part1 xs)
  (for/sum ([line (in-list xs)])
           (corrupt-score line)
            ))

(define uncorrupted-lines (filter (lambda (x) (= 0 (corrupt-score x))) syntax-lines))

(define (completion-score xs)
  (for/fold ([score 0]
             #:result score
             )
            ([c (in-list xs)])
            (cond
              [(equal? "(" c) (+ (* score 5) 1)]
              [(equal? "[" c) (+ (* score 5) 2)]
              [(equal? "{" c) (+ (* score 5) 3)]
              [(equal? "<" c) (+ (* score 5) 4)]
              )))

(define (score-completion line)
  (for/fold ([stack '()] 
             #:result (completion-score stack)
             )
            ([char (in-list line)])
            (cond
              [(set-member? opens char) (cons char stack)]  ; Push char into stack
              [(set-member? closes char) (cdr stack)]  ; Pop char out of stack
              )
            ))

(define (part2 xs)
  (median < 
          (for/list ([line (in-list xs)])
            (score-completion line)
            ))
)
