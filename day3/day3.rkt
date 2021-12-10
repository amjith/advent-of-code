#lang racket

(require threading)   ; imported for the ~> operator.

(define diagnostics
  (for/list ([l (file->lines "input.txt")])
            (string->list l)))

(define half-diag (/ (length diagnostics) 2))

(define (bit-list->number blst)
  (~> blst
      (apply ~a _)
      (string->number _ 2)))

(define (part1-calc data)
  (for/fold
    ([results '(0 0 0 0 0 0 0 0 0 0 0 0)]
     #:result (cons 
                (bit-list->number (map (lambda (x) (if (> x (/ (length diagnostics) 2)) 1 0)) results))
                (bit-list->number (map (lambda (x) (if (< x (/ (length diagnostics) 2)) 1 0)) results)))
     )
    ([line (in-list data)])
    (map + results 
         (for/list ([d line]) 
                   (if (equal? d #\1)
                     1
                     0)))))

(match-define (cons gamma epsilon) (part1-calc diagnostics))
(* gamma epsilon)


(define (calc-bitsum data)
  (for/fold
    ([results '(0 0 0 0 0 0 0 0 0 0 0 0)]
     #:result results
     )
    ([line (in-list data)])
    (map + results 
         (for/list ([d line]) 
                   (if (equal? d #\1)
                     1
                     0)))))


(define (bit-char->number blst)
  (~> blst
      (apply string _)
      (string->number _ 2)))


(define (oxy-rating data [position 0])
  (define bitsum (calc-bitsum data))
  (define half-len (/ (length data) 2))
  (define look-for (if (>= (list-ref bitsum position) half-len) #\1 #\0))
  (if (= (length data) 1)
    (car data)
    (oxy-rating (filter (lambda (x) (equal? (list-ref x position) look-for)) data) (modulo (add1 position) 12))))


(define (co2-rating data [position 0])
  (define bitsum (calc-bitsum data))
  (define half-len (/ (length data) 2))
  (define look-for (if (>= (list-ref bitsum position) half-len) #\0 #\1))
  (if (= (length data) 1)
    (car data)
    (co2-rating (filter (lambda (x) (equal? (list-ref x position) look-for)) data) (modulo (add1 position) 12))))

(* (bit-char->number (oxy-rating diagnostics)) (bit-char->number (co2-rating diagnostics)))
