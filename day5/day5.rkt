#lang racket

(require threading)

(define (string->pairofnums x)
  (~> (string-split x ",")
      (map string->number _)
      ))

(define coords
  (~> (file->lines "input.txt")
    (map string-split _)
    (map (lambda (x) 
           (match x
                [(list a _ b) (list (string->pairofnums a) (string->pairofnums b))])) _)
    (map flatten _)))

(define results (make-hash))

(define (fill-x-line y x1 x2)
  (for ([x (in-inclusive-range x1 x2)])
       (hash-set! results (list x y) (add1 (hash-ref results (list x y) 0)))
       ))

(define (fill-y-line x y1 y2)
  (for ([y (in-inclusive-range y1 y2)])
       (hash-set! results (list x y) (add1 (hash-ref results (list x y) 0)))
       ))

(define (fill-xy-line x1 y1 x2 y2)
  (for ([x (in-inclusive-range x1 x2 (if (< x1 x2) 1 -1))] [y (in-inclusive-range y1 y2 (if (< y1 y2) 1 -1))])
       (hash-set! results (list x y) (add1 (hash-ref results (list x y) 0)))
       ))

(define (fill-results vent-coords)
  (for ([coord (in-list vent-coords)])
       (match coord
              [(list x y1 x y2) (fill-y-line x (min y1 y2) (max y1 y2))]
              [(list x1 y x2 y) (fill-x-line y  (min x1 x2) (max x1 x2))]
              [(list x1 y1 x2 y2) (fill-xy-line x1 y1 x2 y2)]
              ;[_ #f]
              )
       ))

(fill-results coords)

(length (for/list ([(k v) (in-hash results)] #:when (> v 1))
  k))
