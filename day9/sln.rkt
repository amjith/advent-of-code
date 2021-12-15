#lang racket

(require threading)

(define heightmap
  (for/list ([l (file->lines "input.txt")])
            (filter (lambda (x) x) (map string->number (string-split l "")))
            ))

(define rowmax (sub1 (length heightmap)))
(define colmax (sub1 (length (car heightmap))))

(define original-matrix
  (for*/hash ([r (add1 rowmax)] [c (add1 colmax)])
             (values (list r c) (list-ref (list-ref heightmap r) c)))
  )


(define (get-neighbors coord matrix)
  (match coord
         [(list 0 0) (list (list 0 1) (list 1 0))]
         [(list 0 (== colmax)) (list (list 0 (sub1 colmax)) (list 1 colmax))]
         [(list (== rowmax) 0) (list (list (sub1 rowmax) 0) (list rowmax 1))]
         [(list 0 c) (list (list 0 (sub1 c)) (list 0 (add1 c)) (list 1 c))]
         [(list r 0) (list (list (sub1 r) 0) (list (add1 r) 0) (list r 1))]
         [(list (== rowmax) (== colmax)) (list (list rowmax (sub1 colmax)) (list (sub1 rowmax) colmax))]
         [(list (== rowmax) c) (list (list (sub1 rowmax) c) (list rowmax (add1 c)) (list rowmax (sub1 c)))]
         [(list r (== colmax)) (list (list r (sub1 colmax)) (list (sub1 r) colmax) (list (add1 r) colmax))]
         [(list r c) (list (list r (sub1 c)) (list r (add1 c)) (list (add1 r) c) (list (sub1 r) c))]
         ))

(define (is-min? coord neighbors matrix)
  (for/and ([neighbor (in-list neighbors)]) (< (hash-ref matrix coord) (hash-ref matrix neighbor))))

(define (basins matrix)
  (filter (lambda (x) x) (for/list ([(coord height) (in-hash matrix)])
             (let* 
               ([neighbors (get-neighbors coord matrix)])
               (if (is-min? coord neighbors matrix) coord #f)))))

;(define visited (make-hash))

(define (dfs coord matrix visited)
  (begin 
    (hash-set! visited coord #t)
    (for/sum ([neighbor (in-list (get-neighbors coord matrix))]
          #:unless (or (hash-has-key? visited neighbor) (= (hash-ref matrix neighbor) 9))
          )
         (dfs neighbor matrix visited))))

(define (part2 matrix)
  (let* 
    ([basin-hash (make-hash)])
    (for ([basin (in-list (basins matrix))])
         (begin 
           (hash-set! basin-hash basin (make-hash))
           (dfs basin matrix (hash-ref basin-hash basin)))
         ) 
    (define basin-size (for/list ([(k v) (in-hash basin-hash)])
                          (hash-count v)))
    (apply * (take (sort basin-size >) 3))
    )
  )
