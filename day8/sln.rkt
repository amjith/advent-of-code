#lang racket

(require threading)
(require racket/hash)

(define (string->set s)
  (list->set (string->list s)))

(define signals
  (for/list ([l (file->lines "input.txt")])
            (match-define (list input output) (string-split l "|"))
            (hash "input" (map string->set (string-split input)) "output" (map string->set (string-split output)))))

; part 1

(define (line-tally l)
  (map set-count l))

(define (count-chars signals)
  (~>> signals
      (map (lambda (x) (hash-ref x "output")))
      (map line-tally))
  )

(define (ones l)
  (count (lambda (x) (= x 2)) l))

(define (fours l)
  (count (lambda (x) (= x 4)) l))

(define (seven l)
  (count (lambda (x) (= x 3)) l))

(define (eight l)
  (count (lambda (x) (= x 7)) l))

(+
       (apply + (map ones (count-chars signals)))
       (apply + (map fours (count-chars signals)))
       (apply + (map seven (count-chars signals)))
       (apply + (map eight (count-chars signals)))
       )

;(define nums 
;  (for/hash ([item (in-list line)]
;             :#when (set-member? (set 2 3 4 7) (string-length item)))
;            (cond
;              [(= 2 (length item)) (values 1 item)]
;              [(= 4 (length item)) (values 4 item)]
;              [(= 3 (length item)) (values 7 item)]
;              [(= 7 (length item)) (values 8 item)]
;              )
;            )
;  )

; part 2

(define (build-easy-mapping xs) 
  (for/hash ([item (in-list xs)]
             #:when (set-member? (set 2 3 4 7) (set-count item))
             )
            (cond
              [(= 2 (set-count item)) (values 1 item)]
              [(= 4 (set-count item)) (values 4 item)]
              [(= 3 (set-count item)) (values 7 item)]
              [(= 7 (set-count item)) (values 8 item)]
              )))

(define (build-hard-mapping xs decoder-hash)
  (for/hash ([item (in-list xs)]
            #:when (set-member? (set 5 6) (set-count item))
            )
            (cond
              [(= 5 (set-count item)) 
               (cond 
                 [(= 3 (set-count (set-intersect item (hash-ref decoder-hash 7)))) (values 3 item)]
                 [(= 2 (set-count (set-intersect item (hash-ref decoder-hash 4)))) (values 2 item)]
                 [else (values 5 item)])]
              [(= 6 (set-count item))
               (cond 
                 [(= 1 (set-count (set-intersect item (hash-ref decoder-hash 1)))) (values 6 item)]
                 [(= 4 (set-count (set-intersect item (hash-ref decoder-hash 4)))) (values 9 item)]
                 [else (values 0 item)])])
            ))

(define (reverse-hash h)
  (for/hash ([(k v) (in-hash h)])
            (values v k)))

(define (apply-mappings xs mappings)
  (string->number 
    (apply ~a 
           (for/list ([item (in-list xs)])
                     (hash-ref mappings item)))))

(define part2 
  (for/list ([x (in-list signals)])
              (let* 
                ([input (hash-ref x "input")]
                 [output (hash-ref x "output")]
                 [easy-mapping (build-easy-mapping input)]
                 [hard-mapping (build-hard-mapping input easy-mapping)]
                 [digit->display (hash-union easy-mapping hard-mapping)]
                 [display->digit (reverse-hash digit->display)]
                 )
                (apply-mappings output display->digit)
                )))

