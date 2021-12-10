#lang racket

(require threading)   ; imported for the ~> operator.

(define data
  (for/list ([l (file->lines "input.txt")]
             #:unless (equal? l ""))
            l))

(define bingo-numbers
  (~> data
      car
      (string-split ",")
      (map string->number _)))

(define raw-cards
  (~> data
      cdr
      (map string-split _)
      (map (lambda (row) (map string->number row)) _)))

(define (transpose matrix)
  (define rows (length matrix))
  (define cols (length (list-ref matrix 0)))
  (for/list ([i cols])
            (for/list ([j rows])
                      (list-ref (list-ref matrix j) i))))

(define card%
  (class object%
         (init nums)
         (super-new)
         (define numbers nums)
         (define/public (get-nums) numbers)
         (define/public (mark bingo-number)
                        (new card% [nums
                        (for/list ([row (in-list numbers)])
                                  (for/list ([item (in-list row)])
                                            (if (equal? item bingo-number) 'X item)))]))
         (define (check-rows)
                        (> (length (filter (lambda (x) (equal? (set 'X) x)) (map list->set numbers))) 0))
         (define (check-columns)
                        (> (length (filter (lambda (x) (equal? (set 'X) x)) (map list->set (transpose numbers)))) 0))
         (define/public (check)
                        (or (check-rows) (check-columns)))
         (define/public (score)
                        (foldl (lambda (val acc) (+ acc (if (number? val) val 0))) 0 (flatten numbers)))
         )
  )

(define (split-by lst n)
  (if (empty? lst)
    '()
    (cons (take lst n) (split-by (drop lst n) n))
    ))

(define initial-cards 
  (map (lambda (c) (new card% [nums c])) (split-by raw-cards 5)))

; part 1
(define (calc-score bingo-calls)
  (for/fold 
    ([cards initial-cards] [last-call-number (car bingo-calls)]
     #:result (* last-call-number (send (findf (lambda (x) (send x check))  cards) score)))
     ;#:result (~> cards
     ;             (findf (lambda (x) (send x check)) _)
     ;             (* last-call-number (send _ score)))
     ;)
    ([call-number (in-list bingo-calls)])
    #:break (foldl (lambda (x acc) (or x acc)) #f (map (lambda (x) (send x check)) cards))
    (values (map (lambda (x) (send x mark call-number)) cards) call-number)
    ))

; part 2
(define (calc-score-2 bingo-calls)
  (for/fold 
    ([cards initial-cards] [last-call-number (car bingo-calls)]
     #:result (* last-call-number (send (findf (lambda (x) (send x check))  cards) score)))
     ;#:result (~> cards
     ;             (findf (lambda (x) (send x check)) _)
     ;             (* last-call-number (send _ score)))
     ;)
    ([call-number (in-list bingo-calls)])
    #:break (and (= 1 (length cards)) (foldl (lambda (x acc) (or x acc)) #f (map (lambda (x) (send x check)) cards)))
    (values (for/list ([card (in-list cards)]
                       #:unless (send card check)
                       )
                      (send card mark call-number))
                      call-number)
    ))

