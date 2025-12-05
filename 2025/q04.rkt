#lang racket

(require "utils.rkt")

(day 4)
(testing #f)

;; Part One
(define (num-neighbors i j chars)
  (for*/sum ([di (inclusive-range -1 1)]
             [dj (inclusive-range -1 1)])
    (b->n (and (nand (equal? di 0) (equal? dj 0))
               (equal? #\@ (2d-ref chars (+ i di) (+ j dj) #\.))))))

(printf "Part one: ~a\n" (for*/sum ([i (range rows)]
                                    [j (range cols)])
                           (b->n (and (equal? (2d-ref chars i j) #\@)
                                      (< (num-neighbors i j chars) 4)))))

;; Part Two
(define (remove-paper! chars)
  (for*/sum ([i (range rows)]
             [j (range cols)])
    (cond
      ((and (equal? (2d-ref chars i j) #\@)
            (< (num-neighbors i j chars) 4))
       (2d-set! chars i j #\x) 1)
      (else 0))))

(define (remove-all chars)
  (define removed (remove-paper! chars))
  (if (zero? removed)
      removed
      (+ removed (remove-all chars))))

(printf "Part two: ~a\n" (remove-all chars))
