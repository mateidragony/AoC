#lang racket

(require "utils.rkt")

(day 18)

;; Part One
(define (num-on-neighbors g i j)
  (for*/sum ([i^ (inclusive-range (sub1 i) (add1 i))]
             [j^ (inclusive-range (sub1 j) (add1 j))])
    (cond
      ((and (equal? i i^) (equal? j j^)) 0)
      ((equal? #\# (2d-ref g i^ j^ #\.)) 1)
      (else                              0))))

(define (corner? i j rows cols)
  (or (and (equal? i 0) (equal? j 0))
      (and (equal? i 0) (equal? j (sub1 cols)))
      (and (equal? i (sub1 rows)) (equal? j 0))
      (and (equal? i (sub1 rows)) (equal? j (sub1 cols)))))

(define (update g g^ p2?)
  (for* ([i (range rows)]
         [j (range cols)])
    (define ns (num-on-neighbors g i j))
    (define c (2d-ref g i j))
    (cond
      ((and p2? (corner? i j rows cols))
       (2d-set! g^ i j #\#))
      ((and (equal? c #\#) (not (or (equal? ns 2) (equal? ns 3))))
       (2d-set! g^ i j #\.))
      ((and (equal? c #\.) (equal? ns 3))
       (2d-set! g^ i j #\#)))))


(define (update-n g n (p2? #f))
  (cond
    ((equal? 0 n) g)
    (else
     (define g^ (2d-vec-copy g))
     (update g g^ p2?)
     (update-n g^ (sub1 n) p2?))))

(printf "Part one: ~a\n" (for*/sum ([row (update-n chars 100)]
                                    [c row])
                           (if (equal? c #\#) 1 0)))

;; Part Two
(2d-set! chars 0 0 #\#)
(2d-set! chars 0 (sub1 cols) #\#)
(2d-set! chars (sub1 rows) 0 #\#)
(2d-set! chars (sub1 rows) (sub1 cols) #\#)
(printf "Part two: ~a\n" (for*/sum ([row (update-n chars 100 #t)]
                                    [c row])
                           (if (equal? c #\#) 1 0)))
