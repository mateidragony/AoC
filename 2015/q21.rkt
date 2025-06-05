#lang racket

(require "utils.rkt")

(day 21)

(struct item (c d a) #:transparent)
(struct player (h d a) #:transparent)

(define weapons (list
                 (item 8 4 0)
                 (item 10 5 0)
                 (item 25 6 0)
                 (item 40 7 0)
                 (item 74 8 0)))

(define armors (list
                (item 0 0 0) ;; no armor
                (item 13 0 1)
                (item 31 0 2)
                (item 53 0 3)
                (item 75 0 4)
                (item 102 0 5)))

(define rings (list
               (item 25 1 0)
               (item 50 2 0)
               (item 100 3 0)
               (item 20 0 1)
               (item 40 0 2)
               (item 80 0 3)))

(define (attack a d)
  (player (- (player-h d) (max 1 (- (player-d a) (player-a d)))) (player-d d) (player-a d)))

(define (fight-boss p b)
  (cond
    ((<= (player-h b) 0) #t)
    ((<= (player-h p) 0) #f)
    (else (fight-boss (attack b p) (attack p b)))))

(define (parse-boss lines)
  (match-let ([(list _ h) (regexp-match #px"Hit Points: (\\d+)" (first lines))]
              [(list _ d) (regexp-match #px"Damage: (\\d+)" (second lines))]
              [(list _ a) (regexp-match #px"Armor: (\\d+)" (third lines))])
    (player (string->number h) (string->number d) (string->number a))))

(define (build-player w a rs)
  (define-values (rd ra)
    (for/fold ([rd 0] [ra 0]) ([r rs])
      (values (+ rd (item-d r)) (+ ra (item-a r)))))
  (player 100 (+ (item-d w) (item-d a) rd) (+ (item-a w) (item-a a) ra)))

(define (count-gold w a rs)
  (+ (item-c w) (item-c a) (sum (map item-c rs))))

(define boss (parse-boss lines))

;; Part One                
(printf "Part one: ~a\n" (inexact->exact
                          (apply min (for*/list ([w weapons]
                                                 [a armors]
                                                 [rs (append '(()) (combinations rings 1) (combinations rings 2))])
                                       (if (fight-boss (build-player w a rs) boss)
                                           (count-gold w a rs) +inf.0)))))

;; Part Two
(printf "Part two: ~a\n" (inexact->exact
                          (apply max (for*/list ([w weapons]
                                                 [a armors]
                                                 [rs (append '(()) (combinations rings 1) (combinations rings 2))])
                                       (if (fight-boss (build-player w a rs) boss)
                                           0 (count-gold w a rs))))))
