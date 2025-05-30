#lang racket

(require "utils.rkt")

(day 2)

;; Part One
(define (get-surface-area str)
  (match-let ([(list _ l w h) (map string->number (regexp-match #px"(\\d+)x(\\d+)x(\\d+)" str))])
    (+ (* 2 l w) (* 2 w h) (* 2 l h)
       (min (* l w) (* w h) (* l h)))))

(printf "Part one: ~a\n" (sum (map get-surface-area lines)))

;; Part Two
(define (get-ribbon str)
  (match-let ([(list _ l w h) (map string->number (regexp-match #px"(\\d+)x(\\d+)x(\\d+)" str))])
    (+ (min (+ (* 2 l) (* 2 w))
            (+ (* 2 w) (* 2 h))
            (+ (* 2 l) (* 2 h)))
       (* l w h))))

(printf "Part two: ~a\n" (sum (map get-ribbon lines)))
