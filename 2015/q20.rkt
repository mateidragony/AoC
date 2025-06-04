#lang racket

(require (only-in math divisors))
(require "utils.rkt")

(day 20)

;; Part One
(printf "Part one: ~a\n" (for/first ([i (in-naturals)]
                                     #:when (>= (sum (map (curry * 10) (divisors i)))
                                             (string->number (first lines))))
                           i))

;; Part Two
(printf "Part two: ~a\n" (for/first ([i (in-naturals)]
                                     #:when (>= (sum (map (curry * 11)
                                                          (filter (λ (v) (<= i (* 50 v))) (divisors i))))
                                             (string->number (first lines))))
                           i))

