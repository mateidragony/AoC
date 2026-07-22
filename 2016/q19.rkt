#lang racket

(require "utils.rkt")

(day 19)
(testing #f)

;; Part One
;; https://en.wikipedia.org/wiki/Josephus_problem
(define (get-l n (pow 1))
  (if (< n (* 2 pow))
      (- n pow)
      (get-l n (* 2 pow))))

(printf "Part one: ~a\n" (add1 (* 2 (get-l (string->number (first lines))))))

;; Part Two
(define (p2 n (i 1))
  (cond
    ((>= (* i 3) n) (- n i))
    (else (p2 n (* i 3)))))

(printf "Part two: ~a\n" (p2 (string->number (first lines))))
