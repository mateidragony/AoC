#lang racket

(require "utils.rkt")

(day 10)

;; Part One
(define (expand str)
  (define repeats (regexp-match* #px"(.)\\1*" str))
  (string-join (for/list ([r repeats])
                 (string-append (number->string (string-length r)) (substring r 0 1)))
               ""))

(define (expand-n str n)
  (if (zero? n)
      str (expand-n (expand str) (sub1 n))))

(printf "Part one: ~a\n" (string-length (expand-n (first lines) 40)))

;; Part Two
(printf "Part two: ~a\n" (string-length (expand-n (first lines) 50)))
