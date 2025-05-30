#lang racket

(require "utils.rkt")

(day 5)

;; Part One
(define (num-vowels str)
  (length (regexp-match* #rx"[aeiou]" str)))

(define (double-letters str)
  (regexp-match #px"(.)\\1" str))

(define (has-evil-letters str)
  (regexp-match #rx"ab|cd|pq|xy" str))

(define (nice? str)
  (and (>= (num-vowels str) 3) (double-letters str) (not (has-evil-letters str))))

(printf "Part one: ~a\n" (length (filter nice? lines)))

;; Part Two
(define (pair-double str)
  (regexp-match #px"(..).*\\1" str))

(define (repeat-split str)
  (regexp-match #px"(.).\\1" str))

(define (nice?^ str)
  (and (pair-double str) (repeat-split str)))

(printf "Part two: ~a\n" (length (filter nice?^ lines)))
