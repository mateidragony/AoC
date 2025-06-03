#lang racket

(require "utils.rkt")

(day 17)

(define bottles (map string->number lines))

;; Part One
(define (fill-combos bs remaining)
  (match* (bs remaining)
    [(_ 0) `(,(cons '() 1))]
    [('() _) `(,(cons '() 0))]
    [((cons b bs) _) #:when (> b remaining) (fill-combos bs remaining)]
    [((cons b bs) _) (append
                      (map (λ (v) (cons (cons b (car v)) (cdr v))) (fill-combos bs (- remaining b)))
                      (fill-combos bs remaining))]))

(define combos (filter (λ (v) (not (zero? (cdr v)))) (fill-combos bottles 150)))
(printf "Part one: ~a\n" (length combos))

;; Part Two
(define min-num (apply min (map (compose length car) combos)))
(printf "Part two: ~a\n" (length (filter (λ (v) (equal? min-num (length (car v)))) combos)))
