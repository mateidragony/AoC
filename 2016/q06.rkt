#lang racket

(require "utils.rkt")

(day 6)
(testing #f)

(define freqs
  (for/fold ([h (make-immutable-hash)])
            ([line lines]
             #:when #t
             [c line]
             [i (range (string-length line))])
    (define ih (dict-ref h i (make-immutable-hash)))
    (dict-set h i (dict-set ih c (add1 (dict-ref ih c 0))))))

;; Part One
(printf "Part one: ~a\n" (list->string
                          (for/list ([i (range (string-length (first lines)))])
                            (define h (dict-ref freqs i))
                            (define-values (l _) (dict-max-k/v h >))
                            l)))

;; Part Two
(printf "Part two: ~a\n" (list->string
                          (for/list ([i (range (string-length (first lines)))])
                            (define h (dict-ref freqs i))
                            (define-values (l _) (dict-max-k/v h <))
                            l)))
