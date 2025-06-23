#lang racket

(require "utils.rkt")

(day 3)
(testing #f)

(define (get-sides str)
  (map string->number (regexp-match* #px"(\\d+)" str)))

(define (possible? sides)
  (> (+ (first sides) (second sides)) (third sides)))

;; Part One
(printf "Part one: ~a\n" (length (filter possible? (map (curryr sort <) (map get-sides lines)))))

(define-values (lines^ _)
  (for/fold ([final '()]
             [tmp '(() () ())])
            ([line (map get-sides lines)])
    (define tmp^ (list (cons (first line) (first tmp))
                       (cons (second line) (second tmp))
                       (cons (third line) (third tmp))))
    (if (equal? 3 (length (first tmp^)))
        (values (append tmp^ final) '(() () ()))
        (values final tmp^))))

;; Part Two
(printf "Part two: ~a\n" (length (filter possible? (map (curryr sort <) lines^))))
