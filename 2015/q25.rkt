#lang racket

(require "utils.rkt")

(day 25)

(define FIRST-CODE 20151125)
(define CODE-MULT 252533)
(define CODE-MOD 33554393)

;; Part One
(define (new-code c)
  (modulo (* c CODE-MULT) CODE-MOD))

(define (generate-code r c)
  (cond
    ((and (equal? r 1) (equal? c 1)) FIRST-CODE)
    ((equal? c 1) (new-code (generate-code 1 (sub1 r))))
    (else (new-code (generate-code (add1 r) (sub1 c))))))

(define-values (gr gc) (match-let ([(list _ r c) (regexp-match #px"To continue, please consult the code grid in the manual.  Enter the code at row (\\d+), column (\\d+)." (first lines))])
                         (values (string->number r) (string->number c))))

(printf "Part one: ~a\n" (generate-code gr gc))

;; Part Two
(printf "Part two: ~a\n" "Done!")
