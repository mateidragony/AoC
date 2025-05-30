#lang racket

(require file/md5)
(require "utils.rkt")

(day 4)


;; Part One

(define (valid-hash? str)
  (regexp-match #rx"^00000+[1-9].*$" str))

(define (get-valid-hash str fn (n 0))
  (if (fn (md5 (string-append str (number->string n))))
      n (get-valid-hash str fn (add1 n))))

(printf "Part one: ~a\n" (get-valid-hash (first lines) valid-hash?))

;; Part Two
(define (valid-hash?^ str)
  (regexp-match #rx"^000000+[1-9].*$" str))

(printf "Part two: ~a\n" (get-valid-hash (first lines) valid-hash?^))
