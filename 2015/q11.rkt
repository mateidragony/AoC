#lang racket

(require "utils.rkt")

(day 11)

;; Part One
(define (incr-string str)
  (cond
    ((string=? str "") "")
    ((equal? #\z (string-ref str (sub1 (string-length str))))
             (string-append (incr-string (substring str 0 (sub1 (string-length str)))) "a"))
    (else (string-append (substring str 0 (sub1 (string-length str))) ;; holy spaghetti code
                         (string (integer->char (add1 (char->integer (string-ref str (sub1 (string-length str)))))))))))

(define (password-valid? str)
  (and (regexp-match #rx"abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz" str)
       (not (regexp-match #rx"i|o|l" str))
       (regexp-match #px"(.)\\1.*(.)\\2" str)))

(define (new-password str)
  (if (password-valid? str) str (new-password (incr-string str))))

(printf "Part one: ~a\n" (new-password (first lines)))

;; Part Two
(printf "Part two: ~a\n" (new-password (incr-string (new-password (first lines)))))
