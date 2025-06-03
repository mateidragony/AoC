#lang racket

(require "utils.rkt")

(day 16)

;; Part One
(define (parse-sues lines)
  (for/hash ([line lines])
    (match-let* ([(list _ n) (regexp-match #px"Sue (\\d+):" line)]
                 [ms         (regexp-match* #px"([a-zA-Z]+): (\\d+)" line)])
      (values (string->number n)
              (for/hash ([m ms])
                (match-let ([(list _ k v) (regexp-match #px"([a-zA-Z]+): (\\d+)" m)])
                  (values k (string->number v))))))))

(define expected
  (make-immutable-hash (list (cons "children" 3)
                             (cons "cats" 7)
                             (cons "samoyeds" 2)
                             (cons "pomeranians" 3)
                             (cons "akitas" 0)
                             (cons "vizslas" 0)
                             (cons "goldfish" 5)
                             (cons "trees" 3)
                             (cons "cars" 2)
                             (cons "perfumes" 1))))


(define (is-sue stats expected)
  (for/and ([(k v) expected])
    (equal? v (dict-ref stats k v))))

(define (find-sue sues expected (p is-sue))
  (for/fold ([sue #f])
            ([(s stats) sues])
    (if (p stats expected)
        s sue)))

(define sues (parse-sues lines))

(printf "Part one: ~a\n" (find-sue sues expected))

;; Part Two
(define (is-sue^ stats expected)
  (for/and ([(k v) expected])
    (cond
      ((or (equal? "cats" k) (equal? "trees" k)) (< v (dict-ref stats k +inf.0)))
      ((or (equal? "pomeranians" k) (equal? "goldfish" k)) (> v (dict-ref stats k -inf.0)))
      (else (equal? v (dict-ref stats k v))))))

(printf "Part two: ~a\n" (find-sue sues expected is-sue^))
