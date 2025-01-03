#lang racket

(require racket/set)
(require "utils.rkt")

(day 25)
(testing #f)

;; Part One
(define (parse-input lines [keys '()] [locks '()])
  (define-values (cur rst) (splitf-at lines (negate (curry equal? ""))))
  (cond
    ((null? lines) (values keys locks))
    ((null? cur) (parse-input (cdr rst) keys locks))
    ((equal? (first cur) ".....")
     (parse-input rst (cons (grid->heights (reverse (map string->list cur))) keys) locks))
    (else
     (parse-input rst keys (cons (grid->heights (map string->list cur)) locks)))))

(define (grid->heights grid)
  (for/list ([i (range 5)])
    (for/fold ([h -1])
              ([j (range 6)])
      (if (equal? #\. (list-ref (list-ref grid j) i))
          h (add1 h)))))

(define-values (keys locks) (parse-input lines))

(define (overlap? key lock)
  (for/and ([k key] [l lock])
    (> 6 (+ k l))))

(printf "Part one: ~a\n"
        (length (filter id (for*/list ([k keys] [l locks])
                             (overlap? k l)))))

;; Part Two
(printf "Part two: ~a\n" "Done!")
