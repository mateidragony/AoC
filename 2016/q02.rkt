#lang racket

(require "utils.rkt")

(day 2)
(testing #f)

;; Part One
(define (run-line instrs pos)
  (match instrs
    ('() pos)
    ((cons i instrs)
     (match i
       (#\U (run-line instrs (struct-copy point pos [y (max (sub1 (point-y pos)) 0)])))
       (#\D (run-line instrs (struct-copy point pos [y (min (add1 (point-y pos)) 2)])))
       (#\L (run-line instrs (struct-copy point pos [x (max (sub1 (point-x pos)) 0)])))
       (#\R (run-line instrs (struct-copy point pos [x (min (add1 (point-x pos)) 2)])))))))

(define (pos->number pos)
  (match pos
    ((point 0 0) "1")
    ((point 1 0) "2")
    ((point 2 0) "3")
    ((point 0 1) "4")
    ((point 1 1) "5")
    ((point 2 1) "6")
    ((point 0 2) "7")
    ((point 1 2) "8")
    ((point 2 2) "9")))

(define (get-code lines (p2? #f))
  (define-values (ret _)
    (for/fold ([code ""] [pos (if p2? (point 0 2) (point 1 1))])
              ([line lines])
      (define pos^ ((if p2? run-line^ run-line) (string->list line) pos))
      (values (string-append code ((if p2? pos->number^ pos->number) pos^)) pos^)))
  ret)

(printf "Part one: ~a\n" (get-code lines))

;; Part Two
(define (get-movement dir)
  (match dir
    (#\U (point 0 -1))
    (#\D (point 0 1))
    (#\L (point -1 0))
    (#\R (point 1 0))))

(define (pos->no-dir pos)
  (match pos
    ((point 2 0) (list #\U #\L #\R))
    ((point 3 1) (list #\U #\R))
    ((point 4 2) (list #\U #\D #\R))
    ((point 3 3) (list #\D #\R))
    ((point 2 4) (list #\D #\L #\R))
    ((point 1 3) (list #\D #\L))
    ((point 0 2) (list #\U #\D #\L))
    ((point 1 1) (list #\U #\L))
    (_ '())))

(define (pos->number^ pos)
  (match pos
    ((point 2 0) "1")
    ((point 1 1) "2")
    ((point 2 1) "3")
    ((point 3 1) "4")
    ((point 0 2) "5")
    ((point 1 2) "6")
    ((point 2 2) "7")
    ((point 3 2) "8")
    ((point 4 2) "9")
    ((point 1 3) "A")
    ((point 2 3) "B")
    ((point 3 3) "C")
    ((point 2 4) "D")))

(define (run-line^ instrs pos)
  (match instrs
    ('() pos)
    ((cons i instrs)
     (run-line^ instrs
                (if (member i (pos->no-dir pos))
                    pos (add-points pos (get-movement i)))))))

(printf "Part two: ~a\n" (get-code lines #t))
