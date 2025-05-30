#lang racket

(require "utils.rkt")

(day 6)

;; Part One
(define (set-grid! fn sc sr ec er grid)
  (for* ([i (inclusive-range sr er)]
         [j (inclusive-range sc ec)])
    (2d-set! grid i j (fn (2d-ref grid i j)))))

(define (run-instrs instrs)
  (define grid (for/vector ([i (range 1000)])
                 (make-vector 1000 #f)))
  (for ([i instrs])
    (match-let ([(list _ cmd sc sr ec er)
                 (regexp-match #px"(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)" i)])
      (define fn (match cmd
                   ("turn on"  (const #t))
                   ("turn off" (const #f))
                   ("toggle"   not)))
      (set-grid! fn (string->number sc) (string->number sr) (string->number ec) (string->number er) grid)))
  (for*/fold ([s 0])
             ([i (range 1000)]
              [j (range 1000)])
    (if (2d-ref grid i j) (add1 s) s)))

(printf "Part one: ~a\n" (run-instrs lines))

;; Part Two
(define (run-instrs^ instrs)
  (define grid (for/vector ([i (range 1000)])
                 (make-vector 1000 0)))
  (for ([i instrs])
    (match-let ([(list _ cmd sc sr ec er)
                 (regexp-match #px"(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)" i)])
      (define fn (match cmd
                   ("turn on"  add1)
                   ("turn off" (λ (x) (max 0 (sub1 x))))
                   ("toggle"   (λ (x) (+ x 2)))))
      (set-grid! fn (string->number sc) (string->number sr) (string->number ec) (string->number er) grid)))
  (for*/fold ([s 0])
             ([i (range 1000)]
              [j (range 1000)])
    (+ s (2d-ref grid i j))))

(printf "Part two: ~a\n" (run-instrs^ lines))
