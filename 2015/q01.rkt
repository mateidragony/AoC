#lang racket

(require "utils.rkt")

(day 1)
(testing #f)

(define part1
  (for*/fold ([level 0])
             ([line lines]
              [chr  line])
    (if (equal? chr #\()
        (add1 level)
        (sub1 level))))

;; Part One                
(printf "Part one: ~a\n" part1)

;; Part Two
(define (basement-instr instrs level cnt)
  (if (equal? -1 level)
      cnt
      (match instrs
        ('() -1)
        ((cons #\( instrs) (basement-instr instrs (add1 level) (add1 cnt)))
        ((cons #\) instrs) (basement-instr instrs (sub1 level) (add1 cnt))))))

(printf "Part two: ~a\n" (basement-instr (string->list (first lines)) 0 0))
