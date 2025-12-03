#lang racket

(require "utils.rkt")

(day 3)
(testing #f)

;; Part One
(define memo (make-hash))
(define (max-joltage line digits)
  (cond
    ((dict-has-key? memo (cons line digits)) (dict-ref memo (cons line digits)))
    ((zero? digits) 0)
    (else (match line
            ('() -1)
            ((cons n line^)
             (define ex-jolts (max-joltage line^ digits))
             (define inc-jolts (max-joltage line^ (sub1 digits)))
             (define ret (max ex-jolts
                              (if (< inc-jolts 0) -1
                                  (+ (* (- (char->integer n) (char->integer #\0)) (expt 10 (sub1 digits)))
                                     inc-jolts))))
             (dict-set! memo (cons line digits) ret)
             ret)))))

(printf "Part one: ~a\n" (for/sum ([line lines]) (max-joltage (string->list line) 2)))

;; Part Two
(printf "Part two: ~a\n" (for/sum ([line lines]) (max-joltage (string->list line) 12)))
