#lang racket

(require "utils.rkt")

(day 9)
(testing #f)

;; Part One
(define (decompress str (i 0) #:q2? (q2? #f))
  (define m (regexp-match #px"([^\\(]*)\\((\\d+)x(\\d+)\\)" str (min (string-length str) i)))
  (cond
    ((>= i (string-length str)) 0)
    ((not m) (- (string-length str) i))
    (else (match-let ([(list total extra subs repeat) m])
            (define marked (if q2?
                               (decompress (substring str (+ i (string-length total))
                                                      (+ i (string-length total) (string->number subs)))
                                           #:q2? #t)
                               (string->number subs)))
            (+ (string-length extra) (* marked (string->number repeat))
               (decompress str (+ i (string-length total) (string->number subs)) #:q2? q2?))))))

(printf "Part one: ~a\n" (decompress (first lines)))

;; Part Two
(printf "Part two: ~a\n" (decompress (first lines) #:q2? #t))
