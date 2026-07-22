#lang racket

(require "utils.rkt")

(day 18)
(testing #f)

;; Part One
(define (parse-input line)
  (for/vector ([c (string->list line)])
    (equal? c #\.)))

(define (mk-row row)
  (for/vector ([i (range (vector-length row))])
    (define l (if (equal? i 0) #t (vector-ref row (sub1 i))))
    (define c (vector-ref row i))
    (define r (if (equal? i (sub1 (vector-length row))) #t (vector-ref row (add1 i))))
    (not (or (and (not l) (not c) r)
             (and l (not c) (not r))
             (and (not l) c r)
             (and l c (not r))))))

(define (cnt-rows n row)
  (cond
    ((zero? n) 0)
    (else (+ (for/fold ([safe 0])
                       ([c row])
               (+ safe (b->n c)))
             (cnt-rows (sub1 n) (mk-row row))))))

(printf "Part one: ~a\n" (cnt-rows 40 (parse-input (first lines))))

;; part Two
(printf "Part two: ~a\n" (cnt-rows 400000 (parse-input (first lines))))
