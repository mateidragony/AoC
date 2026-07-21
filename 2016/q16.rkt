#lang racket

(require "utils.rkt")

(day 16)
(testing #f)

;; Part One
(define (parse-input line)
  (for/list ([c (string->list line)])
    (match c
      (#\0 #f)
      (#\1 #t))))

(define (dragon-step line)
  (append line (list #f) (map not (reverse line))))

(define (fill-disk line len)
  (cond
    ((>= (length line) len) (take line len))
    (else (fill-disk (dragon-step line) len))))

(define (check-sum-step line)
  (for/list ([p (chunk-by line 2)])
    (not (xor (first p) (second p)))))

(define (check-sum line)
  (cond
    ((odd? (length line)) line)
    (else (check-sum (check-sum-step line)))))

(define (bool-ls->str ls)
  (list->string (for/list ([b ls]) (if b #\1 #\0))))

(printf "Part one: ~a\n" (bool-ls->str (check-sum (fill-disk (parse-input (first lines)) 272))))

(define p2-len 35651584)
(define p2-mem (make-vector p2-len))

(define (parse-input! mem line)
  (for ([c (string->list line)]
        [i (range (string-length line))])
    (vector-set! mem i (match c
                         (#\0 #f)
                         (#\1 #t)))))

(define (dragon-step! mem cur-size)
  (vector-set! mem cur-size #f)
  (for ([i (range cur-size)])
    (define i^ (- (* cur-size 2) i))
    (when (< i^ (vector-length mem))
      (vector-set! mem i^ (not (vector-ref mem i))))))

(define (fill-disk! mem cur-size len)
  (when (< cur-size len)
    (dragon-step! mem cur-size)
    (fill-disk! mem (add1 (* 2 cur-size)) len)))

(parse-input! p2-mem (first lines))
(fill-disk! p2-mem (string-length (first lines)) p2-len)

;; Part Two
(printf "Part two: ~a\n" (bool-ls->str (check-sum (vector->list p2-mem))))
