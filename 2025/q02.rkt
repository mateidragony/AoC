#lang racket

(require "utils.rkt")

(day 2)
(testing #f)

;; Part One
(define (check-range s e [p1? #t])
  (if (> s e) 0
      (if ((if p1? check-invalid check-invalid^) (number->string s))
          (+ s (check-range (add1 s) e p1?))
          (check-range (add1 s) e p1?))))

(define (check-invalid s)
  (if (not (zero? (modulo (string-length s) 2))) #f
   (regexp-match-exact? (format "(~a)+" (substring s 0 (quotient (string-length s) 2))) s)))

(define (check-invalid^ s)
  (for/or ([i (range 1 (string-length s))])
    (regexp-match-exact? (format "(~a)+" (substring s 0 i)) s)))

(printf "Part one: ~a\n" (for/fold ([total 0])
                                   ([r (string-split (first lines) ",")])
                           (match-let ([(list _ s e) (regexp-match #px"(\\d+)-(\\d+)" r)])
                             (+ total (check-range (string->number s) (string->number e))))))

;; Part Two
(printf "Part two: ~a\n" (for/fold ([total 0])
                                   ([r (string-split (first lines) ",")])
                           (match-let ([(list _ s e) (regexp-match #px"(\\d+)-(\\d+)" r)])
                             (+ total (check-range (string->number s) (string->number e) #f)))))
