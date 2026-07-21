#lang racket

(require "utils.rkt")

(day 15)
(testing #f)

;;;
;;; I love CRT!!! (critical race theory (chinese remainder theorem))
;;;

;; Part One
(struct cong (n mod) #:transparent)

(define (parse-input lines)
  (for/list ([line lines])
    (match-let
        ([(list _ idx pos start)
          (regexp-match #px"Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+)"
                        line)])
      (define n (+ (string->number start) (string->number idx)))
      (define mod (string->number pos))
      (cong (- mod (modulo n mod)) mod))))

(define (extended-euclidean a b)
  (cond
    ((zero? b) (values 1 0 a))
    (else
     (define-values (x^ y^ g) (extended-euclidean b (modulo a b)))
     (values y^ (- x^ (* y^ (quotient a b))) g))))

(define (modular-inverse a m)
  (define-values (x y g) (extended-euclidean a m))
  (modulo (+ (modulo x m) m) m))

(define (crt2 c1 c2)
  (match* (c1 c2)
    (((cong a1 m1) (cong a2 m2))
     (define n1 (modular-inverse m1 m2))
     (define n2 (modular-inverse m2 m1))
     (cong (modulo (+ (* a1 n2 m2) (* a2 m1 n1)) (* m1 m2))
           (* m1 m2)))))

(define (crt . congs)
  (match congs
    ('() (cong 0 1))
    ((cons c cs) (crt2 c (apply crt cs)))))

(printf "Part one: ~a\n" (cong-n (apply crt (parse-input lines))))

;; Part Two
(printf "Part two: ~a\n" (cong-n (crt (apply crt (parse-input lines))
                                      (cong (- 11 (add1 (length lines))) 11))))
