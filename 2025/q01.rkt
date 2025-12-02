#lang racket

(require "utils.rkt")

(day 1)
(testing #f)

(define (rotate cur acc instr [p1? #t])
  (match-let ([(list _ dir num) (regexp-match #px"(R|L)(\\d+)" instr)])
    (define n (if (equal? dir "R") (string->number num) (- (string->number num))))
    (values (modulo (+ cur n) 100)
            (if p1?
                (if (zero? cur) (add1 acc) acc)
                (begin
                  (+ acc (count-zero cur n)))))))

;; I tried mathing it and it failed me every time
;; this is pretty embarassing ngl
(define (count-zero cur n)
  (cond
    ((zero? n) 0)
    ((zero? cur) (add1 (count-zero (modulo (if (> n 0) (add1 cur) (sub1 cur)) 100) (if (> n 0) (sub1 n) (add1 n)))))
    (else (count-zero (modulo (if (> n 0) (add1 cur) (sub1 cur)) 100) (if (> n 0) (sub1 n) (add1 n))))))

(define-values (_0 p1)
  (for/fold ([c 50]
             [acc 0])
            ([line lines])
    (rotate c acc line)))

(define-values (_1 p2)
  (for/fold ([c 50]
             [acc 0])
            ([line lines])
    (rotate c acc line #f)))


;; Part One
(printf "Part one: ~a\n" p1)

;; Part Two
(printf "Part two: ~a\n" p2)
