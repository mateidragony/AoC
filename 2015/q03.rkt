#lang racket

(require "utils.rkt")

(day 3)

;; Part One

(define (deliver-gifts instrs (pos (point 0 0)) (visited (set (point 0 0))))
  (match instrs
    ('() visited)
    ((cons dir instrs)
     (define pos^
       (match dir
         (#\^ (point (point-x pos) (add1 (point-y pos))))
         (#\v (point (point-x pos) (sub1 (point-y pos))))
         (#\> (point (add1 (point-x pos)) (point-y pos)))
         (#\< (point (sub1 (point-x pos)) (point-y pos)))))
     (deliver-gifts instrs pos^ (set-add visited pos^)))))

(printf "Part one: ~a\n" (set-count (deliver-gifts (string->list (first lines)))))

;; Part Two
(define (split-indices lst)
  (match lst
    ('() (values '() '()))
    ((cons v '()) (values (cons v '()) '()))
    ((cons e (cons o lst))
     (define-values (el ol) (split-indices lst))
     (values (cons e el) (cons o ol)))))

(define-values (s rs) (split-indices (string->list (first lines))))
(printf "Part two: ~a\n" (set-count (set-union (deliver-gifts s) (deliver-gifts rs))))
