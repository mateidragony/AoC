#lang racket

(require graph)
(require "utils.rkt")

(day 9)

;; Part One
(define (build-graph dists (g (weighted-graph/undirected '())))
  (match dists
    ('() g)
    ((cons str dists)
     (match-let ([(list _  s d w) (regexp-match #px"([a-zA-z]+) to ([a-zA-z]+) = (\\d+)" str)])
       (add-edge! g s d (string->number w))
       (build-graph dists g)))))

(define (get-len p g)
  (match p
    ('() 0)
    ((cons _ '()) 0)
    ((cons s (cons d p)) (+ (edge-weight g s d) (get-len (cons d p) g)))))

(define g (build-graph lines))
(printf "Part one: ~a\n" (apply min (map (λ (p) (get-len p g)) (permutations (get-vertices g)))))

;; Part Two
(printf "Part two: ~a\n" (apply max (map (λ (p) (get-len p g)) (permutations (get-vertices g)))))
