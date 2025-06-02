#lang racket

(require graph)
(require "utils.rkt")

(day 13)

;; Part One
(define (build-graph lines)
  (define g (weighted-graph/undirected '()))
  (for ([line lines])
    (match-let ([(list _ s i w d) (regexp-match #px"([a-zA-z]+) would (gain|lose) (\\d+) happiness units by sitting next to ([a-zA-z]+)" line)])
      (let ((w (if (string=? i "gain")
                   (string->number w)
                   (* -1 (string->number w)))))
        (add-edge! g s d (if (has-edge? g s d)
                             (+ (edge-weight g s d) w)
                             w)))))
  g)

(define g (build-graph lines))

(define (valid-graph g)
  (for/and ([v (in-vertices g)])
    (equal? 2 (length (get-neighbors g v)))))

(define (brute-force g)
  (define g->weight (for/list ([c (combinations (combinations (get-vertices g) 2) (length (get-vertices g)))])
                      (define g^ (undirected-graph c))
                      (cond
                        ((valid-graph g^)
                         (cons g^ (sum (map (λ (e) (edge-weight g (first e) (second e))) c))))
                        (else (cons 0 0)))))
  (first (sort g->weight (λ (a b) (> (cdr a) (cdr b))))))

(define max-arrangement (brute-force g))
(printf "Part one: ~a\n" (cdr max-arrangement))

;; Part Two
(printf "Part two: ~a\n" (- (cdr max-arrangement)
                            (apply min (map (λ (e) (edge-weight g (first e) (second e))) (get-edges (car max-arrangement))))))
