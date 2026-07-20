#lang racket

(require "utils.rkt")

(day 13)
(testing #f)

;; Part One
(define (count-bits n)
  (cond
    ((zero? n) 0)
    ((even? n) (count-bits (quotient n 2)))
    (else (add1 (count-bits (quotient (sub1 n) 2))))))

(define (wall? x y fav-num)
  (odd? (count-bits (+ (* x x) (* 3 x) (* 2 x y) y (* y y) fav-num))))

(define (next-valid x y fav-num)
  (define possible (list (point (sub1 x) y) (point x (sub1 y))
                         (point (add1 x) y) (point x (add1 y))))
  (filter (λ (p) (and (>= (point-x p) 0) (>= (point-y p) 0)
                      (not (wall? (point-x p) (point-y p) fav-num))))
          possible))

(define (solve-maze (fav-num (string->number (first lines))) (end-x 31) (end-y 39))
  (struct fringe-node (p step-ct) #:transparent)
  (define visited (mutable-set))
  (define (bfs fringe)
    (match fringe
      ('() -1)
      ((cons (fringe-node (point x y) step-ct) _) #:when (and (equal? x end-x)
                                                              (equal? y end-y))
                                                  step-ct)
      ((cons (fringe-node p _) fringe) #:when (set-member? visited p) (bfs fringe))
      ((cons (fringe-node p step-ct) fringe)
       (set-add! visited p)
       (bfs (append fringe (for/list ([v (next-valid (point-x p) (point-y p) fav-num)])
                             (fringe-node v (add1 step-ct))))))))
  (bfs (list (fringe-node (point 1 1) 0))))

(printf "Part one: ~a\n" (solve-maze))

;; Part Two
(define (maze-distinct (fav-num (string->number (first lines))) (max-dist 50))
  (struct fringe-node (p step-ct) #:transparent)
  (define visited (mutable-set))
  (define (bfs fringe)
    (match fringe
      ('() 0)
      ((cons (fringe-node _ step-ct) fringe) #:when (> step-ct max-dist) (bfs fringe))
      ((cons (fringe-node p _) fringe) #:when (set-member? visited p) (bfs fringe))
      ((cons (fringe-node p step-ct) fringe)
       (set-add! visited p)
       (add1 (bfs (append fringe (for/list ([v (next-valid (point-x p) (point-y p) fav-num)])
                             (fringe-node v (add1 step-ct)))))))))
  (bfs (list (fringe-node (point 1 1) 0))))


(printf "Part two: ~a\n" (maze-distinct))
